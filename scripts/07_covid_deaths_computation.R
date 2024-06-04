#### script to compute covid-19 mortality rate and perform adjustments ###

library(tidyverse)

# clean pop data
pop_df <- read_csv("data/census_pop_all_ages.csv", show_col_types = F) %>% 
  select(age_gp:july1_2021)

# clean covid deaths data
deaths_df <- read_csv("data/covid19_deaths_all_ages.csv", show_col_types = F) %>% 
  select(region:last_col())

# create age groupings to match those of standard pop projection
pop_df_agg <- pop_df %>% filter(age_gp!="Total") %>% 
  # remove + from 85+ so we can convert to numeric and group ages
  mutate(age_gp = str_remove(age_gp, "\\+")) %>% 
  rename(age = age_gp) %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(age_gp = case_when(age <= 0 ~ "<1",
                            age > 0 & age <= 4 ~ "1-4",
                            age > 4 & age <= 14 ~ "5-14",
                            age > 14 & age <= 24 ~ "15-24",
                            age > 24 & age <= 34 ~ "25-34",
                            age > 34 & age <= 44 ~ "35-44",
                            age > 44 & age <= 54 ~ "45-54",
                            age > 54 & age <= 64 ~ "55-64",
                            age > 64 & age <= 74 ~ "65-74",
                            age > 74 & age <= 84 ~ "75-84",
                            age >= 85 ~ "85+")) %>% 
  select(-age) %>% 
  mutate(age_gp = factor(age_gp, level=c("<1", "1-4","5-14","15-24","25-34","35-44","45-54","55-64", "65-74", "75-84", "85+"), ordered = T)) %>% 
  arrange(region, age_gp) %>% 
  select(region, sex, age_gp, july1_2020, july1_2021) %>% 
  group_by(sex, region, age_gp) %>% 
  summarise(across(july1_2020:july1_2021, sum)) %>% 
  ungroup()


# compute annual population change, divide by 12 so we can add to each month for linear interpolation
pop_df_agg <- pop_df_agg %>% mutate(pop_diff = july1_2021-july1_2020,
                                    pop_diff_12 = pop_diff/12)

# perform linear interpolation
# adding an additional 12th of the annual pop difference to the previous month's estimate
# pivot longer for plotting purposes, convert date column from string to date class
pop_df_agg <- pop_df_agg %>%
  mutate(aug1_2020 = july1_2020 + pop_diff_12,
         sept1_2020 = aug1_2020 + pop_diff_12,
         oct1_2020 = sept1_2020 + pop_diff_12,
         nov1_2020 = oct1_2020 + pop_diff_12,
         dec1_2020 = nov1_2020 + pop_diff_12,
         jan1_2021 = dec1_2020 + pop_diff_12,
         feb1_2021 = jan1_2021 + pop_diff_12,
         march1_2021 = feb1_2021 + pop_diff_12,
         april1_2021 = march1_2021 + pop_diff_12,
         may1_2021 = april1_2021 + pop_diff_12,
         june1_2021 = may1_2021 + pop_diff_12) %>% 
  select(age_gp, sex, region, july1_2020, aug1_2020:june1_2021, july1_2021) %>% 
  pivot_longer(july1_2020:july1_2021, names_to = "date", values_to = "pop_est") %>% 
  mutate(date = lubridate::mdy(date))

# recode under 1 year to <1 to match pop data
deaths_df <- deaths_df %>% mutate(age_gp = ifelse(age_gp=="Under 1 year", "<1", age_gp))

# merge population and deaths data
covid_df <- left_join(pop_df_agg, deaths_df, by=c("age_gp", "date", "region", "sex"))

# display first few rows
head(covid_df)

monthly_travel <- read_csv("data/clean-fb-data/rates_age_monthly.csv", show_col_types = F)

travel_df <- monthly_travel %>% 
  filter(!(region %in% c("Alberta", "British Columbia", "Ontario", "Quebec", "Prince Edward Island", "Nunavut",
                         "Northwest Territories", "Newfoundland and Labrador", "Yukon", "Saskatchewan",
                         "Manitoba","New Brunswick", "Nova Scotia", "Canada"))) %>% 
  select(age_gp, sex, region, date, monthly_trav, monthly_pop, loess_rate) %>% 
  mutate(sex = ifelse(sex=="F", "female", "male"),
         age_gp = case_when(age_gp == "13-29" ~ "18-29",
                            age_gp == "30-50" ~ "30-49",
                            age_gp == "50-65" ~ "50-64")) %>% 
  filter(date!="2020-06-01")

# filter deaths data for correct age groups,
# remove aggregated sex level data, and
# compute total deaths variable to use in our death rate calculations
deaths_df_new <- deaths_df %>%
  filter(age_gp %in% c("18-29", "30-39", "40-49", "50-64")) %>%
  select(region, age_gp, sex, date, covid_deaths) |>
  mutate(covid_deaths = ifelse(is.na(covid_deaths), 1, covid_deaths)) |>
  rename(deaths = covid_deaths)

# combine 30-39 and 40-49 to match fb data
deaths_df_new <- deaths_df_new %>% 
  mutate(age_gp = ifelse(age_gp=="30-39" | age_gp=="40-49", "30-49", age_gp)) %>% 
  group_by(region, age_gp, sex, date) %>% 
  summarise(deaths = sum(deaths)) %>% 
  ungroup()

# create age groups and aggregate population estimates
pop_df_new <- pop_df %>% 
  filter(!str_detect(age_gp, "\\+|[:alpha:]")) %>% 
  mutate(age_gp = as.numeric(age_gp)) %>% 
  rename(age = age_gp) %>% 
  mutate(age_gp = case_when(age > 17 & age <= 29 ~ "18-29",
                            age > 29 & age <= 49 ~ "30-49",
                            age > 49 & age <= 64 ~ "50-64")) %>% 
  filter(!is.na(age_gp)) %>% 
  select(-age) %>% 
  group_by(sex, region, age_gp) %>% 
  summarise(across(july1_2020:july1_2021, sum)) %>% 
  ungroup()

pop_df_new <- pop_df_new %>% 
  mutate(pop_diff = july1_2021-july1_2020,
         pop_diff_12 = pop_diff/12) %>% 
  mutate(aug1_2020 = july1_2020 + pop_diff_12,
         sept1_2020 = aug1_2020 + pop_diff_12,
         oct1_2020 = sept1_2020 + pop_diff_12,
         nov1_2020 = oct1_2020 + pop_diff_12,
         dec1_2020 = nov1_2020 + pop_diff_12,
         jan1_2021 = dec1_2020 + pop_diff_12,
         feb1_2021 = jan1_2021 + pop_diff_12,
         march1_2021 = feb1_2021 + pop_diff_12,
         april1_2021 = march1_2021 + pop_diff_12,
         may1_2021 = april1_2021 + pop_diff_12,
         june1_2021 = may1_2021 + pop_diff_12) %>% 
  select(age_gp, sex, region, july1_2020, aug1_2020:june1_2021, july1_2021) %>% 
  pivot_longer(july1_2020:july1_2021, names_to = "date", values_to = "pop_est") %>% 
  mutate(date = lubridate::mdy(date))

# merge deaths and population estimates data
covid_df_new <- left_join(pop_df_new, deaths_df_new, by=c("region", "age_gp", "sex", "date"))

# merge fb travel data with covid death/pop data, select variables of interest
fb_analysis_df <- left_join(covid_df_new, travel_df, by=c("region", "age_gp", "sex", "date")) 

## out migration: proportional
tot_trav <- fb_analysis_df |> 
  group_by(age_gp, sex, date) |> 
  summarize(total_travellers = sum(monthly_trav))

tot_pop <- fb_analysis_df |> 
  group_by(age_gp, sex, date) |> 
  summarize(total_pop = sum(monthly_pop))

fb_analysis_df |> 
  summarize(unique(region))

fb_analysis_df <- fb_analysis_df |> 
  left_join(tot_trav) |> 
  left_join(tot_pop) |> 
  #mutate(out_trav = total_travellers/51) |> 
  #mutate(out_trav = total_travellers*monthly_pop/total_pop) |> 
  mutate(out_trav = total_travellers*monthly_pop/(total_pop-monthly_pop)) |> 
  mutate(out_trav_rate = out_trav/monthly_pop) |> 
  mutate(net_rate = loess_rate - out_trav_rate)

# compute age specific mortality rates with and without traveller adjustment
fb_age_rates <- fb_analysis_df %>% 
  mutate(pop_est_adj = (pop_est*(1+net_rate)),
         death_rate_adj = 100000*(deaths/pop_est_adj),
         death_rate = 100000*(deaths/pop_est)) %>% 
  # death rate adj is NA for sex=total b/c travel rate is not aggregated by sex
  filter(!is.na(death_rate_adj))

fb_age_rates %>% 
  group_by(age_gp) |> 
  mutate(relative_diff = 100*(death_rate_adj-death_rate)/death_rate,
         abs_diff = death_rate_adj-death_rate) %>% 
  summarise(avg_relative_diff = mean(relative_diff, na.rm = T),
            avg_abs_diff = mean(abs_diff, na.rm = T)) %>% 
  mutate(across(avg_relative_diff:avg_abs_diff, ~ round(., 4)))

fb_age_rates %>% 
  #filter(age_gp=="50-64") |> 
  mutate(relative_diff = 100*(death_rate_adj-death_rate)/death_rate,
         abs_diff = death_rate_adj-death_rate) %>% 
  group_by(region) %>% 
  summarise(avg_relative_diff = mean(relative_diff, na.rm = T),
            avg_abs_diff = mean(abs_diff, na.rm = T)) %>% 
  mutate(across(avg_relative_diff:avg_abs_diff, ~ round(., 4))) |> 
  arrange(avg_abs_diff) 

fb_age_rates |> 
  filter(age_gp=="50-64", region=="Florida"|region=="California"|region=="New York"|region=="Nevada"|region=="New Mexico"|region=="Illinois") |> 
  #filter(age_gp=="50-64", region == "Florida"|region == "California"|region=="Louisiana"|region=="Washington D. C.") |> 
  filter(age_gp=="50-64") |> 
  select(region, age_gp, sex, date, death_rate, death_rate_adj) |> 
  mutate(diff = (death_rate_adj - death_rate),
         sex = factor(sex, levels = c("female", "male"), labels = c("Female", "Male"))) |> 
  ggplot(aes(date, diff, col = sex)) + 
  geom_point()+
  geom_line()+
  facet_wrap(~region)+
  labs(y = "Mortality Rate Difference (deaths per 100,000)", x = "", color="Sex")+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))+
  geom_hline(yintercept = 0)

ggsave("plots/rate_diff.pdf", width = 8, height = 5)

# compute figures for paper
fb_age_rates |> 
  select(age_gp:date, death_rate_adj, death_rate) |> 
  mutate(diff = death_rate_adj - death_rate) |> 
  summarise(min=min(diff), max=max(diff), avg=mean(abs(diff)))

fb_age_rates |> group_by(age_gp) |> 
  select(age_gp:date, death_rate_adj, death_rate) |> 
  mutate(diff = death_rate_adj - death_rate) |> 
  summarise(min=min(diff), max=max(diff), avg=mean(abs(diff)))

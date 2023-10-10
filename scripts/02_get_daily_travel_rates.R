##### script to compute DAILY travel rates, extract smooth estimates, and produce time series plots #####

library(tidyverse)

### READ IN CLEAN DATA ###
travel_df_age <- read_csv("data/clean-fb-data/travel_data_clean_age.csv", show_col_types = F)
pop_df_age <- read_csv("data/clean-fb-data/pop_data_clean_age.csv", show_col_types = F)
travel_df_no_age <- read_csv("data/clean-fb-data/travel_data_clean_no_age.csv", show_col_types = F)
pop_df_no_age <- read_csv("data/clean-fb-data/pop_data_clean_no_age.csv", show_col_types = F)

### CALCULATING RATES ###

## NOTE - we want to take the set of dates where traveler data exist to be one we calculate rates for
## to begin, let's check where we are missing population data for those dates when we have traveler data
## this will be after or on 2020-05-29 for no age data b/c that's when pop data starts being collected
## then, if the population data don't exist for that date, use the data from the previous available day
## if there's two closest dates, use the previous day

# check for dates present in travel_df_no_age but not in pop_df_no_age
# not an issue (filter is for when population data collection begins)
nrow(setdiff(distinct(travel_df_no_age, date) %>% 
               filter(date>='2020-05-29'), 
             distinct(pop_df_no_age, date)))

# check for dates present in travel_df_age but not in pop_df_age
# we are missing 28 dates of denominators (i.e. population data)
nrow(setdiff(distinct(travel_df_age, date), 
             distinct(pop_df_age, date)))

## lets add rows to the population dataset with age for each of these dates, 
## then fill using the closest available date

# extract the missing dates as a vector
missing_dates <- setdiff(distinct(travel_df_age, date), distinct(pop_df_age, date)) %>% pull()

# for each date, we want to add a row for each region, age, sex group
# this gives us 390 rows per date: 3 age groups * 2 sexes * 65 distinct regions
# loop through each missing date and add those rows
for (i in 1:length(missing_dates)){
  pop_df_age <- pop_df_age %>% select(sex, region, age_gp) %>% 
    unique() %>% 
    mutate(date = missing_dates[i]) %>% 
     bind_rows(., pop_df_age)
}

# group by region, sex and age group and arrange by date so we can fill our denominators down correctly
pop_df_age <- pop_df_age %>% group_by(region, sex, age_gp) %>% 
  arrange(date) %>% 
  fill(c(daily, monthly), .direction = "down") %>% 
  ungroup()

# now, merge the population and travel data with age
d_merged_age <-  left_join(travel_df_age, pop_df_age, by = c("sex", "region", "age_gp", "date")) %>% 
  dplyr::rename(daily_trav = daily.x,
         monthly_trav = monthly.x,
         daily_pop = daily.y,
         monthly_pop = monthly.y) %>% 
  select(date, sex, region, age_gp, daily_trav, monthly_trav, daily_pop, monthly_pop)

# now, we can merge the population and travel data without age
# filter for anything on or after 29 may 2020 b/c that's when population data start being collected
d_merged_no_age <-  left_join(travel_df_no_age, pop_df_no_age, by = c("sex", "region", "date")) %>% 
  dplyr::rename(daily_trav = daily.x,
         monthly_trav = monthly.x,
         daily_pop = daily.y,
         monthly_pop = monthly.y) %>% 
  filter(date>='2020-05-29') %>% 
  select(date, sex, region, daily_trav, monthly_trav, daily_pop, monthly_pop)

# filter out rows where daily pop is 0, or daily travelers is 0 and the region is one of the Canadian territories
# this is based on smaller regions where we can realistically expect 0 travelers
df_age <- d_merged_age %>% 
  mutate(daily_pop = ifelse(daily_pop==0, NA, daily_pop)) %>% 
  filter(daily_trav==0 & region %in% c("Yukon", "Nunavut", "Northwest Territories") | daily_trav!=0) %>% 
  filter(is.na(daily_pop) | daily_trav <= daily_pop)

df_no_age <- d_merged_no_age %>% 
  mutate(daily_pop = ifelse(daily_pop==0, NA, daily_pop)) %>% 
  filter(daily_trav==0 & region %in% c("Yukon", "Nunavut", "Northwest Territories") | daily_trav!=0) %>% 
  filter(is.na(daily_pop) | daily_trav <= daily_pop)

# fill in missing data with that from closest previous day
df_age <- df_age %>% group_by(region, age_gp, sex) %>% 
  arrange(date) %>% 
  fill(c(daily_pop, monthly_pop, daily_trav, monthly_trav), .direction = "downup") %>%
  ungroup()

df_no_age <- df_no_age %>% group_by(region, sex) %>% 
  arrange(date) %>% 
  fill(c(daily_pop, monthly_pop, daily_trav, monthly_trav), .direction = "down") %>%
  ungroup()

## we want to make sure all regions and sub-pops have data available for 1 July 2020 and 2021
## as we are interested in computing year on year change

# only one case where a sub-pop doesn't have data for July 1 2020 in df_age
# add the row for that demographic group manually then fill down - previous day is June 30
df_age <- df_age %>%
  add_row(region="South Dakota",
          sex = as.factor("M"),
          date = as.Date('2020-07-01'),
          age_gp = "50-65") %>% 
  arrange(date) %>% 
  group_by(region, sex, age_gp) %>% 
  fill(c(daily_trav, monthly_trav, daily_pop, monthly_pop), .direction = "down") %>% 
  ungroup()

# two cases of this df_no_age, same idea as above
df_no_age <- df_no_age %>%
  add_row(region="British Columbia",
          sex = as.factor("F"),
          date = as.Date('2020-07-01')) %>%
  add_row(region="Nevada",
          sex = as.factor("F"),
          date = as.Date('2020-07-01')) %>% 
  arrange(date) %>% 
  group_by(region, sex) %>% 
  fill(c(daily_trav, monthly_trav, daily_pop, monthly_pop), .direction = "down") %>% 
  ungroup()

# compute daily and monthly rate of travel
df_age <- df_age %>% 
  mutate(daily_rate = daily_trav/daily_pop,
         monthly_rate = monthly_trav/monthly_pop)

df_no_age <- df_no_age %>% 
  mutate(daily_rate = daily_trav/daily_pop,
         monthly_rate = monthly_trav/monthly_pop)

######## PLOTS - TRAVEL RATES ########
## pdf of time series plots for travel rates by sex

# get names of regions without age breakdown to loop through
regions_no_age <- df_no_age %>%
  select(region) %>% 
  distinct() %>% 
  pull()

# start pdf
pdf("plots/fb_travel_rates_no_age.pdf", width = 8, height = 6)

# loop through regions
for(i in 1:length(regions_no_age)){
  p <- df_no_age %>% 
    filter(region==regions_no_age[i]) %>% 
    ggplot(aes(x=date, y=daily_rate, fill=sex, color=sex))+
    geom_smooth()+
    geom_point()+
    theme_bw()+
    scale_x_date(date_labels = "%b %Y")+
    facet_wrap(.~sex)+
    labs(x="Time", y = "Daily rate of travellers", 
         title="Traveller rate over time, by sex",
         subtitle = paste0(regions_no_age[i]))
  print(p)
}

# shut down pdf
dev.off()

## pdf of time series plots for travel rates by age and sex

# get names of regions with age breakdown to loop through
regions_age <- df_age %>%
  select(region) %>% 
  distinct() %>% 
  pull()

# start pdf
pdf("plots/fb_travel_rates_age.pdf", width = 8, height = 6)

# loop through regions
for(i in 1:length(regions_age)){
  p <- df_age %>% 
    filter(region == regions_age[i]) %>% 
    arrange(date) %>% 
    ggplot(aes(x=date, y=daily_rate, color=age_gp))+
    geom_point()+
    geom_smooth(aes(fill=age_gp))+
    theme_bw()+
    facet_wrap(.~as.factor(sex))+
    scale_x_date(date_labels = "%b %Y")+
    labs(title = "Traveller rate over time, by age group and sex",
         subtitle = paste0(regions_age[i]),
         y = "Daily rate of travellers", 
         x = "Time", color="Age Group", fill="Age Group")
  print(p)
}

# shut down pdf
dev.off()

### EXTRACT SMOOTH LOESS ESTIMATES ###

# first split the dataset into the groups we want, then fit the model
# need to correct class of date variable for model
models_age <- df_age %>% mutate(date=as.numeric(date)) %>% 
  split(f=list(.$region, .$sex, .$age_gp)) %>% 
  map(~stats::loess(daily_rate ~ date, span=0.5, data=.x))

models_no_age <- df_no_age %>% mutate(date=as.numeric(date)) %>%
  split(f=list(.$region, .$sex)) %>% 
  map(~stats::loess(daily_rate ~ date, span=0.5, data=.x))

# now, use the predict function to get smooth estimates, for data
predict.models_age <- models_age %>% map( ~ predict(.x, se=T))

predict.models_no_age <- models_no_age %>% map( ~ predict(.x, se=T))

# grab dates for each region/sex/age group, so we can bind those onto the df of the smoothed estimates
dates_age <- df_age %>% split(f=list(.$region, .$sex, .$age_gp)) %>% 
  map( ~ .$date %>% as_tibble() %>% rename(date = value))

dates_no_age <- df_no_age %>% split(f=list(.$region, .$sex)) %>% 
  map( ~ .$date %>% as_tibble() %>% rename(date = value))

# just grab the fit and standard error, so we have a tibble we can bind the dates onto for each group
loess_ests_age <- predict.models_age %>% 
  map( ~ as_tibble(.) %>% select(fit, se.fit))

loess_ests_no_age <- predict.models_no_age %>% 
  map( ~ as_tibble(.) %>% select(fit, se.fit))

# bind columns of two lists together, so for each group we have a tibble with the smoothed estimate, SE, and associated date
loess_ests_age <- map2(loess_ests_age, dates_age, ~ .x %>% bind_cols(.y))

loess_ests_no_age <- map2(loess_ests_no_age, dates_no_age, ~ .x %>% bind_cols(.y))

# combine everything into one massive tibble by binding rows
# note: replace the separator from a . to a * b/c some states have full stops in their name (i.e. Washington D.C.)
# filter out Canada b/c we're not using that data
loess_ests_age  <- loess_ests_age %>% bind_rows(., .id="column_label") %>% 
  mutate(column_label = str_replace(column_label, "\\.(?=M|F\\.)", "*"),
         column_label = str_replace(column_label, "(?<=\\*M|\\*F)\\.", "*")) %>% 
  separate(column_label, c("region", "sex", "age_gp"), sep="\\*") %>% 
  filter(region!="Canada")

loess_ests_no_age  <- loess_ests_no_age %>% bind_rows(., .id="column_label") %>% 
  mutate(column_label = str_replace(column_label, "\\.(?=M|F)", "*")) %>% 
  separate(column_label, c("region", "sex"), sep="\\*") %>% 
  filter(region!="Canada")

# bind observed rates onto dataframe with loess estimates so we have everything in one place
df_age_all <- df_age %>% 
  filter(region!="Canada") %>% 
  left_join(., loess_ests_age, by = c("sex", "age_gp", "region", "date")) %>% 
  rename(loess_rate = fit,
         loess_se = se.fit,
         obs_rate = daily_rate) %>% 
  select(-monthly_pop, -monthly_rate, -monthly_trav)

df_no_age_all <- df_no_age %>% 
  filter(region!="Canada") %>% 
  left_join(., loess_ests_no_age, by = c("sex", "region", "date")) %>% 
  rename(loess_rate = fit,
         loess_se = se.fit,
         obs_rate = daily_rate) %>% 
  select(-monthly_pop, -monthly_rate, -monthly_trav)

### EXPORT CLEANED DAILY TRAVEL RATES DATA ###
write.csv(df_age_all, "data/clean-fb-data/rates_age.csv", row.names = F)
write.csv(df_no_age_all, "data/clean-fb-data/rates_no_age.csv", row.names = F)
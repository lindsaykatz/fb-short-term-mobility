##### script to compute MONTHLY travel rates, extract smooth estimates, and produce time series plots #####

## NOTE - exact same approach as in script 02, just focusing on monthly data here
# also, we are only considering the data with age-disaggregation here because it is for
# for our Covid Mortality rate analysis, and this is the only data we will use for it

### READ IN CLEAN DATA ###
travel_df_age <- read_csv("data/clean-fb-data/travel_data_clean_age.csv", show_col_types = F)
pop_df_age <- read_csv("data/clean-fb-data/pop_data_clean_age.csv", show_col_types = F)

### CALCULATING RATES ###
## we want to take set of dates where traveler data exist to be one we calculate rates for
## if the population data don't exist for that date, then use the data from the previous available day
## if there's two closest dates, use the previous day

# check for dates present in travel_df_age but not in pop_df_age
# we are missing 28 dates of denominators (i.e. population data)
nrow(setdiff(distinct(travel_df_age, date), 
             distinct(pop_df_age, date)))

# lets add rows to the population dataset for each of these dates, then fill using the closest available date
# extract the missing dates as a vector
missing_dates <- setdiff(distinct(travel_df_age, date), 
                         distinct(pop_df_age, date)) %>% pull()

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

# nmerge the population and travel data - age (again, just focusing on monthly data here)
d_merged_age <-  left_join(travel_df_age, pop_df_age, by = c("sex", "region", "age_gp", "date")) %>%
  rename(monthly_trav = monthly.x,
         monthly_pop = monthly.y) %>%
  select(date, sex, region, age_gp, monthly_trav, monthly_pop)

# check that there are no cases where monthly trav<1000 which we might have to filter out
stopifnot(nrow(d_merged_age %>% filter(monthly_trav < 1000)) == 0)

# let's filter out rows where monthly pop is less than monthly travellers
df_age <- d_merged_age %>% filter(monthly_trav <= monthly_pop)

# want to take the observations only from the midpoint of each month, for each region
# we take the 15th as the midpoint, and if that isn't available, we take the 16th
df_age_month <- df_age %>% mutate(month_yr = str_extract(date, "^\\d{4}-\\d{2}")) %>% 
  filter(str_detect(date, "-15$|-16$")) %>% 
  group_by(month_yr) %>% 
  filter(date == min(date)) %>% 
  ungroup()

# sanity check that each month midpoint date has the right number of observations 
# 65 regions * 2 genders * 3 age gps = 390
df_age_month %>% group_by(date) %>% summarise(n=n())

# March 2021 missing one obs, checked that one and it's from Alberta
# not a concern b/c our Covid mortality analysis only looks at American data
df_age_month %>% group_by(date, region) %>% mutate(n=n()) %>% filter(n!=6)

# compute monthly rate of travel, and rename original date as such
# add first of month to date which we will use for regression - this is arbitrary
df_age_month <- df_age_month %>% mutate(monthly_rate = monthly_trav/monthly_pop,
                                        orig_date = date,
                                        date = paste0(month_yr, "-01"),
                                        date = lubridate::ymd(date))

### EXTRACT MONTHLY SMOOTH LOESS ESTIMATES ###

# first split the dataset into the groups we want, then fit the model
# need to correct class of date variable for model
models_age <- df_age_month %>% mutate(date=as.numeric(date)) %>% 
  split(f=list(.$region, .$sex, .$age_gp)) %>% 
  map(~stats::loess(monthly_rate ~ date, span=0.5, data=.x))

# now, use the predict function to get smooth estimates, for data with age
predict.models_age <- models_age %>% map( ~ predict(.x, se=T))

# grab dates for each region/sex/age group, so we can bind those onto the df of the smoothed estimates
dates_age <- df_age_month %>% split(f=list(.$region, .$sex, .$age_gp)) %>% 
  map( ~ .$date %>% as_tibble() %>% rename(date = value))

# just grab the fit and standard error, so we have a tibble we can bind the dates onto for each group
loess_ests_age <- predict.models_age %>% 
  map( ~ as_tibble(.) %>% select(fit, se.fit))

# bind columns of two lists together, so for each group we have a tibble with the smoothed estimate, SE, and associated date
loess_ests_age <- map2(loess_ests_age, dates_age, ~ .x %>% bind_cols(.y))

# combine everything into one massive tibble by binding rows - loess estimates with age
# note: replace the separator from a . to a * b/c some states have full stops in their name (i.e. Washington D.C.)
# (filter out Canada b/c we're not using that data)
loess_ests_age  <- loess_ests_age %>% bind_rows(., .id="column_label") %>% 
  mutate(column_label = str_replace(column_label, "\\.(?=M|F\\.)", "*"),
         column_label = str_replace(column_label, "(?<=\\*M|\\*F)\\.", "*")) %>% 
  separate(column_label, c("region", "sex", "age_gp"), sep="\\*") %>% 
  filter(region!="Canada")

# bind observed rates onto dataframe with loess estimates so we have everything in one place
df_age_all <- df_age_month %>% 
  filter(region!="Canada") %>% 
  left_join(., loess_ests_age, by = c("sex", "age_gp", "region", "date")) %>% 
  rename(loess_rate = fit,
         loess_se = se.fit,
         obs_rate = monthly_rate)

### EXPORT CLEANED MONTHLY TRAVEL RATES DATA ###
write_csv(df_age_all, "data/clean-fb-data/rates_age_monthly.csv")

# compute RMSE to compare our smoothed rates to observed
rmse_age <- df_age_all %>% 
  mutate(sq_diff = (loess_rate-obs_rate)^2) %>% 
  group_by(region, sex, age_gp) %>% 
  summarise(rmse=sqrt(mean(sq_diff)))

# visualizing monthly rates to see how they look, trends look good
df_age_all %>% filter(region=="Florida") %>% 
  ggplot(aes(x=date, y=loess_rate, color=age_gp))+
  geom_point() +
  geom_smooth()+
  facet_wrap(.~sex)

# read in daily age data for RMSE comparison
daily_rates <- read_csv("data/clean-fb-data/rates_age.csv")

# compute daily rmses to compare, the monthly RMSE values are smaller overall, one exception
daily_rates %>% mutate(sq_diff = (loess_rate-obs_rate)^2) %>% 
  group_by(region, sex, age_gp) %>% 
  summarise(rmse=sqrt(mean(sq_diff))) %>% 
  ungroup() %>% 
  arrange(desc(rmse)) %>% 
  #filter(region=="Wyoming") %>% 
  rename(rmse_daily = rmse) %>% 
  left_join(., rmse_age %>% ungroup() %>% 
              #filter(region=="Wyoming") %>% 
              rename(rmse_monthly = rmse),
            by = c("region", "sex", "age_gp")) %>% 
  filter(rmse_monthly >= rmse_daily & rmse_daily!=0)

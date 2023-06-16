##### script to clean the original census and cdc data #####

## note - while only the cleaned data exported from this script are available on our repo,
## the raw data we used are available for download:
## Covid deaths data source: https://data.cdc.gov/NCHS/Provisional-COVID-19-Deaths-by-Sex-and-Age/9bhg-hcku
## population data source: https://www.census.gov/data/datasets/time-series/demo/popest/2020s-state-detail.html
## the script is provided to show exactly how we cleaned and processed the data

library(tidyverse)

### clean & prep data for analysis ###

# read in monthly covid deaths data
deaths_df <- read_csv("data/covid-deaths-data/covid19_deaths.csv", show_col_types = F)

# filter and rename columns of interest
deaths_df <- deaths_df %>% filter(State!="United States" & Group=="By Month") %>% 
  select(Year:`Pneumonia, Influenza, or COVID-19 Deaths`, Footnote) %>% 
  rename(year = Year,
         month = Month,
         region = State,
         sex = Sex,
         age_gp = `Age Group`,
         covid_deaths = `COVID-19 Deaths`,
         pneumonia_deaths = `Pneumonia Deaths`,
         pneumonia_covid_deaths = `Pneumonia and COVID-19 Deaths`,
         influenza_deaths = `Influenza Deaths`,
         pneumonia_influenza_covid_deaths = `Pneumonia, Influenza, or COVID-19 Deaths`,
         total_deaths = `Total Deaths`) %>% 
  select(year:age_gp, total_deaths, everything())

# tidy sex, age group, region and date columns
# unite year and month columns and convert to date class
# filter out regions not included in our analysis: Puerto Rico (part of deaths data not in pop data)
deaths_df_new <- deaths_df %>% 
  mutate(sex = str_to_lower(sex),
         age_gp = str_remove(age_gp, " years"),
         age_gp = ifelse(age_gp=="85 and over", "85+", age_gp),
         age_gp = ifelse(age_gp=="All Ages", "Total", age_gp),
         region = ifelse(region=="District of Columbia", "Washington D. C.", region),
         sex = ifelse(sex=="all sexes", "total", sex)) %>% 
  unite("date", c(month, year), sep = "/01/") %>% 
  mutate(date = lubridate::mdy(date)) %>% 
  filter(date >= "2020-07-01" & date <= "2021-07-01") %>% 
  filter(region!= "Puerto Rico")
  
# aggregate NYC w/ NY
deaths_df_new <- deaths_df_new %>%
  mutate(region = ifelse(region=="New York City", "New York", region)) %>% 
  group_by(region, age_gp, sex, date) %>% 
  summarise(across(total_deaths:pneumonia_influenza_covid_deaths, ~sum(., na.rm=T))) %>% 
  ungroup()

# read in census population data
# list files to loop over
pop_files <- list.files("data/census-data")

# remove any csv files (ie old cleaned data)
pop_files <- pop_files %>% as_tibble() %>% filter(str_detect(value, "\\.xlsx")) %>% pull()

### first for loop here is to include all ages in the resulting CSV, while the next one is designed to produce an aggregated CSV
# define empty tibble to store data in
pop_df <- tibble()

for (i in 1:length(pop_files)){
  # read in file
  thisState <- readxl::read_xlsx(paste0("data/census-data/", pop_files[i]), skip=2)
  
  # clean, and filter for total aggregate value
  pop_df <- thisState %>% rename(age = .Age,
                                 april1_2020_est_total = `April 1, 2020 Estimates Base`,
                                 april1_2020_est_male = `...3`,
                                 april1_2020_est_female = `...4`,
                                 july1_2020_est_total = `Population Estimate (as of July 1)`,
                                 july1_2020_est_male = `...6`,
                                 july1_2020_est_female = `...7`,
                                 july1_2021_est_total = `...8`,
                                 july1_2021_est_male = `...9`,
                                 july1_2021_est_female = `...10`) %>% 
    filter(age=="Total" | str_detect(age, "\\.\\d{1,2}")) %>% 
    rename(age_gp = age) %>% 
    mutate(age_gp = str_remove(age_gp, "\\.")) %>% 
    mutate(across(april1_2020_est_total:july1_2021_est_female, ~ as.numeric(.))) %>% 
    pivot_longer(april1_2020_est_total:july1_2021_est_female, names_to = "type", values_to = "estimate") %>% 
    separate(type, c("date", "sex"), sep = "_est_") %>% 
    #filter(sex!="total") %>% 
    filter(str_detect(date, "july")) %>% 
    mutate(state = str_extract(pop_files[i], "(?<=-).+(?=\\.xlsx)")) %>% 
    pivot_wider(names_from = date, values_from = estimate) %>% 
    bind_rows(pop_df, .)
}

# fix up state variable, recode DC to match that in deaths and fb dataset
pop_df <- pop_df %>% mutate(state = str_replace_all(state, "_", " "),
                            state = str_to_title(state),
                            state = ifelse(state=="Dc", "Washington D. C.", state)) %>% 
  rename(region = state)

### export clean data ###
write.csv(pop_df, "data/census-data/census_pop_all_ages.csv")
write.csv(deaths_df_new, "data/covid-deaths-data/covid19_deaths_all_ages.csv")

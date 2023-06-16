##### script to clean Facebook marketing API data #####

## NOTE - the scraped data files are not publicly available, so this cannot be re-run
## the script is provided to show exactly how we cleaned and processed the data

# read in necessary packages
library(tidyverse)
library(lubridate)

### DATA IMPORT ###
# get vector of file names
all_files <- list.files("~/Documents/fb-migration-canada/data/recent_travel_raw/")

# subset travel files
travel_files <- all_files %>% str_subset(pattern="travellers")

# subset population files
pop_files <- all_files %>% str_subset(pattern="pop.")

# define empty tibbles to store data
d_all_travel <- tibble()
d_all_pop <- tibble()

# for loop to read in all traveler files and grab time/date, then bind into one data frame
for(i in 1:length(travel_files)){
  this_file <- read_csv(paste0("~/Documents/fb-migration-canada/data/recent_travel_raw/", travel_files[i]), show_col_types = FALSE)
  
  time_info <- str_remove(travel_files[i], "[:alpha:]{0,4}_travellers.*.csv")
  date <- str_split(time_info, "_")[[1]][1]
  time_of_day <- str_split(time_info, "_")[[1]][2]
  time_info_all <- strptime(paste(date, time_of_day, sep=" "), format = "%Y-%m-%d %H:%M:%OS", tz = "GMT")
  this_file <- this_file %>% mutate(date = date, time_of_day = time_of_day, time_info = time_info_all)
  
  d_all_travel <- bind_rows(d_all_travel, this_file)
}

# re-code sex and date
d_all_travel <- d_all_travel %>% 
  mutate(date = as.Date(date),
         sex = as.factor(ifelse(sex==1, "M", "F"))) %>% 
  unite(age_gp, c("age1", "age2"), sep="-")

# for loop to read in all population files and grab time/date, then bind into one data frame
for(i in 1:length(pop_files)){
  this_file <- read_csv(paste0("~/Documents/fb-migration-canada/data/recent_travel_raw/", pop_files[i]), show_col_types = FALSE)
  
  time_info <- str_remove(pop_files[i], "_pop.*.csv")
  date <- str_split(time_info, "_")[[1]][1]
  time_of_day <- str_split(time_info, "_")[[1]][2]
  time_info_all <- strptime(paste(date, time_of_day, sep=" "), format = "%Y-%m-%d %H:%M:%OS", tz = "GMT")
  this_file <- this_file %>% mutate(date = date, time_of_day = time_of_day, time_info = time_info_all)
  
  d_all_pop <- bind_rows(d_all_pop, this_file)
}

# re-code sex and date
d_all_pop <- d_all_pop %>%
  mutate(date = as.Date(date),
         sex = as.factor(ifelse(sex==1, "M", "F"))) %>% 
  unite(age_gp, c("age1", "age2"), sep="-")

### DATA CLEANING - TRAVELER DATA  ###
# store raw data frame in case we want it later
d_all_travel_orig <- d_all_travel

# remove duplicates
d_all_travel <- d_all_travel %>% distinct()

# deal w/ days that have multiple unique observations
# when time-stamp is available: take latest time-stamp for that day
# when no time-stamp is available: take highest count
d_all_travel <- d_all_travel %>% group_by(sex, region, date, age_gp) %>% 
  filter((is.na(time_of_day)==TRUE & daily==max(daily)) | 
           (is.na(time_of_day)==FALSE & time_info==max(time_info))) %>% 
  filter((is.na(time_of_day)==TRUE & monthly==max(monthly)) | 
           (is.na(time_of_day)==FALSE)) %>% 
  ungroup()

# deal w/ Canada data (that has a lot of observations on each day b/c was accidentally collected twice daily)
d_all_travel <- d_all_travel %>% group_by(sex, region, date, age_gp) %>% 
  filter((region!="Canada") | (region=="Canada" & daily==max(daily))) %>% 
  ungroup() 
  
# sanity check that there are no more days with multiple observations
stopifnot(d_all_travel %>% 
            group_by(region, date, age_gp, sex) %>% 
            summarise(n=n()) %>% 
            filter(n>1) %>% 
            nrow() == 0)

# now compute the aggregated total across all age groups
# get the total for the age disagg. data and the non-age-disagg. data, and take the max
# let's only use maximum of those double counted
# for plots later on
d_all_travel_tot <- d_all_travel %>% 
  mutate(flag = case_when(age_gp=="NA-NA" ~ 0,
                          age_gp!="NA-NA" ~ 1)) %>% 
  group_by(flag, sex, region, date) %>% 
  summarise(daily_tot = sum(daily)) %>% 
  ungroup() %>% 
  group_by(sex, region, date) %>% 
  summarise(daily_tot = max(daily_tot)) %>% 
  ungroup()

### DATA CLEANING - POPULATION DATA ###
# store raw data frame in case we want it later
d_all_pop_orig <- d_all_pop

# there are no duplicates to remove (checked this)
stopifnot(d_all_pop %>% distinct() %>% nrow() == nrow(d_all_pop))

# deal w/ days that have multiple unique observations
# when time-stamp is available: take latest time-stamp for that day
# when no time-stamp is available: take highest count
d_all_pop <- d_all_pop %>% group_by(sex, region, date, age_gp) %>% 
  filter((is.na(time_of_day)==TRUE & daily==max(daily)) | 
           (is.na(time_of_day)==FALSE & time_info==max(time_info))) %>% 
  filter((is.na(time_of_day)==TRUE & monthly==max(monthly)) | 
           (is.na(time_of_day)==FALSE)) %>% 
  ungroup()

# deal w/ Canada data (that has a lot of observations on each day b/c was accidentally collected twice daily)
d_all_pop <- d_all_pop %>% group_by(sex, region, date, age_gp) %>% 
  filter((region!="Canada") | (region=="Canada" & daily==max(daily))) %>% 
  ungroup()

# sanity check that there are no more days with multiple observations
stopifnot(d_all_pop %>% group_by(region, date, age_gp, sex) %>% 
            summarise(n=n()) %>% 
            filter(n>1) %>% 
            nrow() == 0)

# let's only use the maximum daily count of those double counted
d_all_pop_tot <- d_all_pop %>% 
  mutate(flag = case_when(age_gp=="NA-NA" ~ 0,
                          age_gp!="NA-NA" ~ 1)) %>% 
  group_by(flag, sex, region, date) %>% 
  summarise(daily_tot = sum(daily)) %>% 
  ungroup() %>% 
  group_by(sex, region, date) %>% 
  summarise(daily_tot = max(daily_tot)) %>% 
  ungroup()

### DATA EXPORT ###
# split data up by age-disaggregated and not age disaggregated
d_all_travel_age <- d_all_travel %>% filter(age_gp!="NA-NA")
d_all_pop_age <- d_all_pop %>% filter(age_gp!="NA-NA")
d_all_travel_no_age <- d_all_travel %>% filter(age_gp=="NA-NA") %>% select(-age_gp)
d_all_pop_no_age <- d_all_pop %>% filter(age_gp=="NA-NA") %>% select(-age_gp)

# export the cleaned datasets
write.csv(d_all_travel_age, "data/clean-fb-data/travel_data_clean_age.csv", row.names = F)
write.csv(d_all_pop_age, "data/clean-fb-data/pop_data_clean_age.csv", row.names = F)
write.csv(d_all_travel_no_age, "data/clean-fb-data/travel_data_clean_no_age.csv", row.names = F)
write.csv(d_all_pop_no_age, "data/clean-fb-data/pop_data_clean_no_age.csv", row.names = F)
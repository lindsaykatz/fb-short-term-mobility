#### script to clean flight volume data, and create plots ###

## note - while only the cleaned data exported from this script are available on our repo,
## the raw data we used are available for download: https://www.transtats.bts.gov/ontime/arrivals.aspx
## the script is provided to show exactly how we cleaned and processed the data

# read in necessary packages
library(tidyverse)
library(here)
library(timetk)

### DATA VALIDATION ###

# get list of file names
flight_data <- list.files("data/raw-flight-data")

# empty tibble
check_flight_data <- tibble()

# loop over all flight data files
# parsing warnings are just bc the CSVs are in a weird format at the top
for (i in 1:length(flight_data)){
  # import just first two lines of csv
  thisFile <- read_csv(here(paste0("data/raw-flight-data/", flight_data[i])), n_max = 2,show_col_types = F)
  
  # extract destination and airline from the name we gave the file, bind with those found in file, bind rows to tibble
  check_flight_data <- thisFile %>% mutate(type = c("file_dest", "file_airline")) %>% 
    pivot_wider(names_from = 'type', values_from = `Detailed Statistics Arrivals`) %>% 
    mutate(file_dest = str_remove(file_dest, "Destination Airport: "),
           file_airline = str_remove(file_airline, "Airline: ")) %>% 
    mutate(name_dest = str_extract(flight_data[i], "(?<=^[[:lower:]]{2,5}-)[[:lower:]]{2,10}_[[:lower:]]{2,10}|(?<=^[[:lower:]]{2,5}-)[[:lower:]]{1,15}"),
           name_airline = str_extract(flight_data[i], "^[[:lower:]]{2,5}")) %>% 
    bind_rows(check_flight_data)
}

# make sure airlines are all correct - if so, this should have 0 rows
check_flight_data %>% mutate(name_airline = str_to_upper(name_airline),
                             name_airline = ifelse(name_airline=="DELTA", "DL", name_airline),
                             name_airline = ifelse(name_airline=="SW", "WN", name_airline)) %>% 
  filter(!str_detect(file_airline, paste0(name_airline)))

# make sure destinations match up - if so, this should have 0 rows
check_flight_data %>% select(name_dest, file_dest) %>% unique() %>% 
  group_by(file_dest) %>% 
  summarise(n=n()) %>% 
  filter(n!=1)

### DATA PROCESSING ####

# empty tibble
cleanData <- tibble()

# loop over each file, clean and store data
for (i in 1:length(flight_data)) {
  thisFile <- read_csv(here(paste0("data/raw-flight-data/", flight_data[i])), show_col_types = F, skip = 6) %>% 
    rename(date = 'Date (MM/DD/YYYY)',
           flight_no = 'Flight Number',
           airline = 'Carrier Code') %>% 
    select(airline, date, flight_no) %>% 
    group_by(date, airline) %>% 
    summarise(n=n()) %>% 
    mutate(region = str_extract(flight_data[i], "(?<=^[[:lower:]]{2,5}-)[[:lower:]]{2,10}_[[:lower:]]{2,10}|(?<=^[[:lower:]]{2,5}-)[[:lower:]]{1,15}")) %>% 
    ungroup()

  cleanData <- bind_rows(thisFile, cleanData)
}

# for regions with multiple words, replace underscore with space, fix dc manually, and filter date range to match that of traveler data
cleanData <- cleanData %>% mutate(region = ifelse(str_detect(region, "_"), 
                                                  str_replace(region, "_", " "),
                                                  region),
                                  region = str_to_title(region),
                                  region = ifelse(region=="Washington Dc", "Washington D. C.", region)) %>% 
  mutate(date = as.Date(date, "%m/%d/%Y")) %>% 
  filter(date >= "2020-05-29" & date <= "2021-07-25") %>% 
  arrange(region, date)

# export cleaned flight data to csv
write.csv(cleanData, "data/flight_data_clean.csv", row.names = FALSE)

# compute totals for each region on each date, store in new dataframe
cleanData_totals <- cleanData %>% group_by(date, region) %>% 
  summarise(total = sum(n)) %>% 
  ungroup()

# grab list of regions to loop over for plotting
regions <- cleanData_totals %>% select(region) %>% unique() %>% pull()

# only use list of regions with 3 or more airlines worth of data
regions_complete <- cleanData %>% select(region, airline) %>% unique() %>% 
  group_by(region) %>% 
  summarise(n=n()) %>% 
  filter(n>=3) %>% 
  pull(region)

### read in rate data - with high rmse values removed ###
## NOTE - using this vs the unfiltered data doesn't actually make a diff for us b/c 
## the only ones w/ high RMSE are from Nunavut and Yukon 
rates_no_age <- read_csv("data/rates_no_age_filtered.csv", show_col_types = F)

# compute sex-aggregated rates
aggregated_rates <- rates_no_age %>% select(date, region, sex, daily_trav, daily_pop) %>% 
  group_by(date, region) %>% 
  summarise(daily_trav = sum(daily_trav),
            daily_pop = sum(daily_pop),
            rate = daily_trav/daily_pop) %>% 
  ungroup() %>% 
  select(date, region, rate)

# join sex-aggregated rates and total flights by date and region, store in new df
df <- left_join(aggregated_rates, cleanData_totals, by=c("date", "region")) %>% 
  filter(!is.na(total)) %>% 
  rename(total_flights = total,
         travel_rate = rate)

### export this dataset for later use in the paper ###
write.csv(df, "data/flight_data_aggregate.csv", row.names = FALSE)

### plots with the raw data for the two time series ###

# start pdf
pdf("plots/time_series.pdf", width = 8, height = 6)

# loop over regions
for (i in 1:length(regions_complete)) {
  
  p <- df %>% pivot_longer(c(travel_rate, total_flights), names_to = "names", values_to = "value") %>% 
    filter(region==regions_complete[i]) %>% 
    plot_time_series(.date_var = date,
                     .value = value,
                     .facet_vars = names,
                     .interactive = FALSE,
                     .title = paste0("Time Series Plots - ", regions_complete[i]))
  
  print(p)
}

# close pdf
dev.off()

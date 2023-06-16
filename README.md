# Measuring short-term mobility patterns in North America using Facebook Advertising data, with an application to adjusting Covid-19 mortality rates

This repository contains all materials relating to our paper exploring short-term mobility in North America using data scraped from Facebook's advertising platform. 

This is joint work by Lindsay Katz, Michael Chong, and Monica Alexander.

## data

This folder contains all the data used in our analysis.

The `clean-fb-data` folder contains the cleaned Facebook data and resulting travel rates data.

1.  `pop_data_clean_age.csv` and `pop_data_clean_no_age.csv` have the cleaned daily and monthly population count data disaggregated by age and not disaggregated by age, respectively
2.  `travel_data_clean_age.csv` and `travel_data_clean_no_age.csv` have the cleaned daily and monthly traveller count data disaggregated by age and not disaggregated by age, respectively
3.  `rates_age.csv` and `rates_no_age.csv` contain data on rates of travel, disaggregated by age and not disaggregated by age, respectively. Each CSV has the observed rate (`obs_rate`) and the estimated rate and its associated standard error (`loess_rate` and `loess_se`), where the smoothed estimates were extracted from a loess model fit individually for each age/sex group for each region.
4.  `rates_age_filtered.csv` and `rates_no_age_filtered.csv` are subsets of `rates_age.csv` and `rates_no_age.csv` (respectively). To filter these data, we computed the root mean square error (RMSE) for each demographic grouping, and filtered out those data with a RMSE greater than the 95th quantile value.
5.  `rates_age_monthly.csv` contains travel rates data at the monthly level, disaggregated by age. This file was used for our Covid-19 mortality rate adjustment application.

The `flight_data_aggregate.csv` file contains the cleaned daily inbound flight volume data for each state, aggregated by airline. The raw data are from the Bureau of Transportation Statistics [database](https://www.transtats.bts.gov/ontime/arrivals.aspx).

The `census_pop_all_ages.csv` file contains the cleaned population estimates data by state, age, and sex, for 1 July 2020 and 1 July 2021. The raw data are from the Census Bureau's State Population by Characteristics: 2020-2022 [dataset](https://www.census.gov/data/datasets/time-series/demo/popest/2020s-state-detail.html).

Further, the `covid19_deaths_all_ages.csv` contains the cleaned monthly Covid-19 deaths data by age group, sex and state. This includes death counts for a number of uniquely classified causes of death. The raw data are available for download from the CDC [data catalog](https://data.cdc.gov/NCHS/Provisional-COVID-19-Deaths-by-Sex-and-Age/9bhg-hcku).

Finally, the `age_specific_death_rates.csv` file contains our computed age-specific death rates, calculated using the `census_pop_all_ages.csv` and `covid19_deaths_all_ages.csv` data.

## scripts

1.  `01_fb_data_clean.R` - read in and clean the scraped Facebook marketing API data.

2.  `02_get_daily_travel_rates.R` - compute daily travel rates, extract smooth estimates and produce time series plots for these travel rates.

3.  `03_travel_rate_analysis.R` - analyze daily travel rates data both empirically and with more visualizations. In this script, we compute and explore 3 summary metrics. This script produces the gender ratio plots found in the `plots` folder.

4.  `04_flight_data_analysis.R` - clean the flight volume data, and create plots comparing temporal trends in flight volume to those in travel rates.

5.  `05_mortality_data_clean.R` - clean the Census population data for each state, and the Covid-19 deaths data, in preparation for computing monthly Covid-19 mortality rates.

6.  `06_get_monthly_travel_rates.R` - compute monthly travel rates using the same methods in the second script, which will be used for adjusting monthly Covid-19 mortality rates in the final script.
7.  `07_covid_deaths_analysis.Rmd` - this Rmd file (and it's corresponding PDF) contain the code used to perform our Covid-19 mortality rate adjustment application.

## plots

1.  `fb_travel_rates_age.pdf` contains the plots of traveler rates for each region over time, disaggregated by age group and sex.

2.  `fb_gender_ratio.pdf` contains plots of the male to female ratio of travel rate for each region over time.

3.  `flights_time_series.pdf` contains time series plots for flight volume and aggregated travel rate for each state.

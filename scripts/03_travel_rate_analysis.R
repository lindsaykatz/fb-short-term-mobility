#### script to analyze daily travel rates, empirically and with more visualizations ####

# read in necessary packages
library(tidyverse)
library(usmap)
library(mapcan)
library(geofacet)

### read in rate data ###
rates_age <- read_csv("data/clean-fb-data/rates_age.csv", show_col_types = F)
rates_no_age <- read_csv("data/clean-fb-data/rates_no_age.csv", show_col_types = F)

### compute root MSE ###
# this allows us to assess model fit to the data - higher implies worse fit
# nunavut, yukon and NWT have highest MSE notice yukon F 30-50 is RMSE of 0 b/c every single rate is 0.
rmse_age <- rates_age %>% 
  mutate(sq_diff = (loess_rate-obs_rate)^2) %>% 
  group_by(region, sex, age_gp) %>% 
  summarise(rmse=sqrt(mean(sq_diff))) %>% 
  ungroup()

# left join so that we have rmse values to filter with, and use cutoff of 0.0116 (95th quantile)
# also filtering out rmse==0 b/c one age group in Yukon has 0 for all obs and it results in 0 rmse
rates_age <- left_join(rates_age, rmse_age, by=c("region", "sex", "age_gp")) %>% 
  filter(rmse < quantile(rmse_age$rmse, probs=0.95) & rmse!=0) %>% 
  select(-rmse)

# checked rows w/ rmse==0 and this is only the case for the F 30-50 group of Yukon
left_join(rates_age, rmse_age, by=c("region", "sex", "age_gp")) %>%
  filter(rmse==0) %>% select(region, age_gp, sex) %>% distinct()

# same as data with age, nunavut, yukon and NWT have highest RMSE
rmse_no_age <- rates_no_age %>% 
  mutate(sq_diff = (loess_rate-obs_rate)^2) %>% 
  group_by(region, sex) %>% 
  summarise(rmse=sqrt(mean(sq_diff))) %>% 
  ungroup()

# left join so that we have rmse values to filter with, and use cutoff of 0.01 also filtering out rmse==0
# filtering out US level data - this was only collected in the non-age specific data and we aren't using it
rates_no_age <- left_join(rates_no_age, rmse_no_age, by=c("region", "sex")) %>% 
  filter(rmse < quantile(rmse_age$rmse, probs=0.95) & rmse!=0) %>% 
  select(-rmse) %>% 
  filter(region!="United States")

### export this filtered data ###
write.csv(rates_age, "data/rates_age_filtered.csv", row.names = F)
write.csv(rates_no_age, "data/rates_no_age_filtered.csv", row.names = F)

### MALE TO FEMALE TRAVEL RATE RATIO ###

# filtering for groups where we have both M and F (some days just have one obs)
gender_ratio <- rates_no_age %>% select(date, sex, region, loess_rate) %>% 
  group_by(date, region) %>% 
  filter(n()==2) %>% 
  summarise(ratio = loess_rate[sex=="M"]/loess_rate[sex=="F"]) %>% 
  ungroup()

# we can see that for 60 of the 62 regions where we have ratios in the no age data
# the travel rate for men is always greater than that of women
gender_ratio %>% group_by(region) %>% 
  summarise(over1 = ifelse(all(ratio>1), 1, 0)) %>% 
  group_by(over1) %>% 
  summarise(n = n())

# look at distribution of ratios
hist(gender_ratio$ratio)
density(gender_ratio$ratio)

# look at regions where the ratio is always greater than the 75th quantile value
# i.e. the gender difference is more pronounced over the entire time span
# there are only 8 regions where this is the case (Alberta, Louisiana, Manitoba, Mississippi, North Dakota, Saskatchewan, Texas and Wyoming)
gender_ratio %>% group_by(region) %>% 
  summarise(top25 = ifelse(all(ratio>quantile(gender_ratio$ratio, probs = .75)), 1, 0)) %>% 
  filter(top25==1)

# define provinces to look at Canada vs US averages
provinces <- c("Alberta", "British Columbia", "Ontario", "Quebec", "Prince Edward Island", "Nunavut",
               "Northwest Territories", "Newfoundland and Labrador", "Yukon", "Saskatchewan", "Manitoba",
               "New Brunswick", "Nova Scotia")

# look at average ratio in Canada
gender_ratio %>% filter(region %in% provinces) %>% summarise(mean(ratio))

# look at average ratio in US
gender_ratio %>% filter(!(region %in% provinces)) %>% summarise(mean(ratio))

# top 5 average ratio over entire time span
gender_ratio %>% group_by(region) %>% 
  summarise(mean = mean(ratio)) %>% 
  arrange(desc(mean)) %>% 
  slice(1:5)

# we might want to use the age data so we can focus on working age group ratio only ?
# didn't end up using this
# rates_age %>% select(date, sex, age_gp, region, loess_rate) %>% 
#   filter(age_gp!="50-65") %>% 
#   group_by(date, region) %>% 
#   filter(n()==4) %>%
#   group_by(date, region, sex) %>% 
#   summarise(loess_rate = sum(loess_rate)) %>% 
#   group_by(date, region) %>% 
#   summarise(ratio = loess_rate[sex=="M"]/loess_rate[sex=="F"]) %>% 
#   ungroup() %>% 
#   group_by(region) %>% 
#   summarise(over1 = ifelse(all(ratio>quantile(gender_ratio$ratio, probs=0.75)), 1, 0)) %>% 
#   filter(over1==1)

# make plots
regions_no_age <- rates_no_age %>%
  select(region) %>% 
  distinct() %>% 
  pull()

# pdf of plots for whole time period (not by age)
# start pdf
pdf("fb_gender_ratio.pdf", width = 8, height = 6)

# loop through regions
for(i in 1:length(regions_no_age)){
  p <- gender_ratio %>% 
    filter(region==regions_no_age[i]) %>% 
    ggplot() + 
    geom_point(aes(x=date, y=ratio)) + 
    ylim(0.5, 2.0)+
    theme_bw()+
    geom_hline(aes(yintercept=1), colour="red", linetype="dashed") +
    scale_x_date(date_labels = "%b %Y") +
    labs(x="Time", y = "Male to female ratio", 
         title="Male to female ratio of rate of travel over time",
         subtitle = paste0(regions_no_age[i]))
  print(p)
}

# shut down pdf
dev.off()

### SEASONALITY METRIC ###

# winter peak or trough - max/min between January and March 2021
# earliest date in January we have for age data is 03 Jan 2021, and then we'll use 01 March 2021 as our endpoint
# adding flag for peak, else trough
# this code results in a tibble that also has the peak or trough for each age/sex group, not just the max abs diff of all of them
# max or min flag denotes which demographic group from each region contains the max or min peak or trough respectively
peak_or_trough_age <- rates_age %>% filter(date>="2020-10-01" & date<="2021-03-31") %>% 
  select(date, sex, region, age_gp, loess_rate) %>%
  left_join(., rates_age %>% 
              filter(date=="2020-07-01") %>% 
              select(date, sex, region, age_gp, loess_rate) %>% 
              rename(july1_est = loess_rate,
                     july1_2020 = date), 
            by=c("sex", "region", "age_gp")) %>% 
  mutate(relative_diff = (loess_rate-july1_est)/july1_est,
         abs_relative_diff = abs(relative_diff)) %>% 
  group_by(region, age_gp, sex) %>% 
  filter(abs_relative_diff==max((abs_relative_diff))) %>% 
  arrange(region) %>% 
  ungroup() %>% 
  group_by(region) %>% 
  mutate(max_or_min = ifelse(abs_relative_diff==max((abs_relative_diff)), 1, 0)) %>% 
  ungroup() %>% 
  mutate(peak = ifelse(relative_diff > 0, 1, 0))

# in 37 of the 64 regions, the biggest peak or trough is attributed to the 50-65 age group
peak_or_trough_age %>% filter(max_or_min==1) %>% group_by(age_gp) %>% summarise(n=n())

# top 5 positive relative change from July 1 2020 - both Canada and US
peak_or_trough_age %>% 
  arrange(desc(relative_diff)) %>% 
  slice(1:5) %>% 
  select(region, sex, age_gp, relative_diff)

# top 5 positive relative change from July 1 2020 - just Canada
peak_or_trough_age %>% 
  filter(region %in% provinces) %>% 
  arrange(desc(relative_diff)) %>% 
  slice(1:5) %>% 
  select(region, sex, age_gp, relative_diff)

# top 5 positive relative change from July 1 2020 - just US
peak_or_trough_age %>% 
  filter(!(region %in% provinces)) %>% 
  arrange(desc(relative_diff)) %>% 
  slice(1:5) %>% 
  select(region, sex, age_gp, relative_diff)

# top 5 negative relative change from July 1 2020 - both Canada and US
peak_or_trough_age %>% 
  arrange(relative_diff) %>% 
  slice(1:5) %>% 
  select(region, sex, age_gp, relative_diff)

# top 5 negative relative change from July 1 2020 - just Canada
peak_or_trough_age %>% 
  filter(region %in% provinces) %>% 
  arrange(relative_diff) %>% 
  slice(1:5) %>% 
  select(region, sex, age_gp, relative_diff)

# top 5 negative relative change from July 1 2020 - just US
peak_or_trough_age %>% 
  filter(!(region %in% provinces)) %>% 
  arrange(relative_diff) %>% 
  slice(1:5) %>% 
  select(region, sex, age_gp, relative_diff)

# top 10 biggest magnitude troughs
peak_or_trough_age %>% filter(peak==0 & max_or_min==1) %>% 
  arrange(desc(abs_relative_diff)) %>% 
  select(date, sex, region, age_gp, abs_relative_diff) %>% 
  slice(1:10) %>% 
  mutate(abs_relative_diff = round(abs_relative_diff, 4)) %>% 
  rename("Date" = date,
         "Sex" = sex,
         "Region" = region,
         "Age Group" = age_gp,
         "Rate of Change from July 1 2020" = abs_relative_diff)

#### YEAR ON YEAR CHANGE #####

# create tibble of all 01 July 2020 and 01 July 2021 estimates, compute percent growth
yr_on_yr_age <- rates_age %>% filter(date=="2020-07-01" | date=="2021-07-01") %>% 
  select(date, sex, region, age_gp, loess_rate) %>% 
  pivot_wider(., values_from = loess_rate, names_from = date) %>% 
  rename(july1_2020 = `2020-07-01`,
         july1_2021 = `2021-07-01`) %>% 
  mutate(yr_on_yr = ((july1_2021 - july1_2020)/(july1_2020))*100)

# lets look at the top 5 positive relative change
yr_on_yr_age %>% arrange(desc(yr_on_yr)) %>% slice(1:5)

# lets look at the top 5 negative relative change
yr_on_yr_age %>% arrange(yr_on_yr) %>% slice(1:5)

# same idea for no age
yr_on_yr_no_age <- rates_no_age %>% filter(date=="2020-07-01" | date=="2021-07-01") %>% 
  select(date, sex, region, loess_rate) %>% 
  pivot_wider(., values_from = loess_rate, names_from = date) %>% 
  rename(july1_2020 = `2020-07-01`,
         july1_2021 = `2021-07-01`) %>% 
  mutate(yr_on_yr = ((july1_2021 - july1_2020)/(july1_2020))*100)

# lets look at the top 5 positive relative change
yr_on_yr_no_age %>% arrange(desc(yr_on_yr)) %>% slice(1:5)

# lets look at the top 5 negative relative change
yr_on_yr_no_age %>% arrange(yr_on_yr) %>% slice(1:5)

#############################################################################################
##### NOTE - all code below is extra/initial EDA #####

# define provinces for maps
provinces <- c("Alberta", "British Columbia", "Ontario", "Quebec", "Prince Edward Island", "Nunavut",
               "Northwest Territories", "Newfoundland and Labrador", "Yukon", "Saskatchewan", "Manitoba",
               "New Brunswick", "Nova Scotia")

# define states for maps
states <- rates_age %>% select(region) %>% filter(!(region %in% provinces)) %>% distinct() %>% pull()

# load us map data
states_data <- usmap::us_map() %>% 
  rename(long = x,
         lat = y,
         region = full) %>% 
  mutate(region = ifelse(region=="District of Columbia", "Washington D. C.", region))

# grab state centroids for us map
states_centroid_labels <- usmapdata::centroid_labels("states")

# load canada map data
provinces_data <- mapcan(boundaries = province, type = standard) %>% 
  select(-pr_french) %>% 
  rename(region = pr_english,
         abbr = pr_alpha)

# can't find centroids for canada - trying to make them myself
# northwest territories and nunavut are bad so need to fix manually
province_centroid_labels <- provinces_data %>% 
  group_by(region, abbr) %>% 
  select(region, abbr, lat, long) %>%
  summarise(max_long = max(long), min_long = min(long), max_lat = max(lat), min_lat = min(lat)) %>% 
  mutate(centroid_lat = (min_lat+max_lat)/2,
         centroid_long = (min_long+max_long)/2) %>% 
  select(region, abbr, centroid_lat, centroid_long) %>% 
  ungroup() %>% 
  mutate(centroid_long = ifelse(abbr=="NT", -1250000, centroid_long),
         centroid_lat = ifelse(abbr=="NT", 2000000, centroid_lat),
         centroid_long = ifelse(abbr=="NU", 0, centroid_long),
         centroid_lat = ifelse(abbr=="NU", 1750000, centroid_lat))


############################### geo facet ###############################

# geo facet of male to female ratio for US
gender_ratio %>% 
  filter(region %in% states) %>% 
  mutate(region = ifelse(region=="Washington D. C.", "District of Columbia", region)) %>% 
  ggplot() + 
  geom_point(aes(x=date, y=ratio), size=0.5) + 
  ylim(0.5, 2.0)+
  theme_bw()+
  geom_hline(aes(yintercept=1), colour="red", linetype="dashed") +
  scale_x_date(date_labels = "%b %Y") +
  labs(x="Time", y = "Male to female ratio")+
  facet_geo(~ region)+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

# geo facet of male to female ratio for Canada
gender_ratio %>% 
  filter(region %in% provinces) %>% 
  ggplot() + 
  geom_point(aes(x=date, y=ratio), size=0.5) + 
  ylim(0.5, 2.0)+
  theme_bw()+
  geom_hline(aes(yintercept=1), colour="red", linetype="dashed") +
  scale_x_date(date_labels = "%b %Y") +
  labs(x="Time", y = "Male to female ratio")+
  facet_geo(~ region, grid = "ca_prov_grid1")+
  theme(axis.text.x = element_text(angle = 90, hjust=1))

# geo facet travel rate no age US
rates_no_age %>% 
  filter(region %in% states) %>% 
  mutate(region = ifelse(region=="Washington D. C.", "District of Columbia", region)) %>% 
  ggplot(aes(x = date, y = loess_rate, color = sex)) +
  geom_line()+
  facet_geo(~ region) +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1)) +
  labs(x = "Time", y="Daily travel rate")

# geo facet travel rate no age Canada
rates_no_age %>% 
  filter(region %in% provinces) %>% 
  ggplot(aes(x=date, y=loess_rate, color=sex)) +
  geom_line()+
  facet_geo(~ region, grid = "ca_prov_grid1")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust=1))+
  labs(x = "Time", y="Daily travel rate")

########### gender ratio map
# US
gender_ratio %>% 
  group_by(region) %>% 
  summarise(avg_ratio = mean(ratio)) %>% 
  inner_join(states_data, ., by="region") %>% 
  filter(region %in% states) %>% 
  ggplot() + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=avg_ratio),
               color="white", size=0.2) +
  ggtitle("Average Male to Female ratio of rate of travel across 10 month period") +
  theme_mapcan() +
  geom_text(data = states_centroid_labels, aes(x=x, y=y, label=abbr), color="white", size=2)

# Canada
gender_ratio %>% 
  group_by(region) %>% 
  summarise(avg_ratio = mean(ratio)) %>% 
  add_row(region = c("Yukon", "Nunavut")) %>% 
  inner_join(provinces_data, ., by="region") %>% 
  filter(region %in% provinces) %>% 
  ggplot() + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=avg_ratio),
               color="white", size=0.2) +
  ggtitle("Average Male to Female ratio of rate of travel across 10 month period") +
  theme_mapcan() +
  geom_text(data = province_centroid_labels, aes(x=centroid_long, y=centroid_lat, label=abbr), color="white", size=2)

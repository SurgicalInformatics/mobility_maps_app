#Prepare data
library(lubridate)
library(tidyverse)
library(patchwork)
library(rmapshaper)
library(smoothr)

#load in data
load('data/mobility_data.rda')
eng_sco_wal_ni = readRDS('map/uk_map.rds')
unmatched_locations = read_csv('unmatched_names.csv') 

#Set dates
date_1 = '2020-03-29'
date_2 = '2020-04-15'
date_3 = '2020-05-02'
country = 'UK'

amount_to_average_dates = 7

#filter for selected dates only

uk_mobility_data_subnational_dates = uk_mobility_data_subnational %>% 
  filter(date == as.Date(date_1) | date == as.Date(date_1) + amount_to_average_dates | date == as.Date(date_1) - amount_to_average_dates |
           date == as.Date(date_2) | date == as.Date(date_2) + amount_to_average_dates | date == as.Date(date_2) - amount_to_average_dates |
           date == as.Date(date_3) | date == as.Date(date_3) + amount_to_average_dates | date == as.Date(date_3) - amount_to_average_dates )

uk_mobility_data_national_dates = uk_mobility_data_national %>% 
  filter(date == as.Date(date_1) | date == as.Date(date_2) | date == as.Date(date_3))

#Calculate a lagged average
uk_mobility_data_subnational_dates = uk_mobility_data_subnational_dates %>% 
  mutate(date_diff_1 = ifelse(as.Date(date_1) - date <= amount_to_average_dates & as.Date(date_1) - date >= -amount_to_average_dates, 'Within','Outwith'),
         date_diff_2 = ifelse(as.Date(date_2) - date <= amount_to_average_dates & as.Date(date_2) - date >= -amount_to_average_dates, 'Within','Outwith'),
         date_diff_3 = ifelse(as.Date(date_2) - date <= amount_to_average_dates & as.Date(date_2) - date >= -amount_to_average_dates, 'Within','Outwith')) %>% 
  group_by(sub_region_1, date_diff_1) %>% 
  mutate(residential_percent_change_from_baseline = ifelse(date == date_1, mean(residential_percent_change_from_baseline, na.rm = T),
                                                           residential_percent_change_from_baseline),
         retail_and_recreation_percent_change_from_baseline = ifelse(date == date_1, mean(retail_and_recreation_percent_change_from_baseline, 
                                                                                          na.rm = T),
                                                                     retail_and_recreation_percent_change_from_baseline),
         grocery_and_pharmacy_percent_change_from_baseline = ifelse(date == date_1, mean(grocery_and_pharmacy_percent_change_from_baseline, 
                                                                                         na.rm = T),
                                                                    grocery_and_pharmacy_percent_change_from_baseline),
         parks_percent_change_from_baseline = ifelse(date == date_1, mean(parks_percent_change_from_baseline, 
                                                                          na.rm = T),
                                                     parks_percent_change_from_baseline),
         transit_stations_percent_change_from_baseline = ifelse(date == date_1, mean(transit_stations_percent_change_from_baseline, 
                                                                                     na.rm = T),
                                                                transit_stations_percent_change_from_baseline),
         workplaces_percent_change_from_baseline = ifelse(date == date_1, mean(workplaces_percent_change_from_baseline, 
                                                                               na.rm = T),
                                                          workplaces_percent_change_from_baseline)) %>% 
  ungroup() %>% 
  group_by(sub_region_1, date_diff_2) %>% 
  mutate(residential_percent_change_from_baseline = ifelse(date == date_2, mean(residential_percent_change_from_baseline, na.rm = T),
                                                           residential_percent_change_from_baseline),
         retail_and_recreation_percent_change_from_baseline = ifelse(date == date_2, mean(retail_and_recreation_percent_change_from_baseline, 
                                                                                          na.rm = T),
                                                                     retail_and_recreation_percent_change_from_baseline),
         grocery_and_pharmacy_percent_change_from_baseline = ifelse(date == date_2, mean(grocery_and_pharmacy_percent_change_from_baseline, 
                                                                                         na.rm = T),
                                                                    grocery_and_pharmacy_percent_change_from_baseline),
         parks_percent_change_from_baseline = ifelse(date == date_2, mean(parks_percent_change_from_baseline, 
                                                                          na.rm = T),
                                                     parks_percent_change_from_baseline),
         transit_stations_percent_change_from_baseline = ifelse(date == date_2, mean(transit_stations_percent_change_from_baseline, 
                                                                                     na.rm = T),
                                                                transit_stations_percent_change_from_baseline),
         workplaces_percent_change_from_baseline = ifelse(date == date_2, mean(workplaces_percent_change_from_baseline, 
                                                                               na.rm = T),
                                                          workplaces_percent_change_from_baseline)) %>% 
  ungroup() %>% 
  group_by(sub_region_1, date_diff_3) %>% 
  mutate(residential_percent_change_from_baseline = ifelse(date == date_3, mean(residential_percent_change_from_baseline, na.rm = T),
                                                           residential_percent_change_from_baseline),
         retail_and_recreation_percent_change_from_baseline = ifelse(date == date_3, mean(retail_and_recreation_percent_change_from_baseline, 
                                                                                          na.rm = T),
                                                                     retail_and_recreation_percent_change_from_baseline),
         grocery_and_pharmacy_percent_change_from_baseline = ifelse(date == date_3, mean(grocery_and_pharmacy_percent_change_from_baseline, 
                                                                                         na.rm = T),
                                                                    grocery_and_pharmacy_percent_change_from_baseline),
         parks_percent_change_from_baseline = ifelse(date == date_3, mean(parks_percent_change_from_baseline, 
                                                                          na.rm = T),
                                                     parks_percent_change_from_baseline),
         transit_stations_percent_change_from_baseline = ifelse(date == date_3, mean(transit_stations_percent_change_from_baseline, 
                                                                                     na.rm = T),
                                                                transit_stations_percent_change_from_baseline),
         workplaces_percent_change_from_baseline = ifelse(date == date_3, mean(workplaces_percent_change_from_baseline, 
                                                                               na.rm = T),
                                                          workplaces_percent_change_from_baseline)) %>% 
  ungroup() %>% 
  filter(date == as.Date(date_1) |
           date == as.Date(date_2) |
           date == as.Date(date_3)) %>% 
  select(-date_diff_1, -date_diff_2, -date_diff_3)


uk_mobility_data_subnational_dates = uk_mobility_data_subnational_dates %>% 
  mutate(date_class = ifelse(date == as.Date(date_1), '1', NA),
         date_class = ifelse(date == as.Date(date_2), '2', date_class),
         date_class = ifelse(date == as.Date(date_3), '3', date_class)) %>%
  select(-country_region_code, -country_region, -sub_region_2) %>% 
  pivot_wider(names_from = date_class, 
              values_from = all_of(c("date", "retail_and_recreation_percent_change_from_baseline", "grocery_and_pharmacy_percent_change_from_baseline", 
                                     "parks_percent_change_from_baseline", "transit_stations_percent_change_from_baseline",
                                     "workplaces_percent_change_from_baseline", "residential_percent_change_from_baseline" 
              ))) %>% 
  select(-contains('_NA', ignore.case = FALSE))


eng_sco_wal_ni@data %>% 
  select(name, details) %>%
  left_join(unmatched_locations, by = 'name') %>% 
  mutate(merge_name = name) %>% 
  mutate(name = ifelse(is.na(new_name), name, new_name)) %>% 
  left_join(uk_mobility_data_subnational_dates, by = c('name' = 'sub_region_1')) %>% 
  dplyr::select(-new_name) -> uk_mobility_data_subnational_dates

if(country == 'UK'){
  line_plot_data = uk_mobility_data_national
  uk_mobility_data_subnational_dates = uk_mobility_data_subnational_dates %>%  select(-details)
} else if (country == 'Scotland' | country == 'England' | country == 'Wales' | country == 'Northern Ireland'){
  uk_mobility_data_subnational_dates = uk_mobility_data_subnational_dates %>% subset(details == country) %>%  select(-details)
  eng_sco_wal_ni = eng_sco_wal_ni %>% subset(details == country)
  line_plot_data = uk_mobility_data_subnational %>% filter(sub_region_1 %in% eng_sco_wal_ni$name) %>% 
    group_by(country_region, date) %>% 
    summarise(retail_and_recreation_percent_change_from_baseline = mean(retail_and_recreation_percent_change_from_baseline, na.rm = T),
              grocery_and_pharmacy_percent_change_from_baseline = mean(grocery_and_pharmacy_percent_change_from_baseline, na.rm = T),
              parks_percent_change_from_baseline = mean(parks_percent_change_from_baseline, na.rm = T),
              transit_stations_percent_change_from_baseline = mean(transit_stations_percent_change_from_baseline, na.rm = T),
              residential_percent_change_from_baseline = mean(residential_percent_change_from_baseline, na.rm = T),
              workplaces_percent_change_from_baseline = mean(workplaces_percent_change_from_baseline, na.rm = T)) %>% drop_na()
}



uk_mobility_data_sp = uk_mobility_data_subnational_dates %>% 
  sp::merge(eng_sco_wal_ni, ., by.x = 'name', by.y = 'merge_name')

save.image('mapping_data.RData')

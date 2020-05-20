#Prepare and pull out the google data
#Draw from data sources
library(lubridate)
library(tidyverse)

#download
today_date = lubridate::today()

#next see if there's a date from today, if not, test if there is a date available for today
file_to_load = tibble(files = list.files(path = "reports/google", all.files = T, recursive = T, include.dirs = T, full.names = T)) %>% 
  mutate(date_to_use = ifelse(grepl(today_date, files), 'Latest', NA)) %>% 
  filter(date_to_use == 'Latest') %>% pull(files)

if(length(file_to_load) == 0){
  
  download.file('https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv', 
                destfile = paste0('reports/google/google_', today_date, '_mobility_report.csv'))
  
  global_mobility_data = read_csv(paste0('reports/google/google_', today_date, '_mobility_report.csv'))
  
} else if(length(file_to_load) == 1){
  
  global_mobility_data = read_csv(file_to_load)

  }

#prepare the data

global_mobility_data = global_mobility_data %>% 
  mutate(sub_region_1 = stringr::str_to_title(sub_region_1),
         sub_region_1 = gsub(' Council', '', sub_region_1),
         sub_region_1 = gsub('Borough Of ', '', sub_region_1),
         sub_region_1 = gsub(' County Borough', '', sub_region_1),
         sub_region_1 = gsub('Na H-Eileanan An Iar', 'Eilean Siar', sub_region_1),
         sub_region_1 = gsub('Wrexham Principal Area', 'Wrexham', sub_region_1))

uk_mobility_data = global_mobility_data %>%
  filter(country_region_code == 'GB')

uk_mobility_data_national = global_mobility_data %>%
  filter(country_region_code == 'GB', 
         is.na(sub_region_1))

uk_mobility_data_subnational = global_mobility_data %>%
  filter(country_region_code == 'GB', 
         !is.na(sub_region_1))

#Check data into an rdata image
rm(file_to_load, today_date)

save.image('data/mobility_data.rda')
#Read GEOJson world file
library(tidyverse)
world_map = geojsonio::geojson_read('world_maps/countries.geojson', what = 'sp')

#Linkage CSVs
iso_code_conv = read_csv('world_maps/iso_code_conversion.csv')

#Add the ISO2 code for lookup
world_map@data = world_map@data %>% 
  left_join(iso_code_conv %>% select(iso2, iso3), by = c('ISO_A3' = 'iso3'))

#Now merge in 
example_data_to_merge = global_mobility_data %>% 
  filter(is.na(sub_region_1) & date == '2020-05-24')

#plot
world_map_fortified = fortify(world_map, region = 'iso2') %>% 
  left_join(example_data_to_merge, by = c('id' = 'country_region_code'))

world_map_fortified %>%  
  ggplot(aes(y = lat, x = long, group = group)) + 
  geom_polygon(aes(fill = retail_and_recreation_percent_change_from_baseline), size = 0.25, color = 'black')
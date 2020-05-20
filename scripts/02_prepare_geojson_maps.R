#prepare the maps
library(tidyverse)
library(patchwork)
library(rmapshaper)
library(smoothr)

#geoJSON
eng_wal_counties_unitary_authorities = geojsonio::geojson_read('geoJSONs/unitary_auth_counties_eng_wal.geojson', what = 'sp')

sco_counties_unitary_authorities = geojsonio::geojson_read('geoJSONs/scotland_la.geojson', what = 'sp')

ni_counties = geojsonio::geojson_read('geoJSONs/ni_counties.geojson', what = 'sp')

#Remove little islands and simplify
eng_wal_counties_unitary_authorities = ms_filter_islands(eng_wal_counties_unitary_authorities, sys = T, min_area = 9999999)
eng_wal_counties_unitary_authorities = ms_simplify(eng_wal_counties_unitary_authorities, sys = T, keep = 0.27, weighting = 1)
eng_wal_counties_unitary_authorities = smooth(eng_wal_counties_unitary_authorities, method = 'densify')

sco_counties_unitary_authorities = ms_filter_islands(sco_counties_unitary_authorities, sys = T, min_area = 99999999)
sco_counties_unitary_authorities = ms_simplify(sco_counties_unitary_authorities, sys = T, keep = 0.03, weighting = 1)
sco_counties_unitary_authorities = smooth(sco_counties_unitary_authorities, method = 'densify')

ni_counties = ms_filter_islands(ni_counties, sys = T, min_area = 99999999)
ni_counties = ms_simplify(ni_counties, sys = T, keep = 0.15, weighting = 0.95)
ni_counties = smooth(ni_counties, method = 'densify')

#prepare geoJSONs for combining
eng_wal_counties_unitary_authorities$bng_e = NULL
eng_wal_counties_unitary_authorities$bng_n = NULL
eng_wal_counties_unitary_authorities$long = NULL
eng_wal_counties_unitary_authorities$lat = NULL
eng_wal_counties_unitary_authorities$st_areashape = NULL
eng_wal_counties_unitary_authorities$st_lengthshape = NULL
eng_wal_counties_unitary_authorities$details = NULL

ni_counties$id = ni_counties$COUNTY_ID
ni_counties$second_id = ni_counties$OBJECTID
ni_counties$name = ni_counties$CountyName
ni_counties$details = 'Northern Ireland'

ni_counties$COUNTY_ID = NULL
ni_counties$OBJECTID = NULL
ni_counties$CountyName = NULL
ni_counties$Area_SqKM = NULL

names(sco_counties_unitary_authorities) = c('id', 'second_id', 'name', 'details')

sco_counties_unitary_authorities$details = 'Scotland'

names(eng_wal_counties_unitary_authorities) = c('id', 'second_id', 'name', 'details')

eng_sco_wal = rbind(sco_counties_unitary_authorities, eng_wal_counties_unitary_authorities)

eng_sco_wal_ni = rbind(eng_sco_wal, ni_counties)

#make the names correct/ line up with google
location_eng_sco_wal_ni = eng_sco_wal_ni$name %>% 
  tibble(name = .) %>%
  mutate(name = stringr::str_to_title(name),
         name = gsub('Londonderry', 'Derry And Strabane', name),
         name = gsub(', City Of', '', name),
         name = gsub(', County Of', '', name),
         name = gsub('Borough Of ', '', name),
         name = gsub('Orkney Islands', 'Orkney', name),
         name = gsub('City Of Edinburgh', 'Edinburgh', name),
         name = gsub('Bristol', 'City Of Bristol', name),
         name = gsub('Armagh', 'Armagh City And Banbridge And Craigavon', name))

eng_sco_wal_ni$name = location_eng_sco_wal_ni$name

rm(list=setdiff(ls(), c('eng_sco_wal_ni')))

saveRDS(eng_sco_wal_ni, 'map/uk_map.rds')

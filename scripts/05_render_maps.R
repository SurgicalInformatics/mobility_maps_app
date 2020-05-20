#Render in sequence

#run this once per day
source('scripts/01_download_from_google.R')

#this only needs running once ever (for UK)
source('scripts/02_prepare_geojson_maps.R')

#run once to get maps
source('scripts/03_prepare_data.R')
source('scripts/04_make_maps.R')

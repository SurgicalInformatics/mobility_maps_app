#Make the plots
source('scripts/01_download_from_google.R')
source('scripts/02_prepare_geojson_maps.R')
source('scripts/03_prepare_data.R')

#themes
ggplot_map = function(...) ggplot2::ggplot(...) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = 20, vjust= 5),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank(),
        legend.text=element_text(size = 16),
        legend.title=element_blank(),
        plot.title = element_text(size = 22, hjust = 0.45, vjust = 0.5)) + theme(legend.position="bottom") + 
  guides(fill=guide_colourbar(direction = "horizontal", barheight = 0.6, barwidth = 10, frame.colour = 'black'))#+


## Change in Residential Mobility
chunk_var = 'residential_percent_change_from_baseline'
chunk_map_text = 'Hello'
chunk_map_y_lab = 'Residential'

chunk_data_sp = uk_mobility_data_sp

var_1 = paste0(chunk_var, '_1')
var_2 = paste0(chunk_var, '_2')
var_3 = paste0(chunk_var, '_3')

test = chunk_data_sp@data
colnames(test)

chunk_data_sp@data = chunk_data_sp@data %>% 
  select(name, id, second_id, details, 'date_3', 'date_2', 'date_1', contains(chunk_var)) %>% 
  mutate(date_1_to_3 := !!as.name(var_3) - !!as.name(var_1),
         date_2_to_3 := !!as.name(var_3) - !!as.name(var_2),
         date_1_to_2 := !!as.name(var_2) - !!as.name(var_1))                            

#fortify
chunk_data_sp_points = fortify(chunk_data_sp, region = "id")
chunk_data_sp_fort = dplyr::left_join(chunk_data_sp_points, chunk_data_sp@data, by="id")

#plot the first date only
fixed_baseline_plot = chunk_data_sp_fort %>% 
  ggplot_map(aes(y = lat, x = long, group = group)) + geom_polygon(aes_string(fill = var_1), size = 0.25, color = 'black') + coord_equal(ratio = 1.3) +
  scale_fill_gradientn(colours = c("#e89f00", "white", "blue"), values = c('0', '0.25', '0.5', '0.75', '1'), 
                       breaks = 50*-2:2, labels =  50*-2:2, limits = c(-50,50), name = chunk_map_text) + 
  ylab(chunk_map_y_lab) + theme(axis.title.y = element_blank()) +
  ggtitle(as.Date(date_1) %>% format("%d-%B-%Y"))

# #plot difference between first and third date
change_plot = chunk_data_sp_fort %>%
  ggplot_map(aes(y = lat, x = long, group = group)) + geom_polygon(aes_string(fill = 'date_1_to_3'), size = 0.25, color = 'black') + 
  coord_equal(ratio = 1.3) +
  scale_fill_gradientn(colours =rev(c("#0e1b76", "white", "#e89f00")), values = c(0, 0.25, 0.50, 0.75, 1), breaks = 50*-2:2, 
                       labels =  50*-2:2, limits = c(-50,50), name = chunk_map_text) +
  ylab(chunk_map_y_lab) + theme(axis.title.y = element_blank()) +
  ggtitle(paste0(as.Date(date_1) %>% format("%d-%b-%Y"), ' to ', as.Date(date_3) %>% format("%d-%b-%Y")))

#Plot line change
plot_line_change = line_plot_data %>% 
  ggplot(aes(x = date)) + 
  geom_vline(xintercept = as.Date('2020-03-21'), colour="dark grey", size = 1, linetype = 'dashed') +
  geom_text(aes(x= as.Date('2020-03-21'), label="Restrictions\nImposed", y = 15), colour="grey", angle=90, size = 5) +
  geom_path(aes_string(y = chunk_var), size = 1.8, colour = '#454545') +
  geom_point(aes_string(y = chunk_var), size = 4, colour = '#00acac') + theme_minimal() + xlab('Date') + 
  ylab('Change from\nBaseline (%)') + ylim(min(uk_mobility_data_national[chunk_var]) - 2,
                                           max(uk_mobility_data_national[chunk_var]) + 5) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20, vjust = 5),
        axis.text.y = element_text(size = 18),
        plot.margin = unit(c(1,1,1,1),"cm"))

(fixed_baseline_plot | change_plot) / plot_line_change +  plot_layout(heights = unit(c(35, 4), c('cm', 'cm')), widths = unit(c(40, 12), c('cm', 'cm'))) 

rm(chunk_var, plot_line_change, chunk_data_sp, var_1, var_2, var_3)

## Retail and Recreation

chunk_var = 'retail_and_recreation_percent_change_from_baseline'
chunk_map_text = 'Hello'
chunk_map_y_lab = 'Retail'

chunk_data_sp = uk_mobility_data_sp

var_1 = paste0(chunk_var, '_1')
var_2 = paste0(chunk_var, '_2')
var_3 = paste0(chunk_var, '_3')

test = chunk_data_sp@data

chunk_data_sp@data = chunk_data_sp@data %>% 
  select(name, id, second_id, details, date_3, date_2, date_1, contains(chunk_var)) %>% 
  mutate(date_1_to_3 := !!as.name(var_3) - !!as.name(var_1),
         date_2_to_3 := !!as.name(var_3) - !!as.name(var_2),
         date_1_to_2 := !!as.name(var_2) - !!as.name(var_1))                            

#fortify
chunk_data_sp_points = fortify(chunk_data_sp, region = "id")
chunk_data_sp_fort = dplyr::left_join(chunk_data_sp_points, chunk_data_sp@data, by="id")

#plot the first date only
fixed_baseline_plot = chunk_data_sp_fort %>% 
  ggplot_map(aes(y = lat, x = long, group = group)) + geom_polygon(aes_string(fill = var_1), size = 0.25, color = 'black') + coord_equal(ratio = 1.3) +
  scale_fill_gradientn(colours = c("white", "white", "red"), values = c(0, 0.25, 1), 
                       breaks = 50*-2:2, labels =  50*-2:2, limits = c(-100,0), name = chunk_map_text) + 
  ylab(chunk_map_y_lab) + theme(axis.title.y = element_blank()) +
  ggtitle(as.Date(date_1) %>% format("%d-%B-%Y"))

# #plot difference between first and third date
change_plot = chunk_data_sp_fort %>%
  mutate(date_1_to_3 = ifelse(date_1_to_3 < 0, 0, date_1_to_3)) %>% 
  ggplot_map(aes(y = lat, x = long, group = group)) + geom_polygon(aes_string(fill = 'date_1_to_3'), size = 0.25, color = 'black') + 
  coord_equal(ratio = 1.3) +
  scale_fill_gradientn(colours = c("white", "#e89f00"), values=c(0, 1), breaks = 50*-2:2, 
                       labels =  50*-2:2, limits = c(0, 50), name = chunk_map_text) +
  ylab(chunk_map_y_lab) + theme(axis.title.y = element_blank()) +
  ggtitle(paste0(as.Date(date_1) %>% format("%d-%b-%Y"), ' to ', as.Date(date_3) %>% format("%d-%b-%Y")))

#Plot line change
plot_line_change = line_plot_data %>% 
  ggplot(aes(x = date)) + 
  geom_vline(xintercept = as.Date('2020-03-21'), colour="dark grey", size = 1, linetype = 'dashed') +
  geom_text(aes(x= as.Date('2020-03-21'), label="Restrictions\nImposed", y = -40), colour="grey", angle=90, size = 5) +
  geom_path(aes_string(y = chunk_var), size = 1.8, colour = '#454545') +
  geom_point(aes_string(y = chunk_var), size = 4, colour = '#00acac') + theme_minimal() + xlab('Date') + 
  ylab('Change from\nBaseline (%)') + ylim(min(uk_mobility_data_national[chunk_var]) - 2,
                                           max(uk_mobility_data_national[chunk_var]) + 5) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20, vjust = 5),
        axis.text.y = element_text(size = 18),
        plot.margin = unit(c(1,1,1,1),"cm"))

(fixed_baseline_plot | change_plot) / plot_line_change +  plot_layout(heights = unit(c(35, 4), c('cm', 'cm')), widths = unit(c(40, 12), c('cm', 'cm'))) 

rm(chunk_var, plot_line_change, chunk_data_sp, var_1, var_2, var_3)

## Pharmacy and Groceries

chunk_var = 'grocery_and_pharmacy_percent_change_from_baseline'
chunk_map_text = 'Hello'
chunk_map_y_lab = 'Pharmacy and Groceries'

chunk_data_sp = uk_mobility_data_sp

var_1 = paste0(chunk_var, '_1')
var_2 = paste0(chunk_var, '_2')
var_3 = paste0(chunk_var, '_3')

test = chunk_data_sp@data

chunk_data_sp@data = chunk_data_sp@data %>% 
  select(name, id, second_id, details, date_3, date_2, date_1, contains(chunk_var)) %>% 
  mutate(date_1_to_3 := !!as.name(var_3) - !!as.name(var_1),
         date_2_to_3 := !!as.name(var_3) - !!as.name(var_2),
         date_1_to_2 := !!as.name(var_2) - !!as.name(var_1))                            

#fortify
chunk_data_sp_points = fortify(chunk_data_sp, region = "id")
chunk_data_sp_fort = dplyr::left_join(chunk_data_sp_points, chunk_data_sp@data, by="id")

#plot the first date only
fixed_baseline_plot = chunk_data_sp_fort %>% 
  mutate(var_1 = ifelse(var_1 > 0, 0, var_1)) %>% 
  ggplot_map(aes(y = lat, x = long, group = group)) + geom_polygon(aes_string(fill = var_1), size = 0.25, color = 'black') + coord_equal(ratio = 1.3) +
  scale_fill_gradientn(colours = c("white", "white", "red"), values = c(0, 0.25, 1), 
                       breaks = 50*-2:2, labels =  50*-2:2, limits = c(-100,0), name = chunk_map_text) + 
  ylab(chunk_map_y_lab) + theme(axis.title.y = element_blank()) +
  ggtitle(as.Date(date_1) %>% format("%d-%B-%Y"))

# #plot difference between first and third date
change_plot = chunk_data_sp_fort %>%
  mutate(date_1_to_3 = ifelse(date_1_to_3 < 0, 0, date_1_to_3)) %>% 
  ggplot_map(aes(y = lat, x = long, group = group)) + geom_polygon(aes_string(fill = 'date_1_to_3'), size = 0.25, color = 'black') + 
  coord_equal(ratio = 1.3) +
  scale_fill_gradientn(colours = c("white", "#e89f00"), values=c(0, 1), breaks = 50*-2:2, 
                       labels =  50*-2:2, limits = c(0, 50), name = chunk_map_text) +
  ylab(chunk_map_y_lab) + theme(axis.title.y = element_blank()) +
  ggtitle(paste0(as.Date(date_1) %>% format("%d-%b-%Y"), ' to ', as.Date(date_3) %>% format("%d-%b-%Y")))

#Plot line change
plot_line_change = line_plot_data %>% 
  ggplot(aes(x = date)) + 
  geom_vline(xintercept = as.Date('2020-03-21'), colour="dark grey", size = 1, linetype = 'dashed') +
  geom_text(aes(x= as.Date('2020-03-21'), label="Restrictions\nImposed", y = -20), colour="grey", angle=90, size = 5) +
  geom_path(aes_string(y = chunk_var), size = 1.8, colour = '#454545') +
  geom_point(aes_string(y = chunk_var), size = 4, colour = '#00acac') + theme_minimal() + xlab('Date') + 
  ylab('Change from\nBaseline (%)') + ylim(min(uk_mobility_data_national[chunk_var]) - 2,
                                           max(uk_mobility_data_national[chunk_var]) + 5) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20, vjust = 5),
        axis.text.y = element_text(size = 18),
        plot.margin = unit(c(1,1,1,1),"cm"))

(fixed_baseline_plot | change_plot) / plot_line_change +  plot_layout(heights = unit(c(35, 4), c('cm', 'cm')), widths = unit(c(40, 12), c('cm', 'cm'))) 

rm(chunk_var, plot_line_change, chunk_data_sp, var_1, var_2, var_3)


## Transit
chunk_var = 'transit_stations_percent_change_from_baseline'
chunk_map_text = 'Hello'
chunk_map_y_lab = 'Transit'

chunk_data_sp = uk_mobility_data_sp

var_1 = paste0(chunk_var, '_1')
var_2 = paste0(chunk_var, '_2')
var_3 = paste0(chunk_var, '_3')

test = chunk_data_sp@data

chunk_data_sp@data = chunk_data_sp@data %>% 
  select(name, id, second_id, details, date_3, date_2, date_1, contains(chunk_var)) %>% 
  mutate(date_1_to_3 := !!as.name(var_3) - !!as.name(var_1),
         date_2_to_3 := !!as.name(var_3) - !!as.name(var_2),
         date_1_to_2 := !!as.name(var_2) - !!as.name(var_1))                            

#fortify
chunk_data_sp_points = fortify(chunk_data_sp, region = "id")
chunk_data_sp_fort = dplyr::left_join(chunk_data_sp_points, chunk_data_sp@data, by="id")

#plot the first date only
fixed_baseline_plot = chunk_data_sp_fort %>% 
  mutate(var_1 = ifelse(var_1 > 0, 0, var_1)) %>%
  ggplot_map(aes(y = lat, x = long, group = group)) + geom_polygon(aes_string(fill = var_1), size = 0.25, color = 'black') + coord_equal(ratio = 1.3) +
  scale_fill_gradientn(colours = c("white", "white", "red"), values = c(0, 0.25, 1), 
                       breaks = 50*-2:2, labels =  50*-2:2, limits = c(-100,0), name = chunk_map_text) + 
  ylab(chunk_map_y_lab) + theme(axis.title.y = element_blank()) +
  ggtitle(as.Date(date_1) %>% format("%d-%B-%Y"))

# #plot difference between first and third date
change_plot = chunk_data_sp_fort %>%
  mutate(date_1_to_3 = ifelse(date_1_to_3 < 0, 0, date_1_to_3)) %>% 
  ggplot_map(aes(y = lat, x = long, group = group)) + geom_polygon(aes_string(fill = 'date_1_to_3'), size = 0.25, color = 'black') + 
  coord_equal(ratio = 1.3) +
  scale_fill_gradientn(colours = c("white", "#e89f00"), values=c(0, 1), breaks = 50*-2:2, 
                       labels =  50*-2:2, limits = c(0, 50), name = chunk_map_text) +
  ylab(chunk_map_y_lab) + theme(axis.title.y = element_blank()) +
  ggtitle(paste0(as.Date(date_1) %>% format("%d-%b-%Y"), ' to ', as.Date(date_3) %>% format("%d-%b-%Y")))

#Plot line change
plot_line_change = line_plot_data %>% 
  ggplot(aes(x = date)) + 
  geom_vline(xintercept = as.Date('2020-03-21'), colour="dark grey", size = 1, linetype = 'dashed') +
  geom_text(aes(x= as.Date('2020-03-21'), label="Restrictions\nImposed", y = -35), colour="grey", angle=90, size = 5) +
  geom_path(aes_string(y = chunk_var), size = 1.8, colour = '#454545') +
  geom_point(aes_string(y = chunk_var), size = 4, colour = '#00acac') + theme_minimal() + xlab('Date') + 
  ylab('Change from\nBaseline (%)') + ylim(min(uk_mobility_data_national[chunk_var]) - 2,
                                           max(uk_mobility_data_national[chunk_var]) + 5) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20, vjust = 5),
        axis.text.y = element_text(size = 18),
        plot.margin = unit(c(1,1,1,1),"cm"))

(fixed_baseline_plot | change_plot) / plot_line_change +  plot_layout(heights = unit(c(35, 4), c('cm', 'cm')), widths = unit(c(40, 12), c('cm', 'cm'))) 

rm(chunk_var, plot_line_change, chunk_data_sp, var_1, var_2, var_3)


## Parks

chunk_var = 'parks_percent_change_from_baseline'
chunk_map_text = 'Hello'
chunk_map_y_lab = 'Parks'

chunk_data_sp = uk_mobility_data_sp

var_1 = paste0(chunk_var, '_1')
var_2 = paste0(chunk_var, '_2')
var_3 = paste0(chunk_var, '_3')

test = chunk_data_sp@data

chunk_data_sp@data = chunk_data_sp@data %>% 
  select(name, id, second_id, details, date_3, date_2, date_1, contains(chunk_var)) %>% 
  mutate(date_1_to_3 := !!as.name(var_3) - !!as.name(var_1),
         date_2_to_3 := !!as.name(var_3) - !!as.name(var_2),
         date_1_to_2 := !!as.name(var_2) - !!as.name(var_1))                            

#fortify
chunk_data_sp_points = fortify(chunk_data_sp, region = "id")
chunk_data_sp_fort = dplyr::left_join(chunk_data_sp_points, chunk_data_sp@data, by="id")

#plot the first date only
fixed_baseline_plot = chunk_data_sp_fort %>% 
  ggplot_map(aes(y = lat, x = long, group = group)) + geom_polygon(aes_string(fill = var_1), size = 0.25, color = 'black') + coord_equal(ratio = 1.3) +
  scale_fill_gradientn(colours = c("white", "white", "red"), values = c(0, 0.25, 1), 
                       breaks = 50*-2:2, labels =  50*-2:2, limits = c(-100,0), name = chunk_map_text) + 
  ylab(chunk_map_y_lab) + theme(axis.title.y = element_blank()) +
  ggtitle(as.Date(date_1) %>% format("%d-%B-%Y"))

# #plot difference between first and third date
change_plot = chunk_data_sp_fort %>%
  ggplot_map(aes(y = lat, x = long, group = group)) + geom_polygon(aes_string(fill = 'date_1_to_3'), size = 0.25, color = 'black') + 
  coord_equal(ratio = 1.3) +
  scale_fill_gradientn(colours = c("white", "#e89f00"), values=c(0, 1), breaks = 50*-2:2, 
                       labels =  50*-2:2, limits = c(-50, 50), name = chunk_map_text) +
  ylab(chunk_map_y_lab) + theme(axis.title.y = element_blank()) +
  ggtitle(paste0(as.Date(date_1) %>% format("%d-%b-%Y"), ' to ', as.Date(date_3) %>% format("%d-%b-%Y")))

#Plot line change
plot_line_change = line_plot_data %>% 
  ggplot(aes(x = date)) + 
  geom_vline(xintercept = as.Date('2020-03-21'), colour="dark grey", size = 1, linetype = 'dashed') +
  geom_text(aes(x= as.Date('2020-03-21'), label="Restrictions\nImposed", y = -15), colour="grey", angle=90, size = 5) +
  geom_path(aes_string(y = chunk_var), size = 1.8, colour = '#454545') +
  geom_point(aes_string(y = chunk_var), size = 4, colour = '#00acac') + theme_minimal() + xlab('Date') + 
  ylab('Change from\nBaseline (%)') + ylim(min(uk_mobility_data_national[chunk_var]) - 2,
                                           max(uk_mobility_data_national[chunk_var]) + 5) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20, vjust = 5),
        axis.text.y = element_text(size = 18),
        plot.margin = unit(c(1,1,1,1),"cm"))

(fixed_baseline_plot | change_plot) / plot_line_change +  plot_layout(heights = unit(c(35, 4), c('cm', 'cm')), widths = unit(c(40, 12), c('cm', 'cm'))) 

rm(chunk_var, plot_line_change, chunk_data_sp, var_1, var_2, var_3)


## Workplace

chunk_var = 'workplaces_percent_change_from_baseline'
chunk_map_text = 'Hello'
chunk_map_y_lab = 'Parks'

chunk_data_sp = uk_mobility_data_sp

var_1 = paste0(chunk_var, '_1')
var_2 = paste0(chunk_var, '_2')
var_3 = paste0(chunk_var, '_3')

test = chunk_data_sp@data

chunk_data_sp@data = chunk_data_sp@data %>% 
  select(name, id, second_id, details, date_3, date_2, date_1, contains(chunk_var)) %>% 
  mutate(date_1_to_3 := !!as.name(var_3) - !!as.name(var_1),
         date_2_to_3 := !!as.name(var_3) - !!as.name(var_2),
         date_1_to_2 := !!as.name(var_2) - !!as.name(var_1))                            

#fortify
chunk_data_sp_points = fortify(chunk_data_sp, region = "id")
chunk_data_sp_fort = dplyr::left_join(chunk_data_sp_points, chunk_data_sp@data, by="id")

#plot the first date only
fixed_baseline_plot = chunk_data_sp_fort %>% 
  mutate(var_1 = ifelse(var_1 > 0, 0, var_1)) %>% 
  ggplot_map(aes(y = lat, x = long, group = group)) + geom_polygon(aes_string(fill = var_1), size = 0.25, color = 'black') + coord_equal(ratio = 1.3) +
  scale_fill_gradientn(colours = c("white", "white", "red"), values = c(0, 0.25, 1), 
                       breaks = 50*-2:2, labels =  50*-2:2, limits = c(-100,0), name = chunk_map_text) + 
  ylab(chunk_map_y_lab) + theme(axis.title.y = element_blank()) +
  ggtitle(as.Date(date_1) %>% format("%d-%B-%Y"))

# #plot difference between first and third date
change_plot = chunk_data_sp_fort %>%
  mutate(date_1_to_3 = ifelse(date_1_to_3 < 0, 0, date_1_to_3)) %>% 
  ggplot_map(aes(y = lat, x = long, group = group)) + geom_polygon(aes_string(fill = 'date_1_to_3'), size = 0.25, color = 'black') + 
  coord_equal(ratio = 1.3) +
  scale_fill_gradientn(colours = c("white", "#e89f00"), values=c(0, 1), breaks = 50*-2:2, 
                       labels =  50*-2:2, limits = c(0, 50), name = chunk_map_text) +
  ylab(chunk_map_y_lab) + theme(axis.title.y = element_blank()) +
  ggtitle(paste0(as.Date(date_1) %>% format("%d-%b-%Y"), ' to ', as.Date(date_3) %>% format("%d-%b-%Y")))

#Plot line change
plot_line_change = line_plot_data %>% 
  ggplot(aes(x = date)) + 
  geom_vline(xintercept = as.Date('2020-03-21'), colour="dark grey", size = 1, linetype = 'dashed') +
  geom_text(aes(x= as.Date('2020-03-21'), label="Restrictions\nImposed", y = -35), colour="grey", angle=90, size = 5) +
  geom_path(aes_string(y = chunk_var), size = 1.8, colour = '#454545') +
  geom_point(aes_string(y = chunk_var), size = 4, colour = '#00acac') + theme_minimal() + xlab('Date') + 
  ylab('Change from\nBaseline (%)') + ylim(min(uk_mobility_data_national[chunk_var]) - 2,
                                           max(uk_mobility_data_national[chunk_var]) + 5) + 
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(size = 20),
        axis.title.y = element_text(size = 20, vjust = 5),
        axis.text.y = element_text(size = 18),
        plot.margin = unit(c(1,1,1,1),"cm"))

(fixed_baseline_plot | change_plot) / plot_line_change +  plot_layout(heights = unit(c(35, 4), c('cm', 'cm')), widths = unit(c(40, 12), c('cm', 'cm'))) 

rm(chunk_var, plot_line_change, chunk_data_sp, var_1, var_2, var_3)

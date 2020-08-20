
library(shinydashboard)
library(shinydashboardPlus)
library(shiny)
library(shinycustomloader)
library(lubridate)
library(tidyverse)


header_title = tags$a('COVID-19 lockdown mapping', icon('virus'), 
                      height = "100%",
                      width = "100%",
                      align = "center",
                      tags$style("@import url(https://use.fontawesome.com/releases/v5.13.0/css/all.css);"),
                      tags$link(href = 'https://fonts.googleapis.com/css?family=Bellota+Text:300&display=swap',
                                rel="stylesheet"))




#UI
ui <- dashboardPage(title="COVID-19 lockdown mapping app",
                    dashboardHeader(title = header_title, titleWidth = "100%"), # remove titleWidth
                    dashboardSidebar(dateInput("date", "Select Date", value = "2020-03-23"),
                                     #dateInput("date_2", "Select Date to compare", value = Sys.Date()-7),
                                     selectInput(inputId = 'column_to_display',
                                                 label = 'Select Data',
                                                 choices = c("retail and recreation" = "retail_and_recreation_percent_change_from_baseline",
                                                             "grocery and pharmacy" = "grocery_and_pharmacy_percent_change_from_baseline",
                                                             "parks" = "parks_percent_change_from_baseline",
                                                             "transit stations" = "transit_stations_percent_change_from_baseline",
                                                             "workplaces" = "workplaces_percent_change_from_baseline",
                                                             "residential" = "residential_percent_change_from_baseline")),
                                     # actionButton(
                                     #     inputId = "Run",
                                     #     label = "Submit",
                                     #     style = "width: 160px; 
                                     #             height: 60px;
                                     #             background-size: cover; 
                                     #             background-position: center;
                                     #             background-repeat: no-repeat;
                                     #             border-radius: 10px;
                                     #             border-width: 3px;"),
                                     # actionBttn(
                                     #     inputId = "Run",
                                     #     label = "Submit",
                                     #     style = "unite", 
                                     #     color = "default"
                                     # ),
                                     actionButton("Run", "Submit", 
                                                  #icon("paper-plane"), 
                                                  style="color: #000000; 
                                                  background-color: #ffffff; 
                                                  border-color: #676767"),
                                     disable = F, 
                                     collapsed = F),
                    dashboardBody(
                        tags$head(tags$style(HTML('.content-wrapper { overflow: auto; }')),
                                  tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                                  tags$style(".small-box.bg-aqua { background-color: #21aacc !important; color: #ffffff !important;}
                          .small-box.bg-blue { background-color: #1492ba !important; color: #ffffff !important;}
                         .small-box.bg-maroon { background-color: #82d8ed !important; color: #292929 !important;}
                         .box{overflow-y: auto !important;}")
                        ),
                        #conditionalPanel(condition = "input.Run",
                                         #plotOutput("world_plot")#,
                        fluidRow(
                            # box(
                            #     title = "", 
                            #     status = NULL,
                            #     solidHeader = TRUE,
                            #     collapsible = FALSE,
                            #     width = 12,
                                column(
                                    width = 10,
                                    offset = 2,
                                         withLoader(plotOutput(outputId = "world_plot",
                                                               width = "80%",
                                                               height = "500px"
                                                               ),
                                                    type="html", loader="loader6")
                                )
                        )
                        #)
                        #plotOutput("world_plot_2")
                    )
)


# Read GEOJson world file
library(tidyverse)
world_map = geojsonio::geojson_read('world_maps/countries.geojson', what = 'sp')

#Linkage CSVs
iso_code_conv = read_csv('world_maps/iso_code_conversion.csv')

#Add the ISO2 code for lookup
world_map@data = world_map@data %>%
    left_join(iso_code_conv %>% select(iso2, iso3), by = c('ISO_A3' = 'iso3'))

# DD add global_mobility_data
load("data/mobility_data.rda")
rm(uk_mobility_data, uk_mobility_data_national, uk_mobility_data_subnational)



# Server
server <- function(input, output) {
    
    
        example_data_to_merge <- eventReactive(input$Run, {
            global_mobility_data %>%
            filter(is.na(sub_region_1) & date == input$date) #as.Date('2020-07-24')
        })
        
        fill_map <- eventReactive(input$Run, {
            input$column_to_display
        })
        
        output$world_plot <- renderPlot({
        
        #plot
        world_map_fortified = fortify(world_map, region = 'iso2') %>%
            left_join(example_data_to_merge(), by = c('id' = 'country_region_code'))

        world_map_fortified %>%
            ggplot(aes(y = lat, x = long, group = group)) +
            geom_polygon(aes_string(fill = fill_map()), size = 0.25, color = 'black')
    })
   
    
    
    # output$world_plot_2 <- renderPlot({
    #     #Now merge in
    #     example_data_to_merge = global_mobility_data %>%
    #         filter(is.na(sub_region_1) & date == input$date_2) #as.Date('2020-07-24')
    #     
    #     #plot
    #     world_map_fortified = fortify(world_map, region = 'iso2') %>%
    #         left_join(example_data_to_merge, by = c('id' = 'country_region_code'))
    #     
    #     world_map_fortified %>%
    #         ggplot(aes(y = lat, x = long, group = group)) +
    #         geom_polygon(aes(fill = retail_and_recreation_percent_change_from_baseline), size = 0.25, color = 'black')
    # })
    # 

}

# Run the application 
shinyApp(ui = ui, server = server)

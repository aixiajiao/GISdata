packages <- c(
  'dplyr','DT',
  'janitor',
  'ggplot2','gganimate','gifski',
  'rgdal',
  'sf','sp','stringr','spatstat','spdep','SpatialEpi','shiny','shinythemes',
  'tidyverse','tmap','transformr',
  'openxlsx','INLA','reshape2','leaflet',
  'htmlwidgets','Hmisc'
)
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])}
invisible(lapply(packages, library, character.only = TRUE))
spatial.rds <- readRDS("temp_model.rds")
spatial.rds$relative_risk <- round(spatial.rds$relative_risk,2)
ui <- fluidPage(
  # set the theme (see https://rstudio.github.io/shinythemes/)
  theme = shinytheme("cerulean"),
  # Title for web application
  titlePanel("Toronto, Canada: Severe Road Accident Risk Maps (2011-2016)"),
  # Including a sidebar - use sidebarLayout() function
  sidebarLayout(
    sidebarPanel(
      # Use selectInput() to capture the year values 2015, 2016, ..., 2020 to make maps reactive on years
      selectInput(inputId = "Years", label = "Select a year:", choices = c(2011,2012,2013,2014,2015,2016))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Posterior Risk Estimates", leafletOutput("CompactLayeredMapOutput", height = "90vh")),
        tabPanel("KSI (Plots)",plotOutput("HistogramOutput", height = "90vh")),
        tabPanel("Data Viewer", DT::dataTableOutput("DataViewerOutput", height = "auto"))
      )
    )
  )
)

# Part 2: Define the server logic
server <- function(input, output) {
  # create a reactive function to make all outputs in mainPanel change with the years
  year_react <- reactive({
    yr <- spatial.rds[spatial.rds$year == input$Years,]
    return(yr)
  })
  # year_react() apply this function in place of spatial.rds
  output$CompactLayeredMapOutput <- renderLeaflet({
    # risk categories for labels
    RiskCategorylist <- c("0.01 to 0.40", "0.40 to 0.55", "0.55 to 0.65",
                          "0.65 to 0.75", "0.75 to 0.85", "0.85 to 0.99",
                          "1.00 (null value)", ">1.00 to 1.15", "1.15 to 1.35",
                          "1.30 to 1.60", "1.60 to 2.00", "2.00 to 2.50",
                          "Above 3")
    #colours for the above categories
    RRPalette <- c("#33a6fe","#65bafe","#98cffe",
                   "#cbe6fe","#dfeffe","#fef9f9",
                   "#fed5d5","#feb1b1","#fe8e8e",
                   "#fe6a6a","#fe4646","#fe2424",
                   "#fe0000")
    # Significance label categories and colour scheme
    SigCategorylist <- c("Significantly low", "Not Significant", "Significantly high")
    SigPalette <- c("#33a6fe", "white", "#fe0000")
    ProbCategorylist <- c("<0.01", "0.01-0.09", "0.10-0.19",
                          "0.20-0.29", "0.30-0.39", "0.40-0.49",
                          "0.50-0.59", "0.60-0.69", "0.70-0.79",
                          "0.80-0.89", "0.90-1.00")
    
    # create one table containing the year, KSI, RelativeRiskCat, Signficance, probability.Here, apply function year_react() here to make dynamic
    `Relative Risk` <- year_react()[, c(4,1,14)]
    `Significance` <- year_react()[, c(4,1,15)]
    `Exceedance Probabilities` <- year_react()[, c(4,1,17)]
    `KSI per thousand` <- year_react()[,c(4,1,18)]
    `Neighbourhoods` <- read_sf('shp.geojson')
    
    # Create tmap with all maps in one object
    InteractiveMap <- tm_shape(`Significance`) +
      tm_fill("Significance", style = "cat", title = "Significance Categories", palette = SigPalette, labels = SigCategorylist) +
      tm_shape(`Relative Risk`) + # map of relative risk
      tm_polygons("RelativeRiskCat", style = "cat", title = "Relavtive Risk", palette = RRPalette, labels = RiskCategorylist, border.alpha = 0) +
      tm_shape(`Exceedance Probabilities`) + # map of Probability
      tm_polygons("ProbCat", style = "cat", title = "Exceedance Probabilities", palette = "plasma", labels = ProbCategorylist, border.alpha = 0) +
      tm_shape(`KSI per thousand`) + # map of KSI per thousand
      tm_polygons("ksi_per_thousand", style = "pretty", title = "KSI per thousand", palette = "Reds", border.alpha = 0) +
      tm_shape(`Neighbourhoods`) + tm_polygons(alpha = 0, border.alpha = 1, border.col = "black")
    
    # Visualise them as a compact map through leaflet
    InteractiveMap <- tmap_leaflet(InteractiveMap) %>%
      addProviderTiles(providers$OpenStreetMap, group="Street") %>% 
      addProviderTiles(providers$CartoDB.DarkMatter, group="Dark") %>%
      addLayersControl(baseGroups = c("Street", "Dark"), overlayGroups=c("Neighbourhoods", "KSI per thousand","Relative Risk", "Significance","Exceedance Probabilities"), position="topleft", options = layersControlOptions(collapsed = FALSE))
    
    # Resulting leaflet map
    InteractiveMap
  })
  
  # Create histogram for second panel
  output$HistogramOutput <- renderPlot({
    hist(year_react()$ksi, 
         xlab = paste("Number of recorded cases in ", input$Years, sep= ""),
         ylab = "Frequency", main=paste("Distribution of Road Accidents in ", input$Years, sep = ""), breaks = 20)
  })
  # Create data viewer for third panel
  output$DataViewerOutput <- DT::renderDataTable({
    DT::datatable(year_react()[, c("hood","hood_id", "year", "ksi",'relative_risk')], rownames = FALSE, options = list(sScrollY = '75vh', scrollCollapse = TRUE), extensions = list("Scroller"))
  })
}
shinyApp(ui = ui, server = server)


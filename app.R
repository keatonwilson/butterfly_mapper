#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(tidyverse)
library(rgdal)
library(rmapshaper)
library(raster)
library(stringr)
library(shinythemes)
library(DT)
library(maptools)
library(maps)
library(sf)

# Setting up data outside of ui and server
occ_data = read_csv("./data/species_obs.csv") %>%
    mutate(provider = as.factor(provider))

unique_species = unique(occ_data$scientific_name)

checklist = read_csv("./data/checklist.csv") %>%
    mutate(species = str_replace(species, "_", " ") %>% str_to_sentence())


states = st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))

centroids = states %>%
    st_centroid()

coords = data.frame(st_coordinates(centroids)) %>%
    rename(long = X, lat = Y)

state_id = centroids$ID

centroids_df = bind_cols(state = state_id, coords) %>%
    mutate(state = str_to_sentence(state))

#species to focal state 
spec_focal_state = read_csv("./data/species_state_mapping.csv")

# # National Forest Shapefile
# nfs = readOGR("./data/geo_data/S_USA.AdministrativeForest.shp") 
# nfs_small = subset(nfs, REGION %in% c("01", "06", "04", "05", "03", "02"))
# 
# nfs_smaller = ms_simplify(nfs_small, sys = TRUE)
# saveRDS(nfs_smaller, "./data/nfs_small.rds")

nfs = readRDS("./data/nfs_small.rds")

# Load in threshold maps for all species
# START HERE


# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("superhero"),

    # Application title
    titlePanel("Butterfly Mapper"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("species",
                        "Select a Species:",
                        choices = unique_species, 
                        selected = unique_species[[1]]),
            h4("Species Checklist"),
            DT::dataTableOutput("checklist")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           leafletOutput("species_map", height = 800)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # focal centroid
    focal_centroid_df = reactive({
        focal_state = spec_focal_state %>% filter(Species == input$species) %>%
            pull(State)
        centroid = centroids_df %>% filter(state == focal_state)
        return(centroid)
    })

    
    
    # filtering data
    data_to_plot = reactive({occ_data %>%
        dplyr::filter(scientific_name == input$species)
    })
    
    # generating filename to load raster from
    filename = reactive({
        paste0("./data/thresh_maps_rasters/", 
               str_to_lower(input$species) %>%
            str_replace(" ", "_") , 
            "_all_thresh.tif"
        )
    })
    
    # loading raster file dynamically
    rast = reactive({
        raster(filename())
    })
    
    # loading subset of checklist
    checklist_sub = reactive({
        checklist %>% filter(species == input$species)
    })
    
    # palette
    pal = colorFactor(c("navy", "red"), domain = c("iNat", "as"))
    
    #center for plotting
    
    # generating leaflet
    output$species_map <- renderLeaflet({
        leaflet(data_to_plot()) %>%
            setView(lat = focal_centroid_df()$lat,
                    lng = focal_centroid_df()$long, 
                    zoom = 6) %>%
            addTiles() %>%
            addPolygons(data = nfs, color = "#FFFF99", 
                        weight = 1, smoothFactor = 1,
                        opacity = 0.3, fillOpacity = 0.5, 
                        label = nfs$FORESTNAME, 
                        highlightOptions = highlightOptions(color = "white", 
                                                            weight = 1)) %>%
            addRasterImage(rast(), colors = "#87CEEB",
                           opacity = 0.8) %>%
            addCircleMarkers(~longitude, ~latitude, radius = 2, 
                             color = ~pal(provider), 
                             stroke = FALSE,
                             fillOpacity = 0.6) %>%
            addLegend(colors = c("navy", "red"), title = "Occurrence Data", 
                      labels = c("Adventure Scientists", "iNaturalist")) %>%
            addLegend(colors = c("#FFFF99", "#87CEEB"), title = "Layers", 
                      labels = c("National Forest", "Predicted Species Occurrence"))
    })
    
    # generating table checklist
    output$checklist = DT::renderDataTable(checklist_sub(), 
                                           colnames = c("Forest Name", 
                                                        "Species", 
                                                        "Predicted Presence"))
    
}

# Run the application 
shinyApp(ui = ui, server = server)

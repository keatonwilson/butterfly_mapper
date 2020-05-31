# converting the grid data to a polygon to be able to plot in leaflet
# Keaton Wilson
# keatonwilson@me.com
# 2020-05-28

# packages
library(tidyverse)
library(inlmisc)
library(spatial)
library(sp)
library(raster)
library(leaflet)
library(stringr)

test = readRDS("./data/thresh_maps/Aglais milberti_all_thresh.rds")
test = test %>%
  dplyr::select(-value) %>%
  mutate(values = 1)


rast = raster()
extent(rast) = extent(test)
rast2 = rasterize(test %>% dplyr::select(x, y), rast, (test$values), background = 0)
        
files = list.files("./data/thresh_maps/", full.names = TRUE)

# Rasters for plotting
for(i in 1:length(files)){
  tmp = readRDS(files[i])
  tmp = tmp %>%
    dplyr::select(-value) %>%
    mutate(values = 1)
  rast = raster()
  extent(rast) = extent(tmp)
  rast2 = rasterize(tmp %>% dplyr::select(x, y), rast, tmp$values)
  filename = paste0("./data/thresh_maps_rasters/",
                    str_remove(files[i], "./data/thresh_maps//") %>%
                      str_remove(".rds") %>%
                      paste0(".tif") %>%
                      str_replace(" ", "_") %>%
                      str_to_lower()
             )
  writeRaster(rast2, filename = filename, overwrite=TRUE)
  print(filename)
}

# Rasters for checklists
for(i in 1:length(files)){
  tmp = readRDS(files[i])
  tmp = tmp %>%
    dplyr::select(-value) %>%
    mutate(values = 1)
  rast = raster()
  extent(rast) = extent(tmp)
  rast2 = rasterize(tmp %>% dplyr::select(x, y), rast, tmp$values, 
                    background = 0)
  filename = paste0("./data/thresh_maps_rasters/checklist_rasters/",
                    str_remove(files[i], "./data/thresh_maps//") %>%
                      str_remove(".rds") %>%
                      paste0(".tif") %>%
                      str_replace(" ", "_") %>%
                      str_to_lower()
  )
  writeRaster(rast2, filename = filename, overwrite=TRUE)
  print(filename)
}

# Raster plotting to test
test_rast = raster("./data/thresh_maps_rasters/aglais_milberti_all_thresh.tif", 
                   values = TRUE)


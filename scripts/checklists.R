# Checklist builder
# Keaton Wilson
# keatonwilson@me.com
# 2020-05-29

# packages
library(tidyverse)
library(raster)
library(rgeos)

#test raster
test_rast = raster("./data/thresh_maps_rasters/checklist_rasters/aglais_milberti_all_thresh.tif")
# NFS polygons
nfs = readRDS("./data/nfs_small.rds")

#same proj
crs(test_rast) = crs(nfs)

# intersection
angeles = subset(nfs, nfs$FORESTNAME == "Angeles National Forest")
intersect = raster::intersect(test_rast, angeles)

# building a checklist for one species for all forests
forest_list = split(nfs, nfs$FORESTNAME, drop = TRUE)

# getting all species threshold file names
files = list.files("./data/thresh_maps_rasters/checklist_rasters/", 
                   full.names = TRUE)
to_run = files[str_detect(files, "all")]

df_out = data.frame(forest_name = c(), 
                    species = c(), 
                    present = c())

for(j in 1:length(to_run)){ 
  spec_rast = raster(to_run[j])
  crs(spec_rast) = crs(nfs)
  species_name = str_remove(to_run[j], 
                            "./data/thresh_maps_rasters/checklist_rasters//") %>%
    str_remove("_all_thresh.tif")
  
  for(i in 1:length(forest_list)){
    check_intersect = try(raster::intersect(spec_rast, forest_list[[i]]))
    df = data.frame(forest_name = names(forest_list[i]), 
                    species = species_name, 
                    present = ifelse(class(check_intersect) == "try-error",
                                     FALSE,
                                     ifelse(sum(check_intersect@data@values) > 0, 
                                     TRUE, 
                                     FALSE)))
    df_out = rbind(df_out, df)
    print(paste(species_name, names(forest_list[i])))
  }
}  

write_csv(df_out, "checklist.csv")

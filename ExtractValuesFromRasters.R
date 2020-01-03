# Extract values from rasterstack to points

rm(list=ls())

library(raster)
library(rgdal)
library(mapview)
library(sf)
library(tidyverse)

region <- "qcs"

# 1: Create a Raster stack or Raster brick of your raster files 
setwd("T:/Benthic_Habitat_Mapping/Data/Environmental_layers")
rasfiles<-list.files(getwd(), pattern = "(*.)tif$",recursive=T)
ras <- raster::stack(rasfiles)

#2:  Read point data, and convert them into spatial points data frame.
setwd("C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep")

dsn <- "C:/Users/daviessa/Documents/R/PROJECTS_OTHERS/SpatializeDiveTransects/ByDepthCat"
sp <- readOGR(dsn=dsn, layer="All_SpatPts_DepthCat", stringsAsFactors = F)
mapview(sp)

# Build extent polygon from rasterstack
e <- extent(ras)
poly_e <- as(e,"SpatialPolygons")
class(poly_e)
proj4string(poly_e) <- CRS("+init=epsg:3005")
crs(poly_e)
mapview(poly_e)

# Check projection
sp <- spTransform(sp, CRS("+init=epsg:3005"))
st_crs(poly_e)==st_crs(sp)

projection(poly_e)
projection(sp)

# Points in a polygon
inPts <- raster::intersect(sp, poly_e)
mapview(poly_e)
mapview(inPts)

# 3: Extract raster value by points
rasValue <- raster::extract(ras, inPts)

# 4: Combine raster values with point, check results, & save as a CSV file.
new.sp <- cbind(inPts,rasValue)
head(new.sp, 3)

env.nmes <- as.data.frame(names(new.sp@data))
fname <- paste0(dsn,"/envNames.csv")
write_csv(env.nmes, fname)

filename <- paste0(region,"__SpatPts_wEnv")
writeOGR(new.sp, dsn=dsn, overwrite_layer = T,
         layer=filename, driver="ESRI Shapefile")

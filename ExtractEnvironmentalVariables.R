#######################################################################################################################
# Extract and attach enviromental data
# 
# Objective:  Get environmental variables at sampling points and attach to dataframe of species observations
#
# Author:     Katie Gale
#             Katie.Gale@dfo-mpo.gc.ca
#
# Edited:     Sarah Davies
# Date:       March, 2020
######################################################################################################################

library(rgdal)
library(raster)
library(tidyverse)

setwd("..")
dir <- getwd()

################################
# Import rasters for analysis  #
################################
setwd("C:/Users/daviessa/Documents/CURRENT PROJECTS/Environmental Layers/NCC/Rasters/")
rasfiles <- list.files(getwd(), pattern = "(*.)tif$",recursive=T)
rasfiles
env <- c("bathy.tif","BBPI.tif","fetchSum.tif","salt_range.tif","slope.tif","tidalcurr_meanSummer.tif")
ras <- stack(env)


# Bring in csv with 1 point per sample (site, transect, quadrat, etc)   
#####################################################################
setwd("C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep/Data/SHP")
sp <- readOGR("spatializedPts_byDepthCat.shp")  

# Extract enviromental data from rasters to points
##################################################
spEnv <- data.frame(sp,raster::extract(ras, sp)) # add .parameters to each record
summary(is.na(spEnv)) # check how many points are NA

spEnv_complete <- spEnv[complete.cases(spEnv[!names(spEnv) %in% names(sp)]),] # remove cases where environmental data aren't available
summary(is.na(spEnv_complete)) 

write.csv(spEnv_complete,"C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep/Data/UpdatedObservations/TransDepth_wEnv.csv", row.names=F)

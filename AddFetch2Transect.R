#######################################################################################################################
# Benthic Habitat Mapping Dive Survey Data Summary --- Build In Situ Observations for depth, slope, and substrate
# 
# Objective:  Ad Fetch value to each transect start position
#
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       October 29, 2019
######################################################################################################################
 
# TO DO:
# Add some code to export all final records with distance to nearest fetch point greater than 100 m and plot
# Check projections of input data, fetch points, output data

# Start fresh
rm(list=ls())

# Check which version of R is being used and reset if necessary
Sys.getenv("R_ARCH") 

# Load required packages 
if (!require('sp')) install.packages('sp'); library('sp')
if (!require('rgdal')) install.packages('rgdal'); library('rgdal')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')

cat("Calculating transect lat/lon...","\n")

# Go to parent directory
setwd("C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep")
outdir <- "./Data/UpdatedObservations/"

#----------------------------------------------------------------------------#
### Load survey data
quads <- read.csv("./Data/ExtractedData/Quadrat.csv", header=T)
hdrs <- read.csv( "./Data/ExtractedData/Headers.csv", header=T, sep="," )
hdrs <- dplyr::select(hdrs,HKey,Survey,Year,Month,Transect,LatDegStart,LatMinStart,LonDegStart,LonMinStart,LatDegEnd,LatMinEnd,LonDegEnd,LonMinEnd)
quads <- dplyr::select(quads, HKey,Quadrat,DepthCat, CorDepthM)
quads$TransDepth <- paste0(quads$HKey,"_",quads$DepthCat)

# Join two together
dat <- dplyr::left_join(quads, hdrs, by="HKey")
head(dat)

# Calculate x,y
dat$LatStart<-(dat$LatDegStart+(dat$LatMinStart/60))
dat$LatEnd<-(dat$LatDegEnd+(dat$LatMinEnd/60))
dat$LonStart<-(-1)*abs(dat$LonDegStart+(dat$LonMinStart/60))
dat$LonEnd<-(-1)*abs(dat$LonDegEnd+(dat$LonMinEnd/60))

# remove NAs in lat and lon
dat <- dat[complete.cases(dat[, c("LatStart","LatEnd","LonStart","LonEnd")]),]
# remove duplicate IDs - should be only one record per transect
trans.coords <- dplyr::select(dat, TransDepth, HKey,DepthCat,LatStart,LatEnd,LonStart,LonEnd)
trans.coords <- trans.coords[!duplicated(trans.coords$TransDepth),] 
# reorder columns

head(trans.coords)

# plots in the middle of Hecate Strait - needs to be removed from the database
trans.coords <- dplyr::filter(trans.coords, HKey!='1425')

cat("Adding Fetch values to transect start position...","\n")

# Read in transects and fetch spatial file
#=========================================
#df <- read.csv('./Data/UpdatedObservations/Transect.coords.csv') # WGS
df <- trans.coords
fetch <- readOGR(dsn = "F:/GIS/Fetch", layer = "Fetch_AllRegions") # NAD 83
proj4string(fetch)

# Promote transect file to SpatialPointsDataFrame
spdf <- df
coordinates(spdf) <- c("LonStart","LatStart")
# Geographical projection, WGS
crs.geo <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ") 
# Define projection
proj4string(spdf) <- crs.geo

# Reproject fetch to match transect point
fetch.nad <- spTransform(fetch, proj4string(spdf))

# Plot layers
plot(fetch.nad)
points(spdf, col="red", pch='+')

# Assign closest fetch value to each TransDepth key
#==================================================
# Define vectors for use in loop
closestSiteVec <- vector(mode = "numeric",length = nrow(spdf))
minDistVec     <- vector(mode = "numeric",length = nrow(spdf))

# Get vector index of the closest fetch value to each transect start location
for (i in 1 : nrow(spdf))
{
  distVec <- spDistsN1(fetch.nad,spdf[i,],longlat = TRUE)
  minDistVec[i] <- min(distVec)
  closestSiteVec[i] <- which.min(distVec)
}

# Create an assignment table
PointAssignFetch <- as(fetch.nad[closestSiteVec,]$Sum_Fetch,"numeric")
FinalTable = data.frame(coordinates(spdf),spdf$TransDepth,
                        closestSiteVec,minDistVec,PointAssignFetch)
head(FinalTable)

# Check distance between two points
#==================================
hist(FinalTable$minDistVec, main="Distance from Transect to Nearest Fetch Point",
     xlab="Distance (km)", las=1, col="blue")

# Output updated csv file
#========================
FinalTable <- dplyr::select(FinalTable, spdf.TransDepth, PointAssignFetch)
colnames(FinalTable) <- c("TransDepth","Sum_Fetch")
new.df <- dplyr::left_join(df, FinalTable, by="TransDepth")
write.csv(new.df, file=paste0(outdir,"Trans.coords.fetch.csv"), row.names = F)

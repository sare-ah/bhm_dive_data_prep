#######################################################################################################################
# Benthic Habitat Mapping Dive Survey Sites, joined to depth bin locations
# 
# Objective:  Join sample records to "spatialized points" from bottom patches (i.e., coordinates for individual depth bins, not just sites)
#
# Author:     Katie Gale
#             Katie.Gale@dfo-mpo.gc.ca
#             250-363-6411
# Date:       August 3, 2018
######################################################################################################################

# Based off of old scripts NearshoreRichness.R and PrepareSpatialPoints.R

library(reshape)
library(foreign)
library(rgdal)

setwd("E:/Documents/Projects/WorldClass_DiveSurveys/RCP/bhm_dive_data_prep/Data/")

#Import data, join depth category,  and add SourceKey to join to spatial file
specobs<-read.csv("UpdatedObservations/SpeciesObs_updated.csv")
specobs<-specobs[,c(-1,-2)]
specobs$HKey_Quad<-paste(specobs$HKey, specobs$Quadrat, sep="_")

quadrat<-read.csv("ExtractedData/quadrat.csv")
quadrat$HKey_Quad<-paste(quadrat$HKey, quadrat$Quadrat, sep="_")
quadrat<-quadrat[,names(quadrat) %in% c("HKey_Quad", "DepthCat")]

#join depth category to each quadrat
length(unique(specobs$HKey_Quad)) #10055
length(unique(quadrat$HKey_Quad)) #11097 (probably because there are some quadrats without species)

specrec<-merge(specobs, quadrat, by="HKey_Quad", all.x=T, all.y=F)

#Add identifier to join to spatial points
specrec$SourceKey<-paste("S16",  specrec$HKey, specrec$DepthCat, sep="_")

#Bring in spatialized points files for both north central coast and Haida Gwaii
spatpoint_NCC<-readOGR("E:/Documents/Projects/WorldClass_DiveSurveys/WCTSS_Dive_Db/SpatializedPoints/NCC/S11_FinalSpatializedPointsAll.shp")
spatpoint_NCC<-spTransform(spatpoint_NCC, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
spatpoint_NCC$Lon<-coordinates(spatpoint_NCC)[,1]
spatpoint_NCC$Lat<-coordinates(spatpoint_NCC)[,2]
spatpoint_NCC$area<-"NCC"

spatpoint_HG<-readOGR(dsn = "E:/Documents/Projects/WorldClass_DiveSurveys/WCTSS_Dive_Db/SpatializedPoints/HaidaGwaii/ShellSpatialize_HG_25Apr_2155.gdb",layer="S11_FinalPointsAll")
spatpoint_HG=spTransform(spatpoint_HG, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
spatpoint_HG$Lon<-coordinates(spatpoint_HG)[,1]
spatpoint_HG$Lat<-coordinates(spatpoint_HG)[,2]
spatpoint_HG$area<-"HG"

#Get fields in common and merge spatial files
common<-c("fcode", "SourceKey","Lat","Lon","BoP1","BoP2", "area")
spatpoint<-rbind.data.frame(spatpoint_HG@data[,names(spatpoint_HG) %in% common], spatpoint_NCC@data[,names(spatpoint_NCC) %in% common])

#Get just the survey number and limit to the World Class surveys (S16) 
spatpoint$surv<-strtrim(spatpoint$SourceKey,3)
spatpoint<-spatpoint[spatpoint$surv=="S16",] 

#Join species records to spatial points 
spatWithSpp<-merge(specrec, spatpoint, by="SourceKey", all.x=T, all.y=F)
nrow(specrec) #85916
nrow(spatWithSpp) #85916

#Check for problems
length(unique(spatWithSpp$SourceKey[is.na(spatWithSpp$Lat)]))#60 depth bins (1111 records) are missing spatial points. This is likely due to errors in spatial point creation.

length(unique(specrec$SourceKey)) #The species data has 2856 depth bins
length(unique(spatpoint$SourceKey)) #The spatialized points have 2811 depth bins of WC dives
#So there are maybe 47 (2858-2811) WC depth bins that are not represented in the spatialized points. Might be sites with no species data. 


#convert to shapefile and write
spatWithSpp_shp<-subset(spatWithSpp[!is.na(spatWithSpp$Lon),])
coordinates(spatWithSpp_shp)<-~Lon+Lat
proj4string(spatWithSpp_shp)<-CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
names(spatWithSpp_shp)[7]<-"SpCode"
writeOGR(spatWithSpp_shp, "./SpatialPointsWithSpecies", layer="SpatialPointsWithSpecies", driver="ESRI Shapefile", overwrite=T )

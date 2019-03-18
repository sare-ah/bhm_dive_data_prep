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

setwd("D:/Documents/Projects/WorldClass_DiveSurveys/RCP")

#Import Headers to attach survey name/date
header<-read.csv("./Data/ExtractedData/Headers.csv")
quadrat<-read.csv("./Data/ExtractedData/Quadrat.csv")

header<-header[,names(header) %in% c("HKey", "Survey")]
quadrat$TransDepth<-paste(quadrat$HKey,quadrat$DepthCat, sep="_")
quadrat<-quadrat[,names(quadrat) %in% c("HKey","TransDepth")]

info<-unique(merge(header, quadrat, by="HKey"))

#Import species observation data, join depth category,  and add SourceKey to join to spatial file
inv<-read.csv("./Data/SpeciesBy_matrices/InvertsByDepthCategory.csv")
algae<-read.csv("./Data/SpeciesBy_matrices/AlgaeByDepthCategory.csv")
names(inv)[2:ncol(inv)]<-paste0("I_", names(inv)[2:ncol(inv)])
names(algae)[2:ncol(algae)]<-paste0("A_", names(algae)[2:ncol(algae)])

#Combine algae and inverts into a single dataframe. Can be separated out later if wanted.
summary(as.character(inv$TransDepth)==as.character(algae$TransDepth)) #false
specrec<-merge(inv, algae, by="TransDepth", all.x=T, all.y=T)
specrec$SourceKey<-paste0("S16_",specrec$TransDepth)
specrec<-merge(specrec, info[,2:3], by="TransDepth")

#Format Survey Names to be better integrated into analysis. Special characters and spaces make it problematic.
specrec$Survey<-gsub("PAC 20", "P", specrec$Survey)
specrec$Survey<-gsub("-|-0", "", specrec$Survey)

#Replace NAs (not recorded in that transect) with 0s
specrec[is.na(specrec)]<-0

#Bring in spatialized points files for both north central coast and Haida Gwaii
spatpoint_NCC<-readOGR("D:/Documents/Projects/WorldClass_DiveSurveys/WCTSS_Dive_Db/SpatializedPoints/NCC/S11_FinalSpatializedPointsAll.shp")
spatpoint_NCC<-spTransform(spatpoint_NCC, CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"))
spatpoint_NCC$Lon<-coordinates(spatpoint_NCC)[,1]
spatpoint_NCC$Lat<-coordinates(spatpoint_NCC)[,2]
spatpoint_NCC$area<-"NCC"

spatpoint_HG<-readOGR(dsn = "D:/Documents/Projects/WorldClass_DiveSurveys/WCTSS_Dive_Db/SpatializedPoints/HaidaGwaii/ShellSpatialize_HG_25Apr_2155.gdb",layer="S11_FinalPointsAll")
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
nrow(specrec) #2858
nrow(spatWithSpp) #2858

#Check for problems
length(unique(spatWithSpp$SourceKey[is.na(spatWithSpp$Lat)]))#60 depth bins are missing spatial points. This is likely due to errors in spatial point creation.

length(unique(specrec$SourceKey)) #The species data has 2858 depth bins
length(unique(spatpoint$SourceKey)) #The spatialized points have 2811 depth bins of WC dives
#So there are maybe 47 (2858-2811) WC depth bins that are not represented in the spatialized points. Might be sites with no species data. 

#convert to shapefile and write
spatWithSpp_shp<-subset(spatWithSpp[!is.na(spatWithSpp$Lon),])
coordinates(spatWithSpp_shp)<-~Lon+Lat
proj4string(spatWithSpp_shp)<-CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
writeOGR(spatWithSpp_shp, "./Data/SpatialPointsWithSpecies", layer="SpatialPointsWithSpecies", driver="ESRI Shapefile", overwrite=T )

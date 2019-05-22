#######################################################################################################################
# Benthic Habitat Mapping --- Append environmental data to spatial points
# 
# Objective:  Build a site X species matrix that also contains environmental variables 
#
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       May 15, 2019
######################################################################################################################

#### Start up tasks ####
# start fresh
rm(list=ls())

# Load packages
if(!require(rgdal)) install.packages("rgdal")

# Check which version of R is being used and reset if necessary
Sys.getenv("R_ARCH")   

# Set working directory
setwd("C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep")

dsn <- "C:/Users/daviessa/Documents/R/PROJECTS_MY/CommunityAssemblages/DataExploration/May"
layer <- "Testing_bhm"

# Read in crosswalk, spatial pts, inSituObs
myFile <- choose.files(caption="Select in situ environment variables")
inSitu <- read.csv(myFile, header=T, sep=",",stringsAsFactors = F)

myFile <- choose.files(caption="Select crosswalk from HKey to spatial ID")
keys <- read.csv(myFile, header = T, sep=",",stringsAsFactors = F)

sdata <- readOGR(dsn=dsn,layer=layer)
plot(sdata)

# Remove unnecessary in situ observations
inSitu$HKeyQuad <- paste0(inSitu$HKey,"_",inSitu$Quadrat)

# Add spatial ID to inSitu
df <- dplyr::inner_join(keys, inSitu, by=c("HKey","HKeyQuad","Quadrat"))
df <- dplyr::select(df, ID,Wood.Bark,Bedrock..smooth,Bedrock.with.crevices,Boulders,Cobble,Gravel,Pea.Gravel,Sand,Mud,Crushed.Shell,Whole.Chunk.Shell)

# Determine the percent cover for each substrate type by spatial ID
sbstrt <- df %>% 
  group_by(ID) %>% 
  summarise_each(list(mean))

# match new data to spatialized points
sdata@data = data.frame(sdata@data, sbstrt[match(sdata@data$ID, sbstrt$ID),])
sdata@data$ID.1 <- NULL
df.chk <- sdata@data  
head(df.chk)

#######################################################################################################################
# Benthic Habitat Mapping Dive Survey Data Exploration
# 
# Objective:  Match a list of headers to data in BHM db
# 
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       April 29, 2019
######################################################################################################################
# start fresh
rm(list=ls())

# Check which version of R is being used and reset if necessary
Sys.getenv("R_ARCH")   

# Set working directory
setwd("C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep")

outdir <- "C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep/Data/ncc_InnerVsOuter/"

# Load required packages 
if (!require('rgdal')) install.packages('rgdal'); library('rgdal')
if (!require('stringr')) install.packages('stringr'); library('stringr')

# Read in shps with headers, build list of headers
dsn <- "F:/GIS/Requests 2019/CommunityAssemblages/InletsVsOuterCoast"
shp1 <- "Inlet_sites"
shp2 <- "Outer_sites"
inner.shp <- readOGR(dsn=dsn,layer = shp1)
plot(inner.shp)
inner <- as.character(inner.shp@data$HKey)
outer.shp <- readOGR(dsn=dsn,layer = shp2)
plot(outer.shp)
outer <- as.character(outer.shp@data$HKey)

# Read in extracted headers
spp <- read.csv("./Data/SpeciesBy_matrices/AllSpeciesByQuadrat.csv", row.names=1,stringsAsFactors = F)
TransQuad <- str_split(spp$TransQuad, "_", simplify=TRUE)
colnames(TransQuad) <- c("HKey", "Quadrat")
spp <- cbind(TransQuad, spp)
spp$TransQuad <- NULL
head(spp)

# Match datasets
inner.sites <- dplyr::filter(spp, HKey %in% inner)
outer.sites <- dplyr::filter(spp, HKey %in% outer)

# Save output
write.csv(inner.sites, paste0(outdir,"inner.sites.csv"), row.names = FALSE)
write.csv(outer.sites, paste0(outdir,"outer.sites.csv"), row.names = FALSE)


#########################################################################
# Prep data for community assemblages analysis from BHM dive survey data
# 
# Objective: Organize data to for community assemblages analysis
#
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       October 2nd, 2019
#########################################################################

# start fresh
rm(list=ls())

# Check which version of R is being used and reset if necessary
Sys.getenv("R_ARCH")   

library(sp)
library(rgdal)
library(tidyverse)
library(mapview)

region <- "all" # options = ....

# Set working directory
setwd("C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep")

# 1. Read input data 
#-------------------
sppAll <- read.csv( "./Data/SpeciesBy_matrices/AllSpeciesByDepthCategory.csv", header=T, sep=",", stringsAsFactors=F )
fetch <- read.csv( "./Data/UpdatedObservations/Trans.coords.fetch.csv", header=T, sep=",", stringsAsFactors=F )
#subst <- read.csv( "./Data/UpdatedObservations/DepthCat_CalcSub.csv", header=T, sep=",", stringsAsFactors=F )
subst <- read.csv( "./Data/UpdatedObservations/DepthCat_summaries.csv", header=T, sep=",", stringsAsFactors=F )
survey <- read.csv( "./Data/LookupTbls/SurveyLookUpTbl.csv", header=T, sep=",", stringsAsFactors=F )
quad <- read.csv( "./Data/ExtractedData/Quadrat.csv", header=T, sep=",", stringsAsFactors=F )
hdrs <- read.csv( "./Data/ExtractedData/Headers.csv", header=T, sep=",", stringsAsFactors=F )
dsn <- "C:/Users/daviessa/Documents/R/PROJECTS_OTHERS/SpatializeDiveTransects/ByDepthCat"
sp <- readOGR(dsn=dsn, layer="All_SpatPts_DepthCat", stringsAsFactors = F)
env.nmes <- read.csv("C:/Users/daviessa/Documents/R/PROJECTS_OTHERS/SpatializeDiveTransects/ByDepthCat/envNames.csv")
mapview(sp)


# Organize field names
#---------------------
quad <- dplyr::select(quad, HKey, DepthCat)
quad <- unique(quad)

hdrs <- dplyr::select(hdrs, HKey, Survey)
unique(hdrs$Survey)

# To Do: fix database!
hdrs$Survey[hdrs$Survey=="PAC 2015-52"] <- "PAC 2015-052" 
hdrs$Survey[hdrs$Survey=="PAC 2015-36"] <- "PAC 2015-036" 
hdrs$Survey[hdrs$Survey=="PAC 2014-58"] <- "PAC 2014-058" 
hdrs$Survey[hdrs$Survey=="PAC 2014-29"] <- "PAC 2014-029" 
hdrs$Survey[hdrs$Survey=="Habitat Mapping 2017 - Indian Arm"] <- "PAC 2017-045" 
hdrs$Survey[hdrs$Survey=="Habitat Mapping 2017 - Gulf Islands"] <- "PAC 2017-045"
hdrs$Survey[hdrs$Survey=="Habitat Mapping 2017 - English Bay"] <- "PAC 2017-045"
hdrs$Survey[hdrs$Survey=="Habitat Mapping 2017 - Vancouver Harbour"] <- "PAC 2017-045" 
hdrs$Survey[hdrs$Survey=="Habitat Mapping 2017 - Juan de Fuca"] <- "PAC 2017-045" 
hdrs$Survey[hdrs$Survey=="PAC2018-105"] <- "PAC 2018-105" 
hdrs$Survey[hdrs$Survey=="PAC2018-067"] <- "PAC 2018-067" 
regions <- dplyr::left_join(hdrs,survey, by="Survey")

regions$chk <- paste0(regions$Region,"_",regions$Survey)
regions <- dplyr::select(regions, HKey, Survey, Region)

df <- dplyr::left_join(quad, regions, by="HKey")
df$TransDepth <- paste0(df$HKey,"_",df$DepthCat)
df <- dplyr::select(df, TransDepth, Region)

# Create list by region of all species
#-------------------------------------
# Join site X speces with region look-up table
addRegions <- right_join(df, sppAll, by="TransDepth")
row.names(addRegions) <- addRegions$TransDepth
addRegions$TransDepth <- NULL

# Save large dataframe as RDS
saveRDS(addRegions, "./Data/RDS/sppByRegion_Dataframe.rds")

# Create list dataframes, one for each region
sppByRegion <- c()
sppByRegion <- addRegions %>%
  split(addRegions$Region) 
# Save as RDS
sppByRegion <-lapply(sppByRegion, function(x) { x["Region"] <- NULL; x })
saveRDS(sppByRegion, "./Data/RDS/sppByRegion.rds")

    
# Combine with spatial points for one region
#-------------------------------------------
env.nmes <- env.nmes[,1] # convert to vector
colnames(sp@data) <- env.nmes
spts <- as.data.frame( cbind( sp@data,sp@coords ) )
colnames(spts)[colnames(spts)=="coords.x1"] <- "x"
colnames(spts)[colnames(spts)=="coords.x2"] <- "y"
spts$bathy <- NULL 
spts <- spts[c(2,1,10,11,3,4,5,6,7,8,9)]

fetch <- dplyr::select(fetch, TransDepth, Sum_Fetch)
df <- left_join(spts, fetch, by=c("TransDepth"))
df <- left_join(spts, fetch, by=c("TransDepth"))
subst$TransDepth <- paste0(subst$HKey,"_",subst$DepthCat)
df <- inner_join(df, subst, by=c("TransDepth","HKey"))
head(df,3)
summary(df)

# Missing data! Need to fix
df <- df %>% drop_na()

final <- inner_join(df, sppAll, by="TransDepth")
#head(final)
cat("All species ",dim(final),"\n")
# Save as csv
filename <- paste0("T:/Benthic_Habitat_Mapping/Data/Species by Site Matrices/",region,"byDepthCat_AllSpp.csv")
write_csv(final, filename)




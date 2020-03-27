#########################################################################
# Prep data for community assemblages analysis from BHM dive survey data
# 
# Objective: Organize data to for community assemblages analysis
#
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       March, 2020
#########################################################################

# start fresh
rm(list=ls())

# Check which version of R is being used and reset if necessary
Sys.getenv("R_ARCH")   

library(sp)
library(rgdal)
library(tidyverse)
library(mapview)

region <- "AllRegions" # options = ....

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
env <- read.csv( "./Data/UpdatedObservations/TransDepth_wEnv.csv", header=T, sep=",", stringsAsFactors=F )
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
head(addRegions)

# Save large dataframe as RDS
saveRDS(addRegions, "./Data/RDS/sppByRegion_Dataframe.rds")

# Create list dataframes, one for each region
sppByRegion <- c()
sppByRegion <- addRegions %>%
  split(addRegions$Region) 
# Remove region column in each element of list
sppByRegion <-lapply(sppByRegion, function(x) { x["Region"] <- NULL; x })
# Save as RDS on local drive and on spatial datasets
saveRDS(sppByRegion, "./Data/RDS/sppByRegion.rds")
#saveRDS(sppByRegion, "T:/Benthic_Habitat_Mapping/Data/Species by Site Matrices/sppByRegion.rds")

    
# Combine with spatial points for one region
#-------------------------------------------
#env.nmes <- env.nmes[,1] # convert to vector

colnames(sp@data) 
spts <- as.data.frame( cbind( sp@data,sp@coords ) )
colnames(spts)[colnames(spts)=="coords.x1"] <- "x"
colnames(spts)[colnames(spts)=="coords.x2"] <- "y"
spts$bathy <- NULL 

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
head(final,3)
# cat("All species ",dim(final),"\n")
# # Save as csv
# #filename <- paste0("T:/Benthic_Habitat_Mapping/Data/Species by Site Matrices/",region,"byDepthCat_AllSpp.csv")
# write_csv(final, filename)

# Add environmental data
#-----------------------
head(sppAll,3)
head(env,3)
env <- dplyr::select(env, -c(HKey,optional,bathy.1))
env <- env[c(1,3,4,2,5,6,7,8,9)]

subst$TransDepth <- paste0(subst$HKey, "_", subst$DepthCat)
subst <- dplyr::select(subst, TransDepth, substrt)
head(subst,3)
env.substr <- right_join(subst, env, by="TransDepth")
head(env.substr,3)
env.substr <- env.substr[c(1,3,4,2,5,6,7,8,9,10)]

final_env <- right_join(sppAll, env.substr, by="TransDepth") 
head(final_env,3)

# Create an RDS file of species names
#------------------------------------
colnames(sppAll[,-1])
species <- colnames(sppAll[,-1])
saveRDS(species, "C:/Users/daviessa/Documents/R/PROJECTS_MY/CommunityAssemblages/RDS/species.RDS")

# Create an RDS file of species & env data
#-----------------------------------------
colnames(final_env[,-1])
spp_env <- colnames(final_env[,-1])
saveRDS(final_env, "C:/Users/daviessa/Documents/R/PROJECTS_MY/CommunityAssemblages/RDS/NCCspp.env.RDS")
head(final_env,3)

# Create an SHP file of species & env data
#-----------------------------------------
# Build shapefile
coordinates(final_env) <- c("coords.x1", "coords.x2")

# Coordinate reference system (http://spatialreference.org
# BC Albers
crs.geo <-  CRS("+proj=aea +lat_1=50 +lat_2=58.5 +lat_0=45 +lon_0=-126 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs")

# define projection
proj4string(final_env) <- crs.geo

# plot
plot(final_env)

filename <- "NCCspp.env"
dsn <- "C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep/Data/SHP"
writeOGR(final_env, dsn=dsn, layer=filename, driver="ESRI Shapefile", overwrite_layer = TRUE )
cat("Fini!")

mapview(final_env)





#######################################################################################################################
# Adapted from Cluster analysis of Benthic Habitat Mapping Dive Survey Sites
# 
# Author:     Katie Gale
#             Katie.Gale@dfo-mpo.gc.ca
#             250-363-6411
#
# Date:       Sept 26, 2018
######################################################################################################################

# start fresh
rm(list=ls())

# Load packages
require(tidyverse)

if(!require(reshape)) install.packages("reshape")
if(!require(data.table)) install.packages("data.table")

setwd("C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep")

#Set up parameters
spThreshold <- 0.03  # proportion of sites that a species must be found in, to be included in the analysis. e.g., 0.02 is 2% of sites.
spCount <- 3 # Remove sites with 3 or less species

##############
#Import data 
cat("Select 'DiveSurveys_DataPrep/Data/SpeciesBy_matrices/AllSpeciesByDepthCategory.csv'")
myFile <- file.choose()
df  <- read.table(myFile,header=TRUE, sep=",")
head(df,3)
rownames(df) <- df$TransDepth
df <- df[,-1]
species <- names(df)

dim(df)

#Apply threshold for "rare" species and sites that have only a few observations, which can skew the cluster analysis
#check for species that only occur in one grid
specsums <- data.frame(species=names(colSums(df)), count=colSums(df)) #Calculate number of grids each species occurs in 
head(specsums)

rare <- subset(specsums, count<=(spThreshold*nrow(df))) # Remove species recorded less than X times
remRare <- df[,!(colnames(df) %in% rare$species)]
ncol(remRare) # We have 110 species

# Remove sites that only have one or two species because these sites throw off cluster analysis)
sitesums <- data.frame(site=rownames(remRare), count=rowSums(remRare))
barren <- subset(sitesums, count<=spCount) 

forCl <- remRare[!(rownames(remRare) %in% barren$site),]

dim(forCl) # 3664 sites, 110 species
forCl <- tibble::rownames_to_column(forCl, "TransDepth")

# Save output
write.csv(forCl,paste0(getwd(),"/Data/SpeciesBy_matrices/remRareByDepthCat.csv"),row.names = F)



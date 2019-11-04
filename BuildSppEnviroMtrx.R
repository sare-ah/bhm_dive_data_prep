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

library(tidyverse)

# Set working directory
setwd("C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep")

###-----------------
# 1. Read input data 
sppRare <- read.csv( "./Data/SpeciesBy_matrices/remRareByDepthCat.csv", header=T, sep=",", stringsAsFactors=F )
sppAll <- read.csv( "./Data/SpeciesBy_matrices/AllSpeciesByDepthCategory.csv", header=T, sep=",", stringsAsFactors=F )
trans.coords <- read.csv( "./Data/UpdatedObservations/Trans.coords.fetch.csv", header=T, sep=",", stringsAsFactors=F )
subst <- read.csv( "./Data/UpdatedObservations/DepthCat_CalcSub.csv", header=T, sep=",", stringsAsFactors=F )

trans.coords <- dplyr::select(trans.coords, TransDepth, LonStart, LatStart, Sum_Fetch)
df <- left_join(trans.coords, subst, by="TransDepth")

###-----------------
# 2. Combine All Species!
final <- right_join(df, sppAll, by="TransDepth")
head(final)
final[is.na(final)] <- 0
# Save as csv
write_csv(final, "T:/Benthic_Habitat_Mapping/Data/byDepthCat_AllSpp.csv")
rm(final)

###-----------------
# 2. Combine Without Rare Species!
final <- right_join(df, sppRare, by="TransDepth")
head(final)
final[is.na(final)] <- 0
# Save as csv
write_csv(final, "T:/Benthic_Habitat_Mapping/Data/byDepthCat_RemRareSpp.csv")


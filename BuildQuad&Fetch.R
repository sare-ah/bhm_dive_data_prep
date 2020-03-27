#----------------------------------------
# Build Site X Species table
# Quadrats and RMSM substrate (clusters)
# 
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       Feb 12, 2019
#----------------------------------------

# Start fresh
#------------
rm(list=ls())

# Load packages
#--------------
library(tidyverse)

# Set working directory
#----------------------
setwd("C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep/Data/SpeciesBy_matrices")

# Inputs
#-------
sppQuad <- read.csv("AllSpeciesByQuadrat.csv", stringsAsFactors = F)
fetch <- read.csv("../UpdatedObservations/Trans.fetch.bins.csv", stringsAsFactors = F)
summary(fetch)


# Add fetch to species by quadrat matrix
#--------------------------------------

# Subset fetch df
df <- dplyr::select(fetch, HKey, fetch.cat)
df <- unique(df)

# Join df together
sppQuadfetch <- left_join(sppQuad, df, by="HKey")
summary(sppQuadfetch)

# Check that there are no NA's and remove records
apply(sppQuadfetch, 2, function(x) any(is.na(x)))
sppQuadfetch <- sppQuadfetch[complete.cases(sppQuadfetch), ]

# Set up for Indicator Species Analysis
sppQuadfetch <- sppQuadfetch %>%
  rename(ID = HKeyQuad) %>%
  rename(cl = fetch.cat) %>%
  dplyr::select(-HKey) %>%
  dplyr::select(-ID, everything())
head(sppQuadfetch, 3)

# Save as csv and RDS
write_csv(sppQuadfetch, "AllSppByQuad_Fetch.csv")
saveRDS(sppQuadfetch, "C:/Users/daviessa/Documents/R/PROJECTS_MY/CommunityAssemblages/Results/byQuad_Fetch/speciesFullCl.RDS")


# Build frequency table for groupings (substrate types)
#------------------------------------------------------

# Extract frequency of values in cl field using count()
df <- count(sppQuadfetch, cl)

# Rename columns
colnames(df) <- c("cl","Freq")

# Order clusters by frequency order 
order.cl <- function(x){
  order <- data.frame( order=seq( 1:nrow(x) ),x[order(-x$Freq),] ) 
  order$cumsum <- cumsum( order$Freq )
  order$cumpercent <- round( order$cumsum/max(order$cumsum), 2 )
  order$percent <- round( order$Freq/sum(order$Freq),2 )
  return(order)
}
cluster.frq <- order.cl(df)

# Save output
saveRDS( cluster.frq, "C:/Users/daviessa/Documents/R/PROJECTS_MY/CommunityAssemblages/Results/byQuad_Fetch/cluster.freq.RDS" )
cluster.frq



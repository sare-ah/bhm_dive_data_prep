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
quad.sub <- read.csv("../UpdatedObservations/Quadrat_RMSM.csv", stringsAsFactors = F)
summary(quad.sub)

# Add RMSM to species by quadrat matrix
#--------------------------------------

# # Subset substrate df
# quad.sub <- quad.sub %>% 
#   dplyr::select(HKey, Quadrat, RMSM.cat) %>% 
#   mutate(HKeyQuad = paste0(HKey, "_", Quadrat))
# 
# # Join df together
# sppQuadRMSM <- left_join(sppQuad, quad.sub, by=c("HKey","HKeyQuad"))
# summary(sppQuadRMSM)
# 
# # Check that there are no NA's and remove records
# apply(sppQuadRMSM, 2, function(x) any(is.na(x)))
# sppQuadRMSM <- sppQuadRMSM[complete.cases(sppQuadRMSM), ]
# 
# # Set up for Indicator Species Analysis
# sppQuadRMSM <- sppQuadRMSM %>% 
#   rename(ID = HKeyQuad) %>% 
#   rename(cl = RMSM.cat) %>% 
#   dplyr::select(-c(HKey, Quadrat)) %>% 
#   dplyr::select(-ID, everything())
# head(sppQuadRMSM, 3) 
# 
# # Save as csv and RDS
# write_csv(sppQuadRMSM, "AllSppByQuad_RMSM.csv")
# saveRDS(sppQuadRMSM, "C:/Users/daviessa/Documents/R/PROJECTS_MY/CommunityAssemblages/Results/byQuad_RMSM/speciesFullCl.RDS")


# Build frequency table for groupings (substrate types)
#------------------------------------------------------

# # Extract frequency of values in cl field using count()
# df <- count(sppQuadRMSM, cl)
# 
# # Rename columns
# colnames(df) <- c("cl","Freq")



# Add BoP BType1 to species by quadrat matrix
#--------------------------------------

# Subset substrate df
quad.bop1 <- quad.sub %>% 
  dplyr::select(HKey, Quadrat, BType1) %>% 
  mutate(HKeyQuad = paste0(HKey, "_", Quadrat))

# Join df together
sppQuadBoP1 <- left_join(sppQuad, quad.bop1, by=c("HKey","HKeyQuad"))
summary(sppQuadBoP1)

# Check that there are no NA's and remove records
apply(sppQuadBoP1, 2, function(x) any(is.na(x)))
sppQuadBoP1 <- sppQuadBoP1[complete.cases(sppQuadBoP1), ]

# Set up for Indicator Species Analysis
sppQuadBoP1 <- sppQuadBoP1 %>% 
  rename(ID = HKeyQuad) %>% 
  rename(cl = BType1) %>% 
  dplyr::select(-c(HKey, Quadrat)) %>% 
  dplyr::select(-ID, everything())
head(sppQuadBoP1, 3) 

# Save as csv and RDS
write_csv(sppQuadBoP1, "AllSppByQuad_BoP1.csv")
saveRDS(sppQuadBoP1, "C:/Users/daviessa/Documents/R/PROJECTS_MY/CommunityAssemblages/Results/byQuad_BoP1/speciesFullCl.RDS")


# Build frequency table for groupings (substrate types)
#------------------------------------------------------

# Extract frequency of values in cl field using count()
df <- count(sppQuadBoP1, cl)

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
saveRDS( cluster.frq, "C:/Users/daviessa/Documents/R/PROJECTS_MY/CommunityAssemblages/Results/byQuad_BoP1/cluster.freq.RDS" )
cluster.frq



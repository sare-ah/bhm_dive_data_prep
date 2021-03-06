######################################################################################################################
# Benthic Habitat Mapping Dive Survey Data Summary
# 
# Objective:  Update substrate observations to assign each quadrat to one of four categories (Rock, mixed, sand, mud)
#
# Requires:   Run 1.ExtractDataFromMSAccess.R script
#
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       January 5, 2019
######################################################################################################################

#### Start up tasks ####
# start fresh
rm(list=ls())

# Check which version of R is being used and reset if necessary
Sys.getenv("R_ARCH")   
# The message returned will tell you which version of R is being used
# "/i386" 32 bit R --- which is necessary to grab data from MS Access database
# "/64"   64 bit R
# To reset: Select Tools menu | Global Options... | R Version: | Change
# Then you will have to open and close R for the changes to take effect

# Set working directory
setwd("C:/Users/daviessa/Documents/CURRENT_DATA")

################ Functions #####################################
################################################################

# Install missing packages and load required packages (if required)
UsePackages <- function( pkgs, update=FALSE, locn="http://cran.rstudio.com/" ) {
  # Identify missing (i.e., not yet installed) packages
  newPkgs <- pkgs[!(pkgs %in% installed.packages( )[, "Package"])]
  # Install missing packages if required
  if( length(newPkgs) )  install.packages( newPkgs, repos=locn )
  # Loop over all packages
  for( i in 1:length(pkgs) ) {
    # Load required packages using 'library'
    eval( parse(text=paste("library(", pkgs[i], ")", sep="")) )
  }  # End i loop over package names
  # Update packages if requested
  if( update ) update.packages( ask=FALSE )
}  # End UsePackages function

# Make packages available
UsePackages( pkgs=c("tidyverse","reshape") ) 

##############
# Pseudocode
##############
# 1. Read in quad data 
# 2. Calculate the substrate that represents >50% for each quad

#####################
# 1. Read input data 
#####################
# Read in quadrat table that was extracted from the MS Access db using the script ExtractDataFromMSAccess.R
quad <- read.csv( "./ExtractedData/Quadrat.csv", header=T, sep="," )
dim(quad)

# remove algae percentages
quad <- dplyr::select(quad, 1:12)

################################################################
# 2. Calculate the substrate that represents >50% for each quad
################################################################
# Need to remove quad 0 from each transect - it represents the starting point for each transect, only depth & time recorded
quad <- filter(quad, Quadrat!=0)
dim(quad)

# As a check calculate the number of HKey and Quadrat combinations
key.combos <- quad %>% 
  dplyr::filter(HKey, Quadrat) 
dim(unique(key.combos))

# Read in substrate category table
sub.cat <- read.csv( "./LookupTbls/SubstrateCategories.csv", header=T, sep="," )

# Remove data without Substrate1 *** TO DO *** Look into why this is later...
quad <- dplyr::filter(quad, !is.na(Substrate1))
dim(quad)

# Match substrateID to substrate category
quad.sub <- dplyr::left_join(quad, sub.cat, by=c("Substrate1", "Substrate2"))

# Check your work
summary(quad.sub)
dim(quad.sub)

# Save updated quadrat table
write.csv(quad.sub, "./UpdatedObservations/Quadrat_RMSM.csv", row.names = F)

# Add Headers details to recordset
hdrs <- read.csv( "./ExtractedData/Headers.csv", header=T, sep="," )
dim(hdrs)

# Calculate x,y
hdrs$LatStart<-(hdrs$LatDegStart+(hdrs$LatMinStart/60))
hdrs$LatEnd<-(hdrs$LatDegEnd+(hdrs$LatMinEnd/60))
hdrs$LonStart<-(-1)*abs(hdrs$LonDegStart+(hdrs$LonMinStart/60))
hdrs$LonEnd<-(-1)*abs(hdrs$LonDegEnd+(hdrs$LonMinEnd/60))

# Select columns of interest
hdrs <- dplyr::select(hdrs, HKey, LatStart, LonStart, LatEnd, LonEnd)
head(hdrs)

# Join hdrs and quad.sub


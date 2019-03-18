#######################################################################################################################
# Benthic Habitat Mapping Dive Survey Data Summary
# 
# Objective:  Update substrate observations to assign each quadrat to one of four categories (Rock, mixed, sand, mud)
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
setwd("F:/R/MY_PROJECTS/DiveSurveys_DataPrep")

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
UsePackages( pkgs=c("dplyr","reshape") ) 

##############
# Pseudocode
##############
# 1. Read in quad data 
# 2. Calculate the substrate that represents >50% for each quad

#####################
# 1. Read input data 
#####################
# Read in quadrat table that was extracted from the MS Access db using the script ExtractDataFromMSAccess.R
quad <- read.csv( "./Data/ExtractedData/Quadrat.csv", header=T, sep="," )

# remove algae percentages
quad <- dplyr::select(quad, 2:13)

################################################################
# 2. Calculate the substrate that represents >50% for each quad
################################################################
# Need to remove quad 0 from each transect - it represents the starting point for each transect, only depth & time recorded
quad <- filter(quad, Quadrat!=0)

# Read in substrate category table
sub.cat <- read.csv( "./Data/LookupTbls/SubstrateCategories.csv", header=T, sep="," )

# Match substrateID to substrate category
quad.sub <- dplyr::left_join(quad, sub.cat, by=c("Substrate1", "Substrate2"))

# Save updated quadrat table
write.csv(quad.sub, "./Data/UpdatedObservations/Quadrat_updated.csv")

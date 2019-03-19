#######################################################################################################################
# Benthic Habitat Mapping Dive Survey Data Summary
# 
# Objective:  Summarise species data from BHM SCUBA surveys into species by sites matrices
#             Species X Site table
#             Species X Quadrat table
#             Species X Depth category table
#             Species X Substrate table
#             Species X Substrate & Depth table
# 
# Requires:   Run 1.ExtractDataFromMSAccess.R script and 2.CreateUpdatedSppObs.R
# 
# Background: Species presence/absence and substrate data is stored in
#             T:/Dive_Surveys/Database/BHM_DiveSurveys_Current.mdb
#             Data was edited for typos extra with CreateUpdateSppObs.R script
# 
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       March 19, 2019
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
setwd("C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep")

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

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# Make packages available
UsePackages( pkgs=c("dplyr","reshape", "vegan") ) 

###########################
# Pseudocode
###########################
# 1. Read in species data 
# 2. Build Species X Site table
# 3. Build Species X Quadrat table
# 4. Build Species X Depth category table
# 5. Build Species X Substrate table
# 6. Build Species X Substrate & Depth table

#####################
# 1. Read input data 
#####################
# Read in tables that were extracted from the MS Access db using the script ExtractDataFromMSAccess.R
# & other look-up tables 
spp <- read.csv( "./Data/UpdatedObservations/SpeciesObs_updated.csv", header=T, sep="," )
hdrs <- read.csv( "./Data/ExtractedData/Headers.csv", header=T, sep="," )
quads <- read.csv( "./Data/ExtractedData/Quadrat.csv", header=T, sep="," )

################################
# 2. Build Species X Site table
################################
# Remove extra fields
sppSite <- dplyr::select( spp, HKey, Spp_cde_type )
# Remove duplicate rows
sppSite <- unique(sppSite)
# Build species X site matrix
sppSite <- reshape2::dcast( sppSite, HKey~Spp_cde_type, fun=length, value.var = "Spp_cde_type")
write.csv( sppSite, "./Data/SpeciesBy_matrices/AllSpeciesBySite.csv" )

###################################
# 3. Build Species X Quadrat table
###################################
# Combine HKey & Quadrat number 
spp$TransQuad <- paste( spp$HKey, spp$Quadrat, sep="_" )
# Remove extra fields
sppQuad <- dplyr::select( spp, TransQuad, Spp_cde_type )
# Build species X quadrat matrix
sppQuad <- reshape2::dcast( sppQuad, TransQuad~Spp_cde_type, fun=length, value.var = "Spp_cde_type" )
write.csv(sppQuad, "./Data/SpeciesBy_matrices/AllSpeciesByQuadrat.csv")

##########################################
# 4. Build Species X Depth category table
##########################################
# Remove some field from the quads table
quadDepth <- dplyr::select( quads, HKey, Quadrat, DepthCat )
# Join quadDepth with spp
sppQuadDepth <- inner_join( spp, quadDepth, by=c("HKey","Quadrat") )
# Remove extra fields
sppDepth <- dplyr::select( sppQuadDepth, HKey, DepthCat, Spp_cde_type )
# Combine transect and depth category
sppDepth$TransDepth <- paste( sppDepth$HKey, sppDepth$DepthCat, sep="_" )
# Remove extra fields
sppDepth <- dplyr::select( sppDepth, TransDepth, Spp_cde_type )
# Remove duplicate rows
sppDepth <- unique( sppDepth )
# Build species X depth category matrix
sppDepth <- reshape2::dcast( sppDepth, TransDepth~Spp_cde_type,  fun=length, value.var = "Spp_cde_type" )
#sppDepth$TransDepth <- substr(sppDepth$TransDepth, ) # This line throws an error
write.csv(sppDepth, "./Data/SpeciesBy_matrices/AllSpeciesByDepthCategory.csv", row.names = FALSE)

#####################################
# 5. Build Species X Substrate table
#####################################
## Use only the dominant substrate, up to 3 substrate values can be recorded on the data sheet
# Select some fields from the quads table
quadSub <- dplyr::select( quads, HKey, Quadrat, Substrate1 )
# Join quadSub with spp
sppQuadSub <- inner_join( spp, quadSub, by=c("HKey","Quadrat") )
# Build Species X Substrate matrix
sppSub <- dplyr::select( sppQuadSub, Substrate1, Spp_cde_type )
sppSub$Substrate <- as.factor(sppSub$Substrate1)
sppSub$Substrate1 <- NULL
sppSub <- na.omit(sppSub)
# # Remove extra fields
# sppSub <- dplyr::select( sppQuadSub, HKey, Substrate1, Spp_cde_type )
# # Combine transect and substrate category
# sppSub$TransSub <- paste( sppSub$HKey, sppSub$Substrate1, sep="_" )
# # Remove extra fields
# sppSub <- dplyr::select( sppSub, TransSub, Spp_cde_type )
# # Remove duplicate rows
# sppSub <- unique( sppSub )
# Build species X depth category matrix
sppSub <- reshape2::dcast( sppSub, Spp_cde_type~Substrate,  fun=length, value.var = "Spp_cde_type" )

write.csv(sppSub, "./Data/SpeciesBy_matrices/AllSpeciesByPrimarySubstrate.csv")

#############################################
# 6. Build Species X Substrate & Depth table
#############################################
## Use only the dominant substrate, up to 3 substrate values can be recorded on the data sheet
# Select some fields from the quads table
SubDepth <- dplyr::select( quads, HKey, Quadrat, Substrate1, DepthCat )
# Join with spp
sppSubDepth <- inner_join( spp, SubDepth, by=c("HKey","Quadrat") )
# Remove extra fields
sppSubDepth <- dplyr::select( sppSubDepth, HKey, DepthCat, Substrate1, Spp_cde_type )
# Combine transect, depth, and substrate category
sppSubDepth$trns.dpth.sub <- paste( sppSubDepth$HKey, sppSubDepth$DepthCat, sppSubDepth$Substrate1, sep="_" )
# Remove extra fields
sppSubDepth <- dplyr::select( sppSubDepth, trns.dpth.sub, Spp_cde_type )
# Remove duplicate rows
sppSubDepth <- unique( sppSubDepth )
# Build species X Depth and Substrate matrix
sppSubDepth <- reshape2::dcast( sppSubDepth, trns.dpth.sub~Spp_cde_type, fun=length, value.var = "Spp_cde_type" )
write.csv( sppSubDepth, "./Data/SpeciesBy_matrices/AllSpeciesByDepthAndSubstrate.csv")

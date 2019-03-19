#######################################################################################################################
# Species prevalence
# 
# Objective:  Determine species prevalence using benthic habitat mapping species tables organized by depth category
#             
# Background: Species presence/absence and substrate data is stored in the WC_Dive_Master.mdb database
# 
# Requires:   Run 1.ExtractDataFromMSAccess.R, 2.CreateUpdateSppObs.R, 
#             3a.Algae_BuildMtrx.R, and 3b.Invert_BuildMtrx.R scripts
# 
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       August 1, 2018
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

# Functions 
# =========

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
UsePackages( pkgs=c("dplyr","reshape", "vegan", "stringr", "plyr", "tidyr") ) 


# Pseudocode
# ==========
# 1. Read in algae & invert tables
# 2. Combine tables
# 3. Calculate frequency of occurence


# Read in algae & invert tables
invert <- read.csv( "./Data/SpeciesBy_matrices/InvertsByDepthCategory.csv", header=T, sep=",", stringsAsFactors = FALSE )
algae <- read.csv( "./Data/SpeciesBy_matrices/AlgaeByDepthCategory.csv", header=T, sep=",", stringsAsFactors = FALSE )
sp.nme <- read.csv("./Data/LookupTbls/SpeciesLookUpTbl.csv", header=T, sep=",", stringsAsFactors = FALSE)

# Combine tables
colnames(invert) <- paste("I", colnames(invert), sep = "_")
colnames(invert)[1] <- "TransDepth"
colnames(algae) <- paste("A", colnames(algae), sep = "_")
colnames(algae)[1] <- "TransDepth"
all.spp <- dplyr::full_join(invert, algae, by="TransDepth")
all.spp[is.na(all.spp)] <- 0

# Determine number of transect/depth category combinations 
n <- (nrow(all.spp)-1) 

# Determine number of species per transect/depth category combinations
sppOnly <- dplyr::select(all.spp, -TransDepth)

# Build a dataframe with species counts and add species name
sppNmes <- colnames(sppOnly)
cnts <- colSums(sppOnly)
sppPreval <- data.frame(sppNmes, cnts)
sppPreval$prevalence <- (round( sppPreval$cnts/n, digits=3 )*100) # Prevalence of species over all sites
sppPreval <- sppPreval[order(sppPreval$prevalence),]
colnames(sppPreval)<- c("Species_Code","counts","prevalence")
sp.nme$Species_Code <- as.character(sp.nme$Species_Code)
sppPreval$Species_Code <- as.character(sppPreval$Species_Code)
sppPreval <- dplyr::left_join(sppPreval, sp.nme, by="Species_Code" )

# Write to file
write.csv(sppPreval, file="./Data/SpeciesPrevalence/SpeciesPrevalence.csv", row.names = FALSE)

# To Do:
# Make plots of species prevalence
# Species vs. species-groups

# # Build a histogram
# #hist(sppPreval$prevalence)
# hist(sppPreval$prevalence, freq=FALSE, main="Density plot", xlim=c(0,60),ylim = c(0,0.1))
# hist(sppPreval$prevalence, xlim=c(0,60),ylim = c(0,80))



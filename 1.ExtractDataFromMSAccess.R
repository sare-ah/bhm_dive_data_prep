#######################################################################################################################
# Benthic Habitat Mapping Dive Survey Data Extraction
# 
# Objective:  Read tables from MS Access database and save as csv
#
# Background: Species presence/absence and substrate data is stored in the WC_Dive_Master.mdb database
#
# Notes:      1. Code using directory paths found on my computer.  Other users may need to adapt code.
#             2. RODBC package REQUIRES 32-bit R 
# 
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       October 10, 2017
######################################################################################################################

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

# Grab the survey data from MS Access database
GrabDat <- function( db, tbl, clean=TRUE ) {
  # Extract the data base extension
  dbExt <- strsplit( x=basename(db), split=".", fixed=TRUE )[[1]][2]
  # Connect to the data base depending on the extension
  if( dbExt == "mdb" )  dataBase <- odbcConnectAccess( access.file=db )
  if( dbExt == "accdb" )  dataBase <- odbcConnectAccess2007( access.file=db )
  # Grab the survey data
  hdrs <- sqlFetch( channel=dataBase, sqtable=tbl )
  # Message re data imported
  cat( "Imported",tbl, "table with", nrow(hdrs), 
       "rows and", ncol(hdrs), "columns\n" )
  # Close the connection
  odbcCloseAll( )
  # Return the data tables
  return( hdrs )
}  # End GrabDat function

################ End Functions #################################
################################################################

# Make packages available
UsePackages( pkgs=c("RODBC") ) 

##############################
# 1. Read in the WCTSS data
##############################

# Add a function to read in all the data

# Set path to the Access database
db <- "T:/Dive_Surveys/Database/BHM_DiveSurveys_Current.mdb"

# Extract five tables from the db
headers <- GrabDat( db, tbl="1-Headers" )
spec <- GrabDat( db, tbl="3-SpeciesObservations" )
quadrat <- GrabDat( db, tbl="2-Quadrat" )
dropCam <- GrabDat( db, tbl="8-DropCam" )

# TO DO --- understand why some records need to be corrected manually

# Do some corrections on the species codes
spec$SpType[spec$SpecCode=="FU"]<-"A"
spec$SpType[spec$SpecCode=="LH"]<-"I"
spec$SpType[spec$SpecCode=="RSC"]<-"I"
spec$SpType[spec$SpecCode=="PN"]<-"I"

# Do some corrections for survey names
headers$Survey <- as.character(headers$Survey)
headers$Survey[headers$Survey=="Habitat Mapping 2017 - Indian Arm"]<-"PAC 2017-045"
headers$Survey[headers$Survey=="Habitat Mapping 2017 - English Bay"]<-"PAC 2017-045"
headers$Survey[headers$Survey=="Habitat Mapping 2017 - Gulf Islands"]<-"PAC 2017-045"
headers$Survey[headers$Survey=="Habitat Mapping 2017 - Vancouver Harbour"]<-"PAC 2017-045"
headers$Survey[headers$Survey=="Habitat Mapping 2017 - Juan de Fuca"]<-"PAC 2017-045"
headers$Survey <- as.factor(headers$Survey)

# Write out tables for MS Access db so that you don't have to import them again!
write.csv(headers, "./Data/ExtractedData/Headers.csv", row.names = F)
write.csv(spec, "./Data/ExtractedData/SpeciesObs.csv", row.names = F)
write.csv(quadrat, "./Data/ExtractedData/Quadrat.csv", row.names = F)
write.csv(dropCam, "./Data/ExtractedData/DropCam.csv", row.names = F)



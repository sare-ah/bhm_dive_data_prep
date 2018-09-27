#######################################################################################################################
# Benthic Habitat Mapping Dive Survey Data Summary --- Build In Situ Observations for depth, slope, and substrate
# 
# Objective:  Build a matrix of environmental variables collected at the quadrat level by divers
#
# Notes:      This script uses the output of ExtractDataFromMSAccess.R; this script may need to be rerun first
#
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       September 26, 2018
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
setwd("C:/Users/daviessa/Documents/R/MY_PROJECTS/DiveSurveys_DataPrep")

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
UsePackages( pkgs=c("dplyr","tidyr", "plyr","reshape","reshape2") ) 

##############
# Pseudocode
##############
# 1. Read in data 
# 2. Calculate mean depth & slope for each quadrat
# 3. Summarise substrate for each quadrat
# 4. Build new table of insitu observations

### 1. Read input data ###
##########################
# Read in tables that were extracted from the MS Access db using the script ExtractDataFromMSAccess.R
quad <- read.csv( "./Data/ExtractedData/Quadrat.csv", header=T, sep="," )
quad <- dplyr::select(quad, HKey, Quadrat, CorDepthM, DepthCat, Substrate1, Sub1Pct, Substrate2, Sub2Pct, Substrate3, Sub3Pct )

### 2. Calculate mean depth & slope for each quadrat ###
########################################################
depth <- dplyr::select(quad, HKey, Quadrat, CorDepthM)

# Set start depth as the recorded depth from the previous quadrat, unless the quadrat is number 0
depth$StartDepth <- ifelse( depth$Quadrat==0, depth$CorDepthM,lag(depth$CorDepthM, n=1)  )
depth$EndDepth <- depth$CorDepthM

# Calculate mean depth for each quadrat
depth$MeanDepth <- (depth$StartDepth+depth$EndDepth)/2

# Calculate slope for each quadrat using the arc-tangent
depth$Elev.Diff <- depth$StartDepth-depth$EndDepth
depth$Slope <- atan2(depth$Elev.Diff,5) 

# Remove unnecessary columns
depth <- dplyr::select(depth, HKey, Quadrat, MeanDepth, Slope)
# Remove quadrat 0 
depth <- filter(depth, Quadrat!=0)

# What to do with records with slope greater than 1.0 or less than -1.0??

### 3. Summarise substrate for each quadrat ###
###############################################
substrate <- dplyr::select(quad, HKey, Quadrat, Substrate1, Sub1Pct, Substrate2, Sub2Pct, Substrate3, Sub3Pct )

# Remove quadrat 0 b/c it has no substrate records
substrate <- filter(substrate, Quadrat!=0)

# Separate data for each substrate field
primary <- dplyr::select(substrate, HKey, Quadrat, Substrate1, Sub1Pct)
names(primary)[names(primary)=="Substrate1"] <- "Substrate"
names(primary)[names(primary)=="Sub1Pct"] <- "SubPct"
secondary <- dplyr::select(substrate, HKey, Quadrat, Substrate2, Sub2Pct)
names(secondary)[names(secondary)=="Substrate2"] <- "Substrate"
names(secondary)[names(secondary)=="Sub2Pct"] <- "SubPct"
tertiary <- dplyr::select(substrate, HKey, Quadrat, Substrate3, Sub3Pct)
names(tertiary)[names(tertiary)=="Substrate3"] <- "Substrate"
names(tertiary)[names(tertiary)=="Sub3Pct"] <- "SubPct"

# Combine rows into one long and narrow table
all.sub <- bind_rows(primary,secondary,tertiary)
# Recode to substrate types
all.sub$Substrate <- as.factor(all.sub$Substrate)
all.sub$Substrate <- revalue(all.sub$Substrate,
                                c("0"="Wood/Bark","1"="Bedrock, smooth","2"="Bedrock with crevices","3"="Boulders",
                                  "4"="Cobble","5"="Gravel","6"="Pea Gravel","7"="Sand","9"="Mud",
                                  "10"="Crushed Shell","11"="Whole/Chunk Shell"))
# Remove empty records (locations with only primary or secondary substrates recorded)
all.sub <- na.omit(all.sub)
# Cast to a wide table with one column for each possible substrate code
sub.cast <- dcast(all.sub, HKey + Quadrat ~ Substrate, fun.aggregate = sum, value.var = "SubPct" )

### 4. Build new table of insitu observations ###
#################################################
final <- dplyr::full_join(depth, sub.cast, by=c("HKey", "Quadrat"))

# Save in situ observations
write.csv(final, "./Data/ExtractedData/InSituObs.csv")


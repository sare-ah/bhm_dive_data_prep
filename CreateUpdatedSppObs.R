#######################################################################################################################
# Benthic Habitat Mapping Dive Survey Data Summary
# 
# Objective:  Update species observations to fix typos and recode non-target species and save new csv files
#
# Author:     Sarah Davies
#             Sarah.Davies@dfo-mpo.gc.ca
#             250-756-7124
# Date:       January 28, 2018
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
UsePackages( pkgs=c("dplyr","reshape") ) 

##############
# Pseudocode
##############
# 1. Read in data 
# 2. Recode invertebrate species codes
# 3. Recode algae species codes
# 4. Divide dataframe into I & A and save updated tables

#####################
# 1. Read input data 
#####################
# Read in tables that were extracted from the MS Access db using the script ExtractDataFromMSAccess.R
spp <- read.csv( "./Data/ExtractedData/SpeciesObs.csv", header=T, sep="," )

### Edit Observations table ###
spp$Species <- as.character(spp$Species)
spp$SpType <- toupper(spp$SpType) # capitalize
spp$Species[spp$Species=="C L"] <- "CL" # correct a typo

# Change column name
names(spp)[names(spp) == 'Species'] <- 'Species_Code'
# algae <- dplyr::filter( spp, SpType=="A" )
# spp <- dplyr::filter( spp, SpType=="I" )

n <- unique(spp$Species)
#length(n)

####################################################################
# 2. Recode some invertebrate observations to match the data sheets
####################################################################
# Recode some species - based on notes found in Habitat Mapping database
# These species were either only recorded in 2013 or recorded as a species group in later surveys
# Comments explain what observations are being recoded to and counts of dive sites with presence
spp$Species_Code[spp$Species_Code=="AB" & spp$SpType=="I"] <- "OB"  # Acorn barnacle = Barnacle - other --- chk yrs
spp$Species_Code[spp$Species_Code=="BB" & spp$SpType=="I"] <- "BRF" # Blue encrusting bryozoan(2) = Bryozoan flat
spp$Species_Code[spp$Species_Code=="BS" & spp$SpType=="I"] <- "SPE" # Boot sponge = Sponge erect
spp$Species_Code[spp$Species_Code=="TS" & spp$SpType=="I"] <- "SPE" # Trumpet sponge = Sponge erect
spp$Species_Code[spp$Species_Code=="GS" & spp$SpType=="I"] <- "SPE" # Glove sponge = Sponge erect
spp$Species_Code[spp$Species_Code=="HS" & spp$SpType=="I"] <- "SH"  # Humpback shrimp(4) = Shrimp
spp$Species_Code[spp$Species_Code=="NS" & spp$SpType=="I"] <- "SH"  # Coonstripe shrimp(13) = Shrimp
spp$Species_Code[spp$Species_Code=="SN" & spp$SpType=="I"] <- "SH"  # Spot prawn(2) = Shrimp
spp$Species_Code[spp$Species_Code=="SW" & spp$SpType=="I"] <- "SP"  # Sea whip(1) = sea pen
spp$Species_Code[spp$Species_Code=="FT" & spp$SpType=="I"] <- "SL"  # Chelyosoma productum(1) = tunicate - solitary - other

n <- unique(spp$Species_Code)
length(n)

# Non-target species
spp$Species_Code[spp$Species_Code=="LP" & spp$SpType=="I"] <- "nontarget"  # Six-armed star(6) = " "
spp$Species_Code[spp$Species_Code=="BC" & spp$SpType=="I"] <- "nontarget"  # Butter clam (Saxidomus gigantea)(5) = ?
spp$Species_Code[spp$Species_Code=="CO" & spp$SpType=="I"] <- "nontarget"  # Cockle (Clinocardium nuttallii)(7) = ?
spp$Species_Code[spp$Species_Code=="XH" & spp$SpType=="I"] <- "nontarget"  # Stevens' hermit crab(5) = ?
spp$Species_Code[spp$Species_Code=="KL" & spp$SpType=="I"] <- "nontarget"  # Limpets(4) - removed
spp$Species_Code[spp$Species_Code=="TW" & spp$SpType=="I"] <- "nontarget"  # Calcareous tubeworm(36) - removed b/c ubiquitous
spp$Species_Code[spp$Species_Code=="DA" & spp$SpType=="I"] <- "nontarget"  # Nudibranch (Dirona albolineata) - removed
spp$Species_Code[spp$Species_Code=="OD" & spp$SpType=="I"] <- "nontarget"  # Nudibranch (Dirona pellucita) - removed
spp$Species_Code[spp$Species_Code=="CN" & spp$SpType=="I"] <- "nontarget"  # Nudibranch (Hermissenda crassicornis) - removed
spp$Species_Code[spp$Species_Code=="RT" & spp$SpType=="I"] <- "nontarget"  # Brown turban --- chk yrs
spp$Species_Code[spp$Species_Code=="PC" & spp$SpType=="I"] <- "nontarget"  # Psolus --- chk yrs
spp$Species_Code[spp$Species_Code=="SY" & spp$SpType=="I"] <- "nontarget"  # Spiny red star --- chk yrs
spp$Species_Code[spp$Species_Code=="WU" & spp$SpType=="I"] <- "nontarget"  # White urchin --- chk yrs

n <- unique(spp$Species_Code)
length(n)

# Species codes that are likely meant to be algae not inverts
spp$SpType[spp$Species_Code=="AC" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species_Code=="AF" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species_Code=="BF" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species_Code=="BH" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species_Code=="BO" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species_Code=="CL" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species_Code=="DB" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species_Code=="GI" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species_Code=="GR" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species_Code=="LA" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species_Code=="PH" & spp$SpType=="I"] <- "A"

# Typos that were removed, do not correspond with any code
spp$Species_Code[spp$Species_Code=="GSC" & spp$SpType=="I"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="JL" & spp$SpType=="I"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="PY" & spp$SpType=="I"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="TH" & spp$SpType=="I"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="UA" & spp$SpType=="I"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="YZ" & spp$SpType=="I"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="MO" & spp$SpType=="I"] <- "nontarget"

# Typo that was not feasibly observed at depth 
spp$Specie_Code[spp$Species_Code=="OR"& spp$SpType=="I"] <- "nontarget" #Pacific Oyster

n <- unique(spp$Species_Code)
length(n)

#############################################################
# 3. Recode some algae observations to match the data sheets
#############################################################
# Non-target species
spp$Species_Code[spp$Species_Code=="AG" & spp$SpType=="A"] <- "nontarget"  # Agarum # OR is this a typo --- Red turban snail?
spp$Species_Code[spp$Species_Code=="AL" & spp$SpType=="A"] <- "nontarget"  # Alaria 
spp$Species_Code[spp$Species_Code=="BO" & spp$SpType=="A"] <- "nontarget"  # Bossiella 
spp$Species_Code[spp$Species_Code=="DE" & spp$SpType=="A"] <- "nontarget"  # Desmarestia 
spp$Species_Code[spp$Species_Code=="EN" & spp$SpType=="A"] <- "nontarget"  # Encrusting 
spp$Species_Code[spp$Species_Code=="LA" & spp$SpType=="A"] <- "nontarget"  # Laminaria 
spp$Species_Code[spp$Species_Code=="OP" & spp$SpType=="A"] <- "nontarget"  # Opuntella 
spp$Species_Code[spp$Species_Code=="BP" & spp$SpType=="A"] <- "nontarget"  # Botryocladia pseudodichotoma
spp$Species_Code[spp$Species_Code=="DG" & spp$SpType=="A"] <- "nontarget"  # Derbesia marina

n <- unique(spp$Species)
length(n)

# Species codes that are likely meant to be inverts not algae
# If they are non-target invertebrates, update the Species_Code too
spp$SpType[spp$Species_Code=="BA" & spp$SpType=="A"] <- "I" # Brooding anemone
spp$SpType[spp$Species_Code=="DC" & spp$SpType=="A"] <- "I" # Dungeness crab
spp$SpType[spp$Species_Code=="HF" & spp$SpType=="A"] <- "I" # Hydrocoral
spp$SpType[spp$Species_Code=="OB" & spp$SpType=="A"] <- "I" # Other barnacle
spp$SpType[spp$Species_Code=="PU" & spp$SpType=="A"] <- "I" # Purple urchin 
spp$SpType[spp$Species_Code=="RS" & spp$SpType=="A"] <- "I" # Rock scallop


# Observations that are likely typos
spp$Species_Code[spp$Species_Code=="AN" & spp$SpType=="A"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="AT" & spp$SpType=="A"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="BR" & spp$SpType=="A"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="DR" & spp$SpType=="A"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="FY" & spp$SpType=="A"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="LD" & spp$SpType=="A"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="LM" & spp$SpType=="A"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="NE" & spp$SpType=="A"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="RG" & spp$SpType=="A"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="RM" & spp$SpType=="A"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="SY" & spp$SpType=="A"] <- "nontarget"
spp$Species_Code[spp$Species_Code=="*" & spp$SpType=="A"] <- "nontarget"

n <- unique(spp$Species)
length(n)

# Remove nontarget species codes
spp <- filter( spp, Species_Code!="nontarget" )

# Create invert observations and save
invert <- dplyr::filter( spp, SpType=="I")
write.csv(invert, "./Data/UpdatedObservations/InvertObs_updated.csv")

# Create invert observations and save
algae <- dplyr::filter( spp, SpType=="A")
write.csv(algae, "./Data/UpdatedObservations/AlgaeObs_updated.csv")

# Save updated species observations
write.csv(spp, "./Data/UpdatedObservations/SpeciesObs_updated.csv")

#######################################################################################################################
# Benthic Habitat Mapping Dive Survey Data Summary
# 
# Objective:  Update species observations to fix typos and recode non-target species and save new csv files
#             Creates three csv files: InvertObs_updated.csv
#                                      AlgaeObs_updated.csv
#                                      SpeciesObs_updated.csv   
#
# Requires:   Run 1.ExtractDataFromMSAccess.R script
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

# # Change column name
# names(spp)[names(spp) == 'Species'] <- 'Species'
# # algae <- dplyr::filter( spp, SpType=="A" )
# # spp <- dplyr::filter( spp, SpType=="I" )

n <- unique(spp$Species)
#length(n)

####################################################################
# 2. Recode some invertebrate observations to match the data sheets
####################################################################
# Recode some species - based on notes found in Habitat Mapping database
# These species were either only recorded in 2013 or recorded as a species group in later surveys
# Comments explain what observations are being recoded to and counts of dive sites with presence
spp$Species[spp$Species=="AB" & spp$SpType=="I"] <- "OB"  # Acorn barnacle = Barnacle - other --- chk yrs
spp$Species[spp$Species=="BB" & spp$SpType=="I"] <- "BRF" # Blue encrusting bryozoan(2) = Bryozoan flat
spp$Species[spp$Species=="BS" & spp$SpType=="I"] <- "SPE" # Boot sponge = Sponge erect
spp$Species[spp$Species=="TS" & spp$SpType=="I"] <- "SPE" # Trumpet sponge = Sponge erect
spp$Species[spp$Species=="GS" & spp$SpType=="I"] <- "SPE" # Glove sponge = Sponge erect
spp$Species[spp$Species=="HS" & spp$SpType=="I"] <- "SH"  # Humpback shrimp(4) = Shrimp
spp$Species[spp$Species=="NS" & spp$SpType=="I"] <- "SH"  # Coonstripe shrimp(13) = Shrimp
spp$Species[spp$Species=="SN" & spp$SpType=="I"] <- "SH"  # Spot prawn(2) = Shrimp
spp$Species[spp$Species=="SW" & spp$SpType=="I"] <- "SP"  # Sea whip(1) = sea pen
spp$Species[spp$Species=="FT" & spp$SpType=="I"] <- "SL"  # Chelyosoma productum(1) = tunicate - solitary - other

n <- unique(spp$Species)
length(n)

# Non-target species
spp$Species[spp$Species=="LP" & spp$SpType=="I"] <- "nontarget"  # Six-armed star(6) = " "
spp$Species[spp$Species=="BC" & spp$SpType=="I"] <- "nontarget"  # Butter clam (Saxidomus gigantea)(5) = ?
spp$Species[spp$Species=="CO" & spp$SpType=="I"] <- "nontarget"  # Cockle (Clinocardium nuttallii)(7) = ?
spp$Species[spp$Species=="XH" & spp$SpType=="I"] <- "nontarget"  # Stevens' hermit crab(5) = ?
spp$Species[spp$Species=="KL" & spp$SpType=="I"] <- "nontarget"  # Limpets(4) - removed
spp$Species[spp$Species=="TW" & spp$SpType=="I"] <- "nontarget"  # Calcareous tubeworm(36) - removed b/c ubiquitous
spp$Species[spp$Species=="DA" & spp$SpType=="I"] <- "nontarget"  # Nudibranch (Dirona albolineata) - removed
spp$Species[spp$Species=="OD" & spp$SpType=="I"] <- "nontarget"  # Nudibranch (Dirona pellucita) - removed
spp$Species[spp$Species=="CN" & spp$SpType=="I"] <- "nontarget"  # Nudibranch (Hermissenda crassicornis) - removed
spp$Species[spp$Species=="RT" & spp$SpType=="I"] <- "nontarget"  # Brown turban --- chk yrs
spp$Species[spp$Species=="PC" & spp$SpType=="I"] <- "nontarget"  # Psolus --- chk yrs
spp$Species[spp$Species=="SY" & spp$SpType=="I"] <- "nontarget"  # Spiny red star --- chk yrs
spp$Species[spp$Species=="WU" & spp$SpType=="I"] <- "nontarget"  # White urchin --- chk yrs

n <- unique(spp$Species)
length(n)

# Species codes that are likely meant to be algae not inverts
spp$SpType[spp$Species=="AC" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species=="AF" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species=="BF" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species=="BH" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species=="BO" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species=="CL" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species=="DB" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species=="GI" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species=="GR" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species=="LA" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species=="PH" & spp$SpType=="I"] <- "A"
spp$SpType[spp$Species=="DG" & spp$SpType=="I"] <- "A"

# Typos that were removed, do not correspond with any code
spp$Species[spp$Species=="GSC" & spp$SpType=="I"] <- "nontarget"
spp$Species[spp$Species=="JL" & spp$SpType=="I"] <- "nontarget"
spp$Species[spp$Species=="PY" & spp$SpType=="I"] <- "nontarget"
spp$Species[spp$Species=="TH" & spp$SpType=="I"] <- "nontarget"
spp$Species[spp$Species=="UA" & spp$SpType=="I"] <- "nontarget"
spp$Species[spp$Species=="YZ" & spp$SpType=="I"] <- "nontarget"
spp$Species[spp$Species=="MO" & spp$SpType=="I"] <- "nontarget"
spp$Species[spp$Species=="PG" & spp$SpType=="I"] <- "nontarget"

# Typo that was not feasibly observed at depth 
spp$Species[spp$Species=="OR"& spp$SpType=="I"] <- "nontarget" #Pacific Oyster

n <- unique(spp$Species)
length(n)

#############################################################
# 3. Recode some algae observations to match the data sheets
#############################################################
# Non-target species
spp$Species[spp$Species=="AG" & spp$SpType=="A"] <- "nontarget"  # Agarum # OR is this a typo --- Red turban snail?
spp$Species[spp$Species=="AL" & spp$SpType=="A"] <- "nontarget"  # Alaria 
spp$Species[spp$Species=="BO" & spp$SpType=="A"] <- "nontarget"  # Bossiella 
spp$Species[spp$Species=="DE" & spp$SpType=="A"] <- "nontarget"  # Desmarestia 
spp$Species[spp$Species=="EN" & spp$SpType=="A"] <- "nontarget"  # Encrusting 
spp$Species[spp$Species=="LA" & spp$SpType=="A"] <- "nontarget"  # Laminaria 
spp$Species[spp$Species=="OP" & spp$SpType=="A"] <- "nontarget"  # Opuntella 
spp$Species[spp$Species=="BP" & spp$SpType=="A"] <- "nontarget"  # Botryocladia pseudodichotoma
spp$Species[spp$Species=="DG" & spp$SpType=="A"] <- "nontarget"  # Derbesia marina

n <- unique(spp$Species)
length(n)

# Species codes that are likely meant to be inverts not algae
# If they are non-target invertebrates, update the Species too
spp$SpType[spp$Species=="BA" & spp$SpType=="A"] <- "I" # Brooding anemone
spp$SpType[spp$Species=="DC" & spp$SpType=="A"] <- "I" # Dungeness crab
spp$SpType[spp$Species=="HF" & spp$SpType=="A"] <- "I" # Hydrocoral
spp$SpType[spp$Species=="OB" & spp$SpType=="A"] <- "I" # Other barnacle
spp$SpType[spp$Species=="PU" & spp$SpType=="A"] <- "I" # Purple urchin 
spp$SpType[spp$Species=="RS" & spp$SpType=="A"] <- "I" # Rock scallop
spp$SpType[spp$Species=="SI" & spp$SpType=="A"] <- "I" # Swimming anemone

# Observations that are likely typos
spp$Species[spp$Species=="AN" & spp$SpType=="A"] <- "nontarget"
spp$Species[spp$Species=="AT" & spp$SpType=="A"] <- "nontarget"
spp$Species[spp$Species=="BR" & spp$SpType=="A"] <- "nontarget"
spp$Species[spp$Species=="DR" & spp$SpType=="A"] <- "nontarget"
spp$Species[spp$Species=="FY" & spp$SpType=="A"] <- "nontarget"
spp$Species[spp$Species=="LD" & spp$SpType=="A"] <- "nontarget"
spp$Species[spp$Species=="LM" & spp$SpType=="A"] <- "nontarget"
spp$Species[spp$Species=="NE" & spp$SpType=="A"] <- "nontarget"
spp$Species[spp$Species=="RG" & spp$SpType=="A"] <- "nontarget"
spp$Species[spp$Species=="RM" & spp$SpType=="A"] <- "nontarget"
spp$Species[spp$Species=="SY" & spp$SpType=="A"] <- "nontarget"
spp$Species[spp$Species=="*" & spp$SpType=="A"] <- "nontarget"
spp$Species[spp$Species=="SU" & spp$SpType=="A"] <- "nontarget"

n <- unique(spp$Species)
length(n)

# Remove nontarget species codes
spp <- filter( spp, Species!="nontarget" )

# Trim white space around Species code
spp$Species <- trimws(spp$Species)

# Create invert observations and save
invert <- dplyr::filter( spp, SpType=="I")
write.csv(invert, "./Data/UpdatedObservations/InvertObs_updated.csv")

# Create invert observations and save
algae <- dplyr::filter( spp, SpType=="A")
write.csv(algae, "./Data/UpdatedObservations/AlgaeObs_updated.csv")

# Add Invert/Algae to species code field
spp$Species_Code <- paste0(spp$SpType,"_",spp$Species)

# Save updated species observations
write.csv(spp, "./Data/UpdatedObservations/SpeciesObs_updated.csv")
cat("Created updated Invert, Alage, Species observations files")

#######################################################################################################################
# Benthic Habitat Mapping Dive Survey Data Summary --- Build In Situ Observations for depth, slope, and substrate
# 
# Objective:  Build a matrix of environmental variables collected at the quadrat level by divers
#
# Requires:   Run 1.ExtractDataFromMSAccess.R script
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
UsePackages( pkgs=c("dplyr","tidyr", "plyr","reshape","reshape2","rgdal") ) 

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
cat("Select extracted quadrat file")
myFile <- choose.files(caption = "Select extracted quadrat file") # "./Data/ExtractedData/Quadrat.csv", header=T, sep="," )
quad <- read.csv(myFile, header=T, sep=",")
quad <- dplyr::select(quad, HKey, Quadrat, CorDepthM, DepthCat, Substrate1, Sub1Pct, Substrate2, Sub2Pct, Substrate3, Sub3Pct )

### 2. Calculate mean depth & slope for each quadrat ###
########################################################
depth <- dplyr::select(quad, HKey, Quadrat, CorDepthM)

# Set start depth as the recorded depth from the previous quadrat, unless the quadrat is number 0
depth$StartDepth <- ifelse( depth$Quadrat==0, depth$CorDepthM,lag(depth$CorDepthM, n=1)  )
depth$EndDepth <- depth$CorDepthM

# Calculate mean depth for each quadrat
depth$Depth.m <- (depth$StartDepth+depth$EndDepth)/2

# Calculate slope for each quadrat using the arc-tangent
depth$Elev.Diff <- depth$StartDepth-depth$EndDepth
depth$Slope <- atan2(depth$Elev.Diff,5) 

# Remove unnecessary columns
depth <- dplyr::select(depth, HKey, Quadrat, Depth.m, Slope)

# Remove quadrat 0 
depth <- filter(depth, Quadrat!=0)

# What to do with records with slope greater than 1.0 or less than -1.0??? There are 5 cases in the dataset.
# Set slope to either 1.0 or -1.0 to correct for typo or instances where the swell exacerbated the difference 
# between two height recordings or where divers swam beyond the transect line and incorrectly guestimated 
# 5 m quadrat length
depth$Slope[depth$Slope > 1.0] <- 1.0
depth$Slope[depth$Slope < -1.0] <- -1.0  
  
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
                                c("0"="Wd.Bark","1"="Bdrk.smth","2"="Bdrk.crv","3"="Boulders",
                                  "4"="Cobble","5"="Gravel","6"="Pea.Gravel","7"="Sand","9"="Mud",
                                  "10"="Crsh.Shell","11"="Chnk.Shell"))
# Remove empty records (locations with only primary or secondary substrates recorded)
all.sub <- na.omit(all.sub)
# Cast to a wide table with one column for each possible substrate code
sub.cast <- dcast(all.sub, HKey + Quadrat ~ Substrate, fun.aggregate = sum, value.var = "SubPct" )

### 4. Build new table with all insitu observations ###
#######################################################
sub.quad <- dplyr::full_join(depth, sub.cast, by=c("HKey", "Quadrat"))

# Save in situ observations
write.csv(sub.quad, "./Data/ExtractedData/InSituObs.csv",row.names = F)

### 5. Summarise calculations for site X species matrix ###
###########################################################
cat("Select crosswalk file between quadrats and sites")
myFile <- choose.files(caption = "Select crosswalk file between quadrats and sites") # "./Data/ExtractedData/Quadrat.csv", header=T, sep="," )
keys <- read.csv(myFile, header=T, sep=",")

# Join crosswalk to sub by quadrat, remove HKey and Quadrat fields
df <- dplyr::left_join(keys, sub.quad, by=c("HKey", "Quadrat"))
summary(df)
df[is.na(df)] <- 0
df <- df[,-c(1:3)]

# Summarise by spatial ID
df.ID <- as.data.frame(df %>% 
                         group_by(ID) %>% 
                         summarise_each(list(mean)) %>%
                         mutate_at(vars(-ID,-Slope),funs(round(.,0))))
head(df.ID,3)

### 6. Join to shapefile ###
############################
# Load the data
shppath <- file.path("C:/Users/daviessa/Documents/CURRENT PROJECTS/Community Assemblages/Data/Final")
# shpname <- list.files(path = shppath, pattern = ".shp$")
# shpname <- sub(".shp","",shpname)
shpname <- "final_bhm"
shp <- readOGR( dsn=shppath, layer=shpname )
head(shp@data,3)
all.env.shp <- merge(shp, df.ID, by = "ID")
head(all.env.shp@data,3)

# Save as a shp
dsn <- "C:/Users/daviessa/Documents/CURRENT PROJECTS/Community Assemblages/Data/Final"
layer <- "REAL_final_bhm"
writeOGR(all.env.shp,dsn=dsn,layer=layer, driver="ESRI Shapefile", overwrite=T)
filename <- paste0(dsn,"/bhm4analysis.csv")

# Add coordinate columns and save as a csv
coords <- all.env.shp@coords
df.final <- cbind(all.env.shp@data,coords)
head(df.final,3)
write.csv(df.final,filename,row.names = F)

cat("Fini!")

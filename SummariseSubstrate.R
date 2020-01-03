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
# Date:       October 1, 2019
######################################################################################################################

#### Start up tasks ####
# Start fresh
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
UsePackages( pkgs=c("data.table","tidyverse") ) 

cat("Summarising substrate observations...","\n")

### 1. Read input data ###
##########################
# Read in tables that were extracted from the MS Access db using the script ExtractDataFromMSAccess.R
cat("Select extracted quadrat file","\n")
# myFile <- choose.files(caption = "Select updated quadrat file with RMSM fields") # "./Data/ExtractedData/Quadrat_RMSM.csv", header=T, sep="," )
# quad <- read.csv(myFile, header=T, sep=",")

#quad <- read.csv( "./Data/ExtractedData/Quadrat.csv", header=T, sep="," )
quad <- read.csv( "./Data/UpdatedObservations/Quadrat_RMSM.csv", header=T, sep="," )

### 2. Calculate mean depth & slope for each depth category ###
###############################################################
depth <- dplyr::select(quad, HKey, Quadrat, CorDepthM, DepthCat, RMSM.cat)

# Set start depth as the recorded depth from the previous quadrat, unless the quadrat is number 0
depth$StartDepth <- ifelse( depth$Quadrat==0, depth$CorDepthM,lag(depth$CorDepthM, n=1)  )
depth$EndDepth <- depth$CorDepthM

# Calculate the change in elevation for each quadrat using the arc-tangent
depth$qElev.Diff <- abs(depth$StartDepth-depth$EndDepth)
depth <- dplyr::filter(depth, Quadrat!=0)
depth <- dplyr::select(depth, HKey, DepthCat, CorDepthM, qElev.Diff, RMSM.cat)

# Summarise depth and elevation change for each transect/Depth category combination
df <- as.data.frame(depth %>%
          group_by(HKey, DepthCat) %>%
          dplyr::select(HKey, DepthCat,CorDepthM,qElev.Diff,RMSM.cat) %>%
          dplyr::summarise(Depth = mean(CorDepthM),
                    dElev.Diff = mean(qElev.Diff),
                    substrt = median(RMSM.cat)))


# Recode some of the substrate categories *** TO DO: Check if recoding is logical ***
df$substrt[df$substrt==1.5] <- 2 # Rock + mixed = mixed
df$substrt[df$substrt==2.5] <- 2 # Mixed + sand = mixed
df$substrt[df$substrt==3.5] <- 4 # Sand + mud = mud


# What to do with records with slope greater than 1.0 or less than -1.0??? There are 5 cases in the dataset.
# Set slope to either 1.0 or -1.0 to correct for typo or instances where the swell exacerbated the difference 
# between two height recordings or where divers swam beyond the transect line and incorrectly guestimated 
# 5 m quadrat length
df$dElev.Diff[depth$dElev.Diff > 1.0] <- 1.0
df$dElev.Diff[depth$dElev.Diff < -1.0] <- -1.0

df <- round(df, digits=2)
#df[1:2] <- sapply(df[1:2],as.character)

cat("Calculating mean depth...","\n")

# Save depth category elevation and slope summaries
write.csv(df, "./Data/UpdatedObservations/DepthCat_summaries.csv", row.names = F)

# ### 3. Summarise substrate for each quadrat ###
# ###############################################
# substrt <- dplyr::select(quad, HKey, Quadrat, DepthCat, Substrate1, Sub1Pct, Substrate2, Sub2Pct, Substrate3, Sub3Pct)
# substrt$HKey <- as.character(substrt$HKey)
# substrt$DepthCat <- as.character(substrt$DepthCat)
# 
# # Remove quadrat 0 b/c it has no substrate records
# substrt <- filter(substrt, Quadrat!=0)
# 
# # Separate data for each substrate field
# primary <- dplyr::select(substrt, HKey, Quadrat, DepthCat, Substrate1, Sub1Pct)
# names(primary)[names(primary)=="Substrate1"] <- "Substrate"
# names(primary)[names(primary)=="Sub1Pct"] <- "SubPct"
# secondary <- dplyr::select(substrt, HKey, Quadrat, DepthCat, Substrate2, Sub2Pct)
# names(secondary)[names(secondary)=="Substrate2"] <- "Substrate"
# names(secondary)[names(secondary)=="Sub2Pct"] <- "SubPct"
# tertiary <- dplyr::select(substrt, HKey, Quadrat, DepthCat, Substrate3, Sub3Pct)
# names(tertiary)[names(tertiary)=="Substrate3"] <- "Substrate"
# names(tertiary)[names(tertiary)=="Sub3Pct"] <- "SubPct"
# 
# # Combine rows into one long and narrow table
# sub.123 <- bind_rows(primary,secondary,tertiary)
# # Recode to substrate types
# sub.123$Substrate <- as.factor(sub.123$Substrate)
# sub.123$Substrate <- plyr::revalue(sub.123$Substrate,
#                                 c("0"="Wd.Bark","1"="Bdrk.smth","2"="Bdrk.crv","3"="Boulders",
#                                   "4"="Cobble","5"="Gravel","6"="Pea.Gravel","7"="Sand","9"="Mud",
#                                   "10"="Crsh.Shell","11"="Chnk.Shell"))
# 
# # Remove empty records (locations with only primary or secondary substrates recorded)
# sub.123 <- filter(sub.123, SubPct!='0')
# sub.123 <- sub.123[complete.cases(sub.123[ , 4]),]
# 
# mydf <- data.table(sub.123)
# head(mydf)
# 
# # Spread data to a wide table with one column for each possible substrate code
# #sub.cast <- reshape2::dcast(sub.123, HKey + DepthCat + Quadrat ~ Substrate, fun.aggregate = sum, value.var = "SubPct" ) # reshape2 has been retired! 
# mydf <- dcast(mydf, HKey + DepthCat ~ Substrate, value.var="SubPct", fun=mean)
# 
# # Set na to 0
# mydf[is.na(mydf)] <- 0
# mydf <- as.data.frame(mydf)
# mydf[3:13] <- round(mydf[3:13], digits=0)
# 
# # Determine dominant substrates, recode all substrates less than 25%
# threshold <- 40
# mydf[3:13][ mydf[3:13] <= threshold ] <- 0
# 
# cat("Calculating substrate greater than", threshold,"\n")
# 
# # Save as csv
# write_csv(mydf, "./Data/UpdatedObservations/Quadrat_CalcSub.csv")

### 4. Build new table with all observations ###
####################################################### 

#**** With RMSM substrate category ****#
final <- left_join(df, quad, by=c("HKey","DepthCat"))
#final[is.na(final)] <- 0
final$TransDepth <- paste0(final$HKey,"_",final$DepthCat)
final <- final[,c(21,3,4,15,16)]

cat("Saving final substrate csv with RMSM","\n")
write_csv(final, "./Data/UpdatedObservations/DepthCat_CalcSub_RMSM.csv")




#**** With 11 different substrate categories ****#
# final <- left_join(df, mydf, by=c("HKey","DepthCat"))
# final[is.na(final)] <- 0
# final$TransDepth <- paste0(df$HKey,"_",df$DepthCat)
# final <- final[c(16,1:15)]
# 
# cat("Saving final substrate csv","\n")
# write_csv(final, "./Data/UpdatedObservations/DepthCat_CalcSub.csv")





# ### 5. Summarise calculations for site X species matrix ###
# ###########################################################
# cat("Select crosswalk file between quadrats and sites")
# myFile <- choose.files(caption = "Select crosswalk file between quadrats and sites") # "./Data/ExtractedData/Quadrat.csv", header=T, sep="," )
# keys <- read.csv(myFile, header=T, sep=",")
# 
# # Join crosswalk to sub by quadrat, remove HKey and Quadrat fields
# df <- dplyr::left_join(keys, sub.quad, by=c("HKey", "Quadrat"))
# summary(df)
# df[is.na(df)] <- 0
# df <- df[,-c(1:3)]
# 
# # Summarise by spatial ID
# df.ID <- as.data.frame(df %>% 
#                          group_by(ID) %>% 
#                          summarise_each(list(mean)) %>%
#                          mutate_at(vars(-ID,-Slope),funs(round(.,0))))
# head(df.ID,3)
# 
# ### 6. Join to shapefile ###
# ############################
# # Load the data
# shppath <- file.path("C:/Users/daviessa/Documents/CURRENT PROJECTS/Community Assemblages/Data/Final")
# # shpname <- list.files(path = shppath, pattern = ".shp$")
# # shpname <- sub(".shp","",shpname)
# shpname <- "final_bhm"
# shp <- readOGR( dsn=shppath, layer=shpname )
# head(shp@data,3)
# all.env.shp <- merge(shp, df.ID, by = "ID")
# head(all.env.shp@data,3)
# 
# # Save as a shp
# dsn <- "C:/Users/daviessa/Documents/CURRENT PROJECTS/Community Assemblages/Data/Final"
# layer <- "REAL_final_bhm"
# writeOGR(all.env.shp,dsn=dsn,layer=layer, driver="ESRI Shapefile", overwrite=T)
# filename <- paste0(dsn,"/bhm4analysis.csv")
# 
# # Add coordinate columns and save as a csv
# coords <- all.env.shp@coords
# df.final <- cbind(all.env.shp@data,coords)
# head(df.final,3)
# write.csv(df.final,filename,row.names = F)
# 
# cat("Fini!")

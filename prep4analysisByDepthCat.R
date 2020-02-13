# Build species matrix for depth category analysis & substrate analysis



# Start fresh
rm(list=ls())

# Load packages
#--------------
# library(mapview)
library(tidyverse)
library(rstudioapi)
# library(vegan)

# Inputs
#-------

analysis <- "DepthCat"
outdir <- analysis

# Read in species by regions
#---------------------------
sppByRegion <- readRDS("C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep/Data/RDS/sppByRegion.AllBC.rds")
#sppDF <- readRDS("C:/Users/daviessa/Documents/R/PROJECTS_MY/DiveSurveys_DataPrep/Data/RDS/sppByRegion_Dataframe.rds")

# Determine the number of elements within the site x species list
nElements <- length(sppByRegion)

# Get path for this script
#-------------------------
# Set working directory to one above script directory
current_path <- getActiveDocumentContext()$path 
setwd(dirname(current_path ))

# Source functions
#-----------------
#source('CommunitySummaries_functions.R')

# Set up new directory for all results
#-------------------------------------
setwd( "../Data") 
if (dir.exists(outdir)){
  setwd(outdir)
} else{
  dir.create(path = outdir)
  setwd(outdir)
  # dir.create(dsn)
}
getwd()

# Get depth category from row name (TransDepth)
#----------------------------------------------
# Extract depth from TransDepth (row name)
addDepthCat <- function(df){
  df <- df %>% 
    mutate(TransDepth = row.names(df)) %>% 
    mutate(cl = as.integer(str_sub(row.names(df), -1)) )
  return(df)
}

# Create new list of species matrices
sppByDepthCat <- lapply(sppByRegion, addDepthCat)
saveRDS( sppByDepthCat, "speciesFulldepth.RDS" )

# Build a new list of depth observations
cl.list <- vector("list",nElements)
for (i in 1:length(sppByDepthCat)){
  names(cl.list)[i] <- names(sppByDepthCat)[i]
  cl.list[[i]] <- sppByDepthCat[[i]]$cl
}

# Convert list element to a dataframe
make_df <- function(x){
  as.data.frame(table(x))
}
depthcounts <- lapply(cl.list, make_df )

# Set column names
c.names <- c("cl","Freq")
depthcounts <- lapply(depthcounts, setNames, c.names)
depthcounts

# Order groups by frequency 
order.cl <- function(x){
  order <- data.frame( order=seq( 1:nrow(x) ),x[order(-x$Freq),] ) 
  order$cumsum <- cumsum( order$Freq )
  order$cumpercent <- round( order$cumsum/max(order$cumsum), 2 )
  order$percent <- round( order$Freq/sum(order$Freq),2 )
  return(order)
}
 
cluster.frq <- lapply( depthcounts, order.cl )
saveRDS( cluster.frq, "cluster.freq.RDS" )
cluster.frq
 
  

  

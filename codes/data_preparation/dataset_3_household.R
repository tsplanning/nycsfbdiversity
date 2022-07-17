##################################################################
# Section 3. Create Household Dataset
# Date: 2022/07/08
##################################################################

# Open Libraries
library(dplyr)
library(nnet)
library(ggplot2)
library(effects)
library(broom)
library(stringr)
library(purrr)
library(tidyr)
library(stargazer)
library(car)
library(fastDummies)
library(fst)
library(data.table)
library(tidycensus)
library(ggplot2)
library(scales)
library(tigris)
library(sf)
library(spData)
library(rgdal)

#* Section 1. Open the household csv file -----------------------

read_hh_func <- function(counties, region) {

# Set working directory
setwd("~/data/raw/dataaxel/")

# Load IDs of households in study areas
for(year in 2013:2019){
  cat("Processing", year, "FAMILYIDs...\n")
  ig <- read_fst(paste0("US_Consumer_5_File_", year, ".fst"),
                 columns = c("FAMILYID", "GE_CENSUS_STATE_2010", "GE_CENSUS_COUNTY")) %>%
    ## Filter to specific counties
    filter(paste0(str_pad(GE_CENSUS_STATE_2010, 2, "left", "0"),
                  str_pad(GE_CENSUS_COUNTY, 3, "left", "0")) %in% counties)
  setattr(ig$FAMILYID, "class", "integer64")
  ## Create list of FAMILYIDs
  if(exists("familyids")){
    familyids <<- c(familyids, ig %>% pull(FAMILYID))
  } else {
    familyids <<- ig %>% pull(FAMILYID)
  }
}

print("read!")

# Just pulls the FAMILYID ... and the STATE AND COUNTY indicators from the entire nation
# Then Filters down to ones that are in specific county you want - 
# Creates a list of all FAMILYIDs that have in SF County at some point during 2006:2019
# Then we're adding it to the FAMILYIDs Object

## Load full dataset for households listed in `familyids`
for(year in 2013:2019){
  cat("Processing", year, "infogroup data...\n")
  ig <- read_fst(paste0("US_Consumer_5_File_", year, ".fst"),
                 ## Add additional fields as desired
                 columns = c("FAMILYID", "GE_CENSUS_STATE_2010",
                             "STREET_NAME", "GE_LONGITUDE_2010", "GE_LATITUDE_2010", 
                             "last_name_1", "Ethnicity_Code_1",
                             "FIND_DIV_1000", "OWNER_RENTER_STATUS", "LOCATION_TYPE", "HEAD_HH_AGE_CODE",
                             "MARITAL_STATUS", "CHILDREN_IND", "LENGTH_OF_RESIDENCE")) %>%
    ## Remove records without street name (bad geocodes), and filter to specified FAMILYIDs
    filter(STREET_NAME != "" & FAMILYID %in% familyids) %>%
    mutate(YEAR = year)
  setattr(ig$FAMILYID, "class", "integer64")
  if(exists("panel")){
    panel <<- bind_rows(panel, ig)
  } else {
    panel <<- ig
  }
  rm(ig)
}

print("complete!")

# Set working directory
setwd("~/data/projects/segregation/datasets/")

write.csv(panel, paste0("hh_raw_", region, ".csv"))

print("exported!")

}

#* Section 2. Run the functions -----------------------

# Define counties
counties_sfb <- c("06075", "06001", "06013", "06041", "06081")
counties_nyc <- c("36005", "36047", "36061", "36081", "36085")

# read_hh_func(counties_sfb, "sfb")
read_hh_func(counties_nyc, "nyc")

#* Section 3. Create nationwide block group boundaries shp file -----------------------

# Create State Codes

state_code <- c(1, 2, 4, 5, 6, 8, 9, 10, 11, 12, 13, 15, 16, 
                17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29,
                30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42,
                44, 45, 46, 47, 48, 49, 50, 51, 53, 54, 55, 56, 60, 66, 69, 72, 78)

state_code <- as.character(state_code)

#** A. Create Block group-level shapefiles

# Set an empty list
 data_list <- list()
 i <- 1
 
# Create a for-loop to import block group geographies
for (x in state_code) {

  blkgrp <- block_groups(state = x, cb= FALSE, year = "2013")

  # Select the required variables only
  blkgrp <- blkgrp %>% select(GEOID, geometry)

  data_list[[i]] <- blkgrp
   
  i <- i + 1  
  print(x)
  
  }

# Bind the lists by row
blkgrp_shp <- do.call(rbind, data_list)

# Write the nationwide block group-level shp file 
sf::st_write(blkgrp_shp, "blkgrp_nationwide.shp", driver = "ESRI Shapefile", append=FALSE)
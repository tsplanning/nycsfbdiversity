##################################################################
# Section 4. Create geocodes
# Date: 2022/07/07
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
library(data.table)
library(scales)
library(tigris) 
library(sf)
library(vctrs)

# Set working directory
setwd("~/data/projects/segregation/datasets")

blkgrp_shp <- sf::st_read("blkgrp_nationwide.shp")

# Create a function for assigining GEOID to each row
geocode_func <- function(region) {

# Read the datasets
hh <- read.csv(paste0("hh_raw_", region, ".csv"))

# Check the coordinate system
st_crs(blkgrp_shp) # NAD83 / GRS 1980 / EPSG, 4269

# Check rows without location values
sum(is.na(hh$GE_CENSUS_STATE_2010)) # 0
sum(is.na(hh$GE_LONGITUDE_2010)) # 0
sum(is.na(hh$GE_LATITUTDE_2010)) # 0

# Open state fips code csv and merge it with the infogroup data
# state_fips <- read.csv("state_code.csv")
# sf_hh <- left_join(sf_hh, state_fips, by=c("GE_CENSUS_STATE_2010" = "state_code"), all.x=TRUE)

# Create a shp file using long/latitude and assign coordinate reference system (NAD83)
hh_shp <- st_as_sf(hh, coords = c("GE_LONGITUDE_2010", "GE_LATITUDE_2010"), crs=4269)

hh_joined <- st_join(hh_shp, blkgrp_shp, left=TRUE)

glimpse(hh_joined)

# Check how many GEOIDs are missing from the newly created shp file
sum(is.na(hh_joined$GEOID==TRUE))

# Remove the geometry column and convert into a dataframe
hh_final <- hh_joined %>%
  data.frame() %>%
  select(-c(X, geometry))

glimpse(hh_final)

write.csv(hh_final, paste0("hh_geocoded_", region, ".csv"))

}

geocode_func("sfb")
geocode_func("nyc")

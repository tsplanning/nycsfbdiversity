##################################################################
# Section 1. Create Segregation Variables
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

# census_api_key <- census_api_key("2de85de290d0a4f17ea6f91ff2263ceec8ae9dc5", install = TRUE, overwrite=TRUE)
Sys.getenv("CENSUS_API_KEY")

# Set working directory
setwd("~/data/projects/segregation/datasets")

############################################################
############################################################

# Write a function for producing the segregation dataset
segregation_func <- function(state, counties, region) {

#* Section 1-1. Create Income Segregation Index -------------------- 

#** A. Read Income Cutoff Data -----------------------------------

# Read the income cutoffs for each city
if (region=="nyc") { wts <- read.csv("weights_nyc.csv")
} else if (region=="sfb") { wts <- read.csv("weights_sfb.csv") }

wts <- wts %>%
  mutate(county_id = ifelse(nchar(county_id)==4,
          as.character(paste0("0", county_id)), as.character(county_id)))

# Find the closest matching value in the census income categories
census_inc_category <- c(10000, 14999, 19999, 24999, 29999, 34999, 39999, 44999,
                          49999, 59999, 74999, 999999, 124999, 14999, 19999, 200000)

# Locate the indices for the closest matching values
wts$AMI80_loc <- sapply(wts$AMI80, function(x) which.min(abs(census_inc_category - x)))
wts$AMI120_loc <- sapply(wts$AMI120, function(x) which.min(abs(census_inc_category - x)))
wts$AMI165_loc <- sapply(wts$AMI165, function(x) which.min(abs(census_inc_category - x)))

#** A. Block Groups ------------------------------------------------

# Load the income and race variables from ACS
 data_list <- list()
 i <- 1
 
 for (yr in 2013:2019) {
   inc <- get_acs(
     geography = "block group",
     table = "B19001",
     state = state,
     county = counties,
     year = yr,
     survey = "acs5",
    cache_table = TRUE
   )
   
   data_list[[i]] <- inc
   i <- i + 1  
 }

all_data <- bind_rows(data_list, .id = "column_label")

# Convert into a long format
data_wide <- spread(all_data, key = "variable", value = "estimate")

# Unlist the data and add variables for calculation
acs_income <- data_wide %>%
  data.frame() %>%
  rename(blkgrp_id = GEOID) %>%
  mutate(YEAR = as.integer(column_label) + 2012,
         county_id = substr(blkgrp_id, 1, 5)) %>%
  select(-c(column_label)) %>%
  left_join(wts, by=c("YEAR", "county_id")) %>%
  group_by(blkgrp_id, YEAR) %>%
  summarize(inc_low = sum(c_across(B19001_002
          :as.symbol(paste0("B19001_0",
            ifelse(nchar(AMI80_loc[1]-1)==1, paste0("0", as.character(AMI80_loc[1]-1)),
            as.character(AMI80_loc[1]-1))))), na.rm=TRUE),
    inc_moderate = sum(c_across(as.symbol(paste0("B19001_0",
            ifelse(nchar(AMI80_loc[1])==1, paste0("0", as.character(AMI80_loc[1])),
            as.character(AMI80_loc[1]))))
          :as.symbol(paste0("B19001_0",
            ifelse(nchar(AMI120_loc[1]-1)==1, paste0("0", as.character(AMI120_loc[1]-1)),
            as.character(AMI120_loc[1]-1))))), na.rm=TRUE),
    inc_middle = sum(c_across(as.symbol(paste0("B19001_0",
            ifelse(nchar(AMI120_loc[1])==1, paste0("0", as.character(AMI120_loc[1])),
            as.character(AMI120_loc[1]))))
          :as.symbol(paste0("B19001_0",
            ifelse(nchar(AMI165_loc[1]-1)==1, paste0("0", as.character(AMI165_loc[1]-1)),
            as.character(AMI165_loc[1]-1))))), na.rm=TRUE),
    inc_high = sum(c_across(as.symbol(paste0("B19001_0",
            ifelse(nchar(AMI165_loc[1])==1, paste0("0", as.character(AMI165_loc[1])),
      as.character(AMI165_loc[1]))))
      :B19001_017), na.rm=TRUE)) %>%
  mutate(blkgrp_pop = inc_low+inc_moderate+inc_middle+inc_high) %>%
  mutate(inc_high_pct = inc_high/blkgrp_pop,
         inc_middle_pct = inc_middle/blkgrp_pop,
         inc_moderate_pct = inc_moderate/blkgrp_pop,
         inc_low_pct = inc_low/blkgrp_pop) %>%
  mutate(entropy_income =
      (4*inc_low_pct*(1-inc_low_pct) +
      4*(inc_low_pct+inc_moderate_pct)*(1-inc_low_pct-inc_moderate_pct) +
      4*(inc_low_pct+inc_moderate_pct+inc_middle_pct)*(1-inc_low_pct-inc_moderate_pct-inc_middle_pct))/3) %>%
    arrange(blkgrp_id, YEAR) %>%
    ungroup()

#** B. Census Tracts ---------------------------------------------

 data_list <- list()
 i <- 1
 
 for (yr in 2013:2019) {
   
   inc <- get_acs(
     geography = "tract",
     table = "B19001",
     state = state,
     county = counties,
     year = yr,
     survey = "acs5",
    cache_table = TRUE
   )
   
   data_list[[i]] <- inc
   
   i <- i + 1  
 }

all_data <- bind_rows(data_list, .id = "column_label")

# Convert into a long format
data_wide <- spread(all_data, key = "variable", value = "estimate")

# Unlist the data and add variables for calculation
acs_income_tract <- data_wide %>%
  data.frame() %>%
  rename(tract_id = GEOID) %>%
  mutate(YEAR = as.integer(column_label) + 2012,
         county_id = substr(tract_id, 1, 5)) %>%
  select(-c(column_label)) %>%
  left_join(wts, by=c("YEAR", "county_id")) %>%
  group_by(tract_id, YEAR) %>%
  summarize(inc_low = sum(c_across(B19001_002
          :as.symbol(paste0("B19001_0",
            ifelse(nchar(AMI80_loc[1]-1)==1, paste0("0", as.character(AMI80_loc[1]-1)),
            as.character(AMI80_loc[1]-1))))), na.rm=TRUE),
    inc_moderate = sum(c_across(as.symbol(paste0("B19001_0",
            ifelse(nchar(AMI80_loc[1])==1, paste0("0", as.character(AMI80_loc[1])),
            as.character(AMI80_loc[1]))))
          :as.symbol(paste0("B19001_0",
            ifelse(nchar(AMI120_loc[1]-1)==1, paste0("0", as.character(AMI120_loc[1]-1)),
            as.character(AMI120_loc[1]-1))))), na.rm=TRUE),
    inc_middle = sum(c_across(as.symbol(paste0("B19001_0",
            ifelse(nchar(AMI120_loc[1])==1, paste0("0", as.character(AMI120_loc[1])),
            as.character(AMI120_loc[1]))))
          :as.symbol(paste0("B19001_0",
            ifelse(nchar(AMI165_loc[1]-1)==1, paste0("0", as.character(AMI165_loc[1]-1)),
            as.character(AMI165_loc[1]-1))))), na.rm=TRUE),
    inc_high = sum(c_across(as.symbol(paste0("B19001_0",
            ifelse(nchar(AMI165_loc[1])==1, paste0("0", as.character(AMI165_loc[1])),
      as.character(AMI165_loc[1]))))
      :B19001_017), na.rm=TRUE)) %>%
  mutate(tract_pop = inc_low+inc_moderate+inc_middle+inc_high) %>%
  mutate(inc_high_pct = inc_high/tract_pop,
         inc_middle_pct = inc_middle/tract_pop,
         inc_moderate_pct = inc_moderate/tract_pop,
         inc_low_pct = inc_low/tract_pop) %>%
  mutate(entropy_income =
      (4*inc_low_pct*(1-inc_low_pct) +
      4*(inc_low_pct+inc_moderate_pct)*(1-inc_low_pct-inc_moderate_pct) +
      4*(inc_low_pct+inc_moderate_pct+inc_middle_pct)*(1-inc_low_pct-inc_moderate_pct-inc_middle_pct))/3) %>%
    arrange(tract_id, YEAR) %>%
    ungroup()

# Calculate regional-level entropy
acs_income_city <- acs_income_tract %>%
  group_by(YEAR) %>%
    summarize(inc_high_city = sum(inc_high, na.rm=TRUE),
      inc_middle_city = sum(inc_middle, na.rm=TRUE),
      inc_moderate_city = sum(inc_moderate, na.rm=TRUE),
      inc_low_city = sum(inc_low, na.rm=TRUE),
      city_pop = sum(tract_pop, na.rm=TRUE)) %>%
  mutate(inc_high_pct = inc_high_city/city_pop,
         inc_middle_pct = inc_middle_city/city_pop,
         inc_moderate_pct = inc_moderate_city/city_pop,
         inc_low_pct = inc_low_city/city_pop) %>%
  mutate(entropy_income_city =
      (4*inc_low_pct*(1-inc_low_pct) +
      4*(inc_low_pct+inc_moderate_pct)*(1-inc_low_pct-inc_moderate_pct) +
      4*(inc_low_pct+inc_moderate_pct+inc_middle_pct)*(1-inc_low_pct-inc_moderate_pct-inc_middle_pct))/3) %>%
  select(YEAR, city_pop, entropy_income_city)

#** C. Calculate regional segregation levels --------------------

acs_income_seg <- left_join(acs_income, acs_income_city, by="YEAR")
acs_income_seg_tract <- left_join(acs_income_tract, acs_income_city, by="YEAR")

acs_income_seg_sum <- acs_income_seg %>%
    mutate(ent_diff = entropy_income_city-entropy_income) %>%
    mutate(nominator_1 = blkgrp_pop*ent_diff) %>%
    group_by(YEAR) %>%
    summarize(nominator = sum(nominator_1, na.rm=TRUE),
              city_pop = mean(city_pop, na.rm=TRUE),
              entropy_income_city = mean(entropy_income_city, na.rm=TRUE)) %>%
    mutate(seg_income_blkgrp = nominator/city_pop*entropy_income_city) %>%
    select(YEAR, entropy_income_city, seg_income_blkgrp)

acs_income_seg_sum_tract <- acs_income_seg_tract %>%
    mutate(ent_diff = entropy_income_city-entropy_income) %>%
    mutate(nominator_1 = tract_pop*ent_diff) %>%
    group_by(YEAR) %>%
    summarize(nominator = sum(nominator_1, na.rm=TRUE),
              city_pop = mean(city_pop, na.rm=TRUE),
              entropy_income_city = mean(entropy_income_city, na.rm=TRUE)) %>%
    mutate(seg_income_tract = nominator/city_pop*entropy_income_city) %>%
    select(YEAR, seg_income_tract)

# Export the dataset
write.csv(acs_income_seg_sum, paste0("saved/segregation_income_blkgrp_", region, ".csv"))
write.csv(acs_income_seg_sum_tract, paste0("saved/sf_segregation_income_tract_", region, ".csv"))

#** D. Merge the block group and census tract dataframes --------

# Subset the block_group ID to create census tract code then merge
acs_income$tract_id <- substr(acs_income$blkgrp_id, 1, 11)

acs_income_seg <- left_join(acs_income, acs_income_tract,
                    by=c("tract_id", "YEAR"),
                   suffix = c("_blkgrp", "_tract"),
                    all.x=TRUE)

# Merge the segregation index calculated at the block group and tract-level
acs_income_seg <- left_join(acs_income_seg, acs_income_seg_sum,
                    by=c("YEAR"),
                    all.x=TRUE)

# Replace missing block group ratio values with tract values
acs_income_seg <- acs_income_seg %>%
    mutate(inc_low = ifelse(is.na(inc_low_blkgrp == TRUE), inc_low_tract, inc_low_blkgrp),
          inc_moderate = ifelse(is.na(inc_moderate_blkgrp == TRUE), inc_moderate_tract, inc_moderate_blkgrp),
            inc_middle = ifelse(is.na(inc_middle_blkgrp == TRUE), inc_middle_tract, inc_middle_blkgrp),
                   inc_high = ifelse(is.na(inc_high_blkgrp == TRUE), inc_high_tract, inc_high_blkgrp),
                  inc_low_pct = ifelse(is.na(inc_low_pct_blkgrp == TRUE), inc_low_pct_tract, inc_low_pct_blkgrp),
          inc_moderate_pct = ifelse(is.na(inc_moderate_pct_blkgrp == TRUE), inc_moderate_pct_tract, inc_moderate_pct_blkgrp),
            inc_middle_pct = ifelse(is.na(inc_middle_pct_blkgrp == TRUE), inc_middle_pct_tract, inc_middle_pct_blkgrp),
                   inc_high_pct = ifelse(is.na(inc_high_pct_blkgrp == TRUE), inc_high_pct_tract, inc_high_pct_blkgrp)) %>%
mutate(entropy_income =
      (4*inc_low_pct*(1-inc_low_pct) +
      4*(inc_low_pct+inc_moderate_pct)*(1-inc_low_pct-inc_moderate_pct) +
      4*(inc_low_pct+inc_moderate_pct+inc_middle_pct)*(1-inc_low_pct-inc_moderate_pct-inc_middle_pct))/3) %>%
    arrange(YEAR, blkgrp_id)

acs_income_final <- left_join(acs_income_seg, acs_income_seg_sum_tract,
                    by=c("YEAR"),
                    all.x=TRUE)

# Export the dataset
write.csv(acs_income_final, paste0("saved/acs_income_", region, ".csv"))

#* Section 1-2. Create Racial Segregation Index -------------------- 

#** A. Block Groups ------------------------------------------------

# Load the race variables from ACS
 data_list <- list()
 i <- 1
 
 for (yr in 2013:2019) {
   
   inc <- get_acs(
     geography = "block group",
     table = "B03002",
     state = state,
     county = counties,
     year = yr,
     survey = "acs5",
    cache_table = TRUE
   )
   
   data_list[[i]] <- inc
   
   i <- i + 1  
 }

all_data <- bind_rows(data_list, .id = "column_label")

# Unlist the data and add variables for calculation
acs_race <- all_data %>% 
   data.frame() %>%
   mutate(YEAR = as.integer(column_label) + 2012) %>%
  mutate(race_category_raw = ifelse(variable=="B03002_001", "Total",
    ifelse(variable=="B03002_002", "Not Hispanic or Latino",
    ifelse(variable=="B03002_003", "White alone",
      ifelse(variable=="B03002_004", "Black alone",
        ifelse(variable=="B03002_005", "American Indian and Alaska Native alone",
          ifelse(variable=="B03002_006", "Asian alone",
            ifelse(variable=="B03002_007", "Native Hawaiian and Other Pacific Islander alone",
              ifelse(variable=="B03002_008", "Some other race alone",
                ifelse(variable=="B03002_009", "Two or more races",
                  ifelse(variable=="B03002_010", "Two races including Some other race",
                    ifelse(variable=="B03002_011", "Two races excluding Some other race, and three or more races",
                      ifelse(variable=="B03002_012", "Hispanic or Latino",
                                    NA))))))))))))) %>%
   # 5 Race Categories
  mutate(race_category = ifelse(variable=="B03002_001", "Total",
    ifelse(variable=="B03002_002", "Not Hispanic or Latino",
    ifelse(variable=="B03002_003", "White",
      ifelse(variable=="B03002_004", "Black",
        ifelse(variable=="B03002_005", "Other",
          ifelse(variable=="B03002_006", "Asian",
            ifelse(variable=="B03002_007", "Other",
              ifelse(variable=="B03002_008", "Other",
                ifelse(variable=="B03002_009", "Other",
                  ifelse(variable=="B03002_010", "Other",
                    ifelse(variable=="B03002_011", "Other",
                      ifelse(variable=="B03002_012", "Latinx",
                                    NA))))))))))))) %>%
  filter(race_category != "Total") %>%
  filter(race_category != "Not Hispanic or Latino") %>%
  rename(blkgrp_id = GEOID,
          number = estimate) %>% 
   select(c(YEAR, blkgrp_id, race_category, number))

# Calculate total population by block group & year
acs_race_total <- acs_race %>%
  group_by(YEAR, blkgrp_id) %>%
    summarize(blkgrp_pop=sum(number)) %>%
      data.frame()

acs_race_sum <- acs_race %>%
  group_by(YEAR, blkgrp_id, race_category) %>%
    summarize(sum_number = sum(number)) %>%
      spread(key = race_category, value = sum_number) %>%
      rename(race_white = White, race_black = Black, race_asian = Asian,
            race_latinx = Latinx, race_other = Other)

# Merge total & sum and calculate ratio
acs_race_pct <- merge(acs_race_total, acs_race_sum, by=c("YEAR", "blkgrp_id"))

acs_race_pct <- acs_race_pct %>%
  mutate(race_white_pct = race_white/blkgrp_pop,
         race_black_pct = race_black/blkgrp_pop,
         race_asian_pct = race_asian/blkgrp_pop,
         race_latinx_pct = race_latinx/blkgrp_pop,
         race_other_pct = race_other/blkgrp_pop) %>%
  rowwise() %>%
  mutate(entropy_race =
    sum(race_white_pct*log(1/race_white_pct) +
        race_black_pct*log(1/race_black_pct) +
        race_asian_pct*log(1/race_asian_pct) +
        race_latinx_pct*log(1/race_latinx_pct) +
        race_other_pct*log(1/race_other_pct), na.rm=TRUE)) %>%
    arrange(blkgrp_id, YEAR) %>%
    ungroup()

#** B. Census Tracts ------------------------------------------------

 # Load the race variables from ACS
 data_list <- list()
 i <- 1
 
 for (yr in 2013:2019) {
   
   inc <- get_acs(
     geography = "tract",
     table = "B03002",
     state = state,
     county = counties,
     year = yr,
     survey = "acs5",
    cache_table = TRUE
   )
   
   data_list[[i]] <- inc
   
   i <- i + 1  
 }

all_data <- bind_rows(data_list, .id = "column_label")

# Unlist the data and add variables for calculation
acs_race_tract <- all_data %>% 
   data.frame() %>%
   mutate(YEAR = as.integer(column_label) + 2012) %>%
  mutate(race_category_raw = ifelse(variable=="B03002_001", "Total",
    ifelse(variable=="B03002_002", "Not Hispanic or Latino",
    ifelse(variable=="B03002_003", "White alone",
      ifelse(variable=="B03002_004", "Black alone",
        ifelse(variable=="B03002_005", "American Indian and Alaska Native alone",
          ifelse(variable=="B03002_006", "Asian alone",
            ifelse(variable=="B03002_007", "Native Hawaiian and Other Pacific Islander alone",
              ifelse(variable=="B03002_008", "Some other race alone",
                ifelse(variable=="B03002_009", "Two or more races",
                  ifelse(variable=="B03002_010", "Two races including Some other race",
                    ifelse(variable=="B03002_011", "Two races excluding Some other race, and three or more races",
                      ifelse(variable=="B03002_012", "Hispanic or Latino",
                                    NA))))))))))))) %>%
   # 5 Race Categories
  mutate(race_category = ifelse(variable=="B03002_001", "Total",
    ifelse(variable=="B03002_002", "Not Hispanic or Latino",
    ifelse(variable=="B03002_003", "White",
      ifelse(variable=="B03002_004", "Black",
        ifelse(variable=="B03002_005", "Other",
          ifelse(variable=="B03002_006", "Asian",
            ifelse(variable=="B03002_007", "Other",
              ifelse(variable=="B03002_008", "Other",
                ifelse(variable=="B03002_009", "Other",
                  ifelse(variable=="B03002_010", "Other",
                    ifelse(variable=="B03002_011", "Other",
                      ifelse(variable=="B03002_012", "Latinx",
                                    NA))))))))))))) %>%
  filter(race_category != "Total") %>%
  filter(race_category != "Not Hispanic or Latino") %>%
  rename(tract_id = GEOID,
          number = estimate) %>% 
   select(c(YEAR, tract_id, race_category, number))

# Calculate total population by tract & year
acs_race_total_tract <- acs_race_tract %>%
  group_by(YEAR, tract_id) %>%
    summarize(tract_pop=sum(number)) %>%
      data.frame()

acs_race_sum_tract <- acs_race_tract %>%
  group_by(YEAR, tract_id, race_category) %>%
    summarize(sum_number = sum(number)) %>%
      spread(key = race_category, value = sum_number) %>%
      rename(race_white = White, race_black = Black, race_asian = Asian,
            race_latinx = Latinx, race_other = Other)

# Merge total & sum and calculate ratio
acs_race_pct_tract <- merge(acs_race_total_tract, acs_race_sum_tract, by=c("YEAR", "tract_id"))

acs_race_pct_tract <- acs_race_pct_tract %>%
  mutate(race_white_pct = race_white/tract_pop,
         race_black_pct = race_black/tract_pop,
         race_asian_pct = race_asian/tract_pop,
         race_latinx_pct = race_latinx/tract_pop,
         race_other_pct = race_other/tract_pop) %>%
  rowwise() %>%
  mutate(entropy_race = sum(race_white_pct*log(1/race_white_pct) +
        race_black_pct*log(1/race_black_pct) +
        race_asian_pct*log(1/race_asian_pct) +
        race_latinx_pct*log(1/race_latinx_pct) +
        race_other_pct*log(1/race_other_pct), na.rm=TRUE)) %>%
    arrange(tract_id, YEAR) %>%
    ungroup()

# Calculate regional-level entropy
acs_race_city <- acs_race_pct_tract %>%
  group_by(YEAR) %>%
    summarize(race_white_city = sum(race_white, na.rm=TRUE),
      race_black_city = sum(race_black, na.rm=TRUE),
      race_asian_city = sum(race_asian, na.rm=TRUE),
      race_latinx_city = sum(race_latinx, na.rm=TRUE),
      race_other_city = sum(race_other, na.rm=TRUE),
      city_pop = sum(tract_pop, na.rm=TRUE)) %>%
  mutate(race_white_pct = race_white_city/city_pop,
         race_black_pct = race_black_city/city_pop,
         race_asian_pct = race_asian_city/city_pop,
         race_latinx_pct = race_latinx_city/city_pop,
         race_other_pct = race_other_city/city_pop) %>%
  rowwise() %>%
  mutate(entropy_race_city = sum(race_white_pct*log(1/race_white_pct) +
        race_black_pct*log(1/race_black_pct) +
        race_asian_pct*log(1/race_asian_pct) +
        race_latinx_pct*log(1/race_latinx_pct) +
        race_other_pct*log(1/race_other_pct), na.rm=TRUE)) %>%
  select(YEAR, city_pop, entropy_race_city) %>%
  ungroup()

#** C. Calculate regional race segregation levels --------------------

acs_race_seg <- left_join(acs_race_pct, acs_race_city, by="YEAR")
acs_race_seg_tract <- left_join(acs_race_pct_tract, acs_race_city, by="YEAR")

acs_race_seg_sum <- acs_race_seg %>%
    mutate(ent_diff = entropy_race_city-entropy_race) %>%
    mutate(nominator_1 = blkgrp_pop*ent_diff) %>%
    group_by(YEAR) %>%
    summarize(nominator = sum(nominator_1, na.rm=TRUE),
              city_pop = mean(city_pop, na.rm=TRUE),
              entropy_race_city = mean(entropy_race_city, na.rm=TRUE)) %>%
    mutate(seg_race_blkgrp = nominator/city_pop*entropy_race_city)

acs_race_seg_sum_tract <- acs_race_seg_tract %>%
    mutate(ent_diff = entropy_race_city-entropy_race) %>%
    mutate(nominator_1 = tract_pop*ent_diff) %>%
    group_by(YEAR) %>%
    summarize(nominator = sum(nominator_1, na.rm=TRUE),
              city_pop = mean(city_pop, na.rm=TRUE),
              entropy_race_city = mean(entropy_race_city, na.rm=TRUE)) %>%
    mutate(seg_race_tract = nominator/city_pop*entropy_race_city)

# Export the dataset
write.csv(acs_race_seg_sum, paste0("saved/sf_segregation_race_blkgrp_", region, ".csv"))
write.csv(acs_race_seg_sum_tract, paste0("saved/sf_segregation_race_tract_", region, ".csv"))

#** D. Merge the block group and census tract dataframes --------

# Subset the block_group ID to create census tract code then merge
acs_race_pct$tract_id <- substr(acs_race_pct$blkgrp_id, 1, 11)

acs_race_seg <- left_join(acs_race_pct, acs_race_pct_tract,
                    by=c("tract_id", "YEAR"),
                   suffix = c("_blkgrp", "_tract"),
                    all.x=TRUE)

# Replace missing block group ratio values with tract values
acs_race_seg <- acs_race_seg %>%
  mutate(race_white = ifelse(is.na(race_white_blkgrp == TRUE), race_white_tract, race_white_blkgrp),
          race_black = ifelse(is.na(race_black_blkgrp == TRUE), race_black_tract, race_black_blkgrp),
            race_asian = ifelse(is.na(race_asian_blkgrp == TRUE), race_asian_tract, race_asian_blkgrp),
              race_latinx = ifelse(is.na(race_latinx_blkgrp == TRUE), race_latinx_tract, race_latinx_blkgrp),
                   race_other = ifelse(is.na(race_other_blkgrp == TRUE), race_other_tract, race_other_blkgrp),
                   race_white_pct = ifelse(is.na(race_white_pct_blkgrp == TRUE), race_white_pct_tract, race_white_pct_blkgrp),
          race_black_pct = ifelse(is.na(race_black_pct_blkgrp == TRUE), race_black_pct_tract, race_black_pct_blkgrp),
            race_asian_pct = ifelse(is.na(race_asian_pct_blkgrp == TRUE), race_asian_pct_tract, race_asian_pct_blkgrp),
              race_latinx_pct = ifelse(is.na(race_latinx_pct_blkgrp == TRUE), race_latinx_pct_tract, race_latinx_pct_blkgrp),
                   race_other_pct = ifelse(is.na(race_other_pct_blkgrp == TRUE), race_other_pct_tract, race_other_pct_blkgrp)) %>%
  rowwise() %>%
  mutate(entropy_race =
    sum(race_white_pct*log(1/race_white_pct) +
        race_black_pct*log(1/race_black_pct) +
        race_asian_pct*log(1/race_asian_pct) +
        race_latinx_pct*log(1/race_latinx_pct) +
        race_other_pct*log(1/race_other_pct), na.rm=TRUE)) %>%
    arrange(YEAR, blkgrp_id) %>%
    ungroup()


# Merge the segregation index calculated at the block group and tract-level
acs_race_seg <- left_join(acs_race_seg, acs_race_seg_sum,
                    by=c("YEAR"),
                    all.x=TRUE)

acs_race_final <- left_join(acs_race_seg, acs_race_seg_sum_tract,
                    by=c("YEAR"),
                    all.x=TRUE)

# Export the dataset
write.csv(acs_race_final, paste0("saved/acs_race_", region, ".csv"))

#* Section 1-3. Reorganize the Segregation Dataset -------------------- 

# Merge the segregation dataframes
segregation_raw <- merge(acs_income_final, acs_race_final, by=c("YEAR", "blkgrp_id"))

glimpse(segregation_raw)

# Remove and rename unrequired/duplicated variables
segregation <- segregation_raw %>%
  select(-c(blkgrp_pop.x, blkgrp_pop.y, tract_id.x, tract_id.y, 
    tract_pop.x, tract_pop.y, nominator.x, nominator.y, city_pop.x, city_pop.y, entropy_race_city.x)) %>%
  rename(entropy_race_city = entropy_race_city.y)

glimpse(segregation)
summary(segregation)

# Export the dataset
write.csv(segregation, paste0("segregation_", region, ".csv"), row.names=FALSE)
}

############################################################
############################################################

#* Section 1-4. Run the function ----------------------------------------

# Designate the variables to use as inputs in the function
state_ca <- "CA"
state_ny <- "NY"

sfb_counties <- c("Alameda", "Marin", "Contra Costa", "San Francisco", "San Mateo")
nyc_counties <- c("New York", "Kings", "Bronx", "Richmond", "Queens")

# Run the functions to produce the datasets
segregation_func(state_ca, sfb_counties, "sfb")
segregation_func(state_ny, nyc_counties, "nyc")

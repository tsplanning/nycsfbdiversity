##################################################################
# Section 2. Create Gentrification Variables
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
gent_func <- function(state, counties, region) {

#* Section 1. Import necessary variables from ACS -----------------------

# List of variables to import #
#** A. Median Family Income
#** B. Median Home Value
#** C. Median Rent
#** D. Median age of housing in 2013 (Growth trajectory)
#** E. Proportion of non-hispanic white population
#** F. Proportion of renter-occupied units
#** F. Proportion of college-educated adults

#** 1-1. Load the block group variables

 data_list <- list()
 i <- 1
 
 for (yr in 2013:2019) {
   
   data <- get_acs(
     geography = "block group",
     variables = c(pop_total = "B01003_001",
     			   med_income = "B19013_001",
     			   med_hmval = "B25077_001",
     			   med_rent = "B25064_001",
     			   total_pop = "B03002_001",
     			   non_hisp_pop = "B03002_003",
     			   total_house = "B25003_001",
     			   renter_house = "B25003_003",
     			   med_year_built = "B25035_001",
     			   edu_total = "B15003_001",
     			   edu_bachelor = "B15003_022",
     			   edu_master = "B15003_023",
     			   edu_professional = "B15003_024",
     			   edu_doctorate = "B15003_025"),
     state = state,
     county = counties,
     year = yr,
     survey = "acs5",
    cache_table = TRUE
   )
   
   data_list[[i]] <- data
   
   i <- i + 1  
 }

all_data <- bind_rows(data_list, .id = "column_label")

# Unlist the data and add variables for calculation
blkgrp_data <- all_data %>% 
   data.frame() %>%
   mutate(YEAR = as.integer(column_label) + 2012) %>%
      select(blkgrp_id = GEOID, YEAR, variable, estimate) %>%
        arrange(blkgrp_id, YEAR)
        
#** 1-2. Load the census tract variables

 data_list <- list()
 i <- 1
 
 for (yr in 2013:2019) {
   
   data <- get_acs(
     geography = "tract",
     variables = c(pop_total = "B01003_001",
     			   med_income = "B19013_001",
     			   med_hmval = "B25077_001",
     			   med_rent = "B25064_001",
     			   total_pop = "B03002_001",
     			   non_hisp_pop = "B03002_003",
     			   total_house = "B25003_001",
     			   renter_house = "B25003_003",
     			   med_year_built = "B25035_001",
     			   edu_total = "B15003_001",
     			   edu_bachelor = "B15003_022",
     			   edu_master = "B15003_023",
     			   edu_professional = "B15003_024",
     			   edu_doctorate = "B15003_025"),
     state = state,
     county = counties,
     year = yr,
     survey = "acs5",
    cache_table = TRUE
   )
   
   data_list[[i]] <- data
   
   i <- i + 1  
 }

all_data <- bind_rows(data_list, .id = "column_label")

# Unlist the data and add variables for calculation
tract_data <- all_data %>% 
   data.frame() %>%
   mutate(YEAR = as.integer(column_label) + 2012) %>%
      select(c(tract_id = GEOID, YEAR, variable, estimate)) %>%
        arrange(tract_id, YEAR)

#** 1-3. Merge the block group and census tract data

data_long <- blkgrp_data %>%
	mutate(tract_id = substr(blkgrp_id, 1, 11)) %>%
	left_join(tract_data,
		by=c("tract_id", "YEAR", "variable"),
		suffix = c("_blkgrp", "_tract")) %>%
	# Replace with tract estimates if block group estimates are missing
	mutate(estimate = ifelse(is.na(estimate_blkgrp)==TRUE, estimate_tract, estimate_blkgrp)) %>%
	select(-c(estimate_blkgrp, estimate_tract))

#** 1-4. Transform from long format to wide format

data_wide <- spread(data_long, key = "variable", value = "estimate")

#** 1-5. Clean the data & create proportion variables
data_cleaned <- data_wide %>%
	rowwise() %>%
	mutate(edu_college = sum(edu_bachelor, edu_master, edu_professional, edu_doctorate, na.rm=TRUE)) %>%
	ungroup() %>%
	mutate(med_income = ifelse(med_income==250001, 250000, med_income),
# 2013 & 2014 are top-coded at 1,000,001 and 2015 ~ are top-coded at 2,000,001
		  med_hmval = ifelse(med_hmval==1000001, 1000000, ifelse(med_hmval==2000001, 2000000, med_hmval)),
# 2013 and 2014 are top coded at 2001 and 2015 ~ are top coded at 3501
		  med_rent = ifelse(med_rent==2001, 2000, ifelse(med_rent==3501, 3500, med_rent)),
# Replace < 1939 (0 & 18) codes as NAs
   		  med_year_built = ifelse(med_year_built < 1939, NA,  med_year_built),
		  non_hisp_pct = ifelse(is.na(total_pop)==TRUE, 0, 100*non_hisp_pop/total_pop),
		  renter_pct = ifelse(is.na(total_house)==TRUE, 0, 100*renter_house/total_house),
		  college_pct = ifelse(is.na(edu_total)==TRUE, 0, 100*edu_college/edu_total),
		  med_age_built = YEAR - med_year_built) %>%
	select(blkgrp_id, YEAR, pop_total, med_hmval, med_income, med_rent, non_hisp_pct, renter_pct, college_pct, med_year_built)


#* Section 2. Import necessary tables from ACS -----------------------

#** 2-1. Load the block group variables

 data_list <- list()
 i <- 1
 
 for (yr in 2013:2019) {
   
   data <- get_acs(
     geography = "block group",
     table = "B25034",
     state = state,
     county = counties,
     year = yr,
     survey = "acs5",
    cache_table = TRUE
   )
   
   data_list[[i]] <- data
   
   i <- i + 1  
 }

all_data <- bind_rows(data_list, .id = "column_label")

# Unlist the data and add variables for calculation
year_built_data <- all_data %>% 
   data.frame() %>%
   mutate(YEAR = as.integer(column_label) + 2012) %>%
      select(c(blkgrp_id = GEOID, YEAR, variable, estimate)) %>%
        arrange(blkgrp_id, YEAR)

#** 2-2. Load the census tract variables

 data_list <- list()
 i <- 1
 
 for (yr in 2013:2019) {
   
   data <- get_acs(
     geography = "block group",
     table = "B25034",
     state = state,
     county = counties,
     year = yr,
     survey = "acs5",
    cache_table = TRUE
   )
   
   data_list[[i]] <- data
   
   i <- i + 1  
 }

all_data <- bind_rows(data_list, .id = "column_label")

# Unlist the data and add variables for calculation
year_built_data_tract <- all_data %>% 
   data.frame() %>%
   mutate(YEAR = as.integer(column_label) + 2012) %>%
      select(c(tract_id = GEOID, YEAR, variable, estimate)) %>%
        arrange(tract_id, YEAR)

#** 2-3. Merge the block group and census tract data

year_built <- year_built_data %>%
	mutate(tract_id = substr(blkgrp_id, 1, 11)) %>%
	left_join(year_built_data_tract,
		by=c("tract_id", "YEAR", "variable"),
		suffix = c("_blkgrp", "_tract")) %>%
	# Replace with tract estimates if block group estimates are missing
	mutate(estimate = ifelse(is.na(estimate_blkgrp)==TRUE, estimate_tract, estimate_blkgrp)) %>%
	select(-c(estimate_blkgrp, estimate_tract))

#** 2-4. Transform from long to wide format

year_built_wide <- spread(year_built, key = "variable", value = "estimate")

year_built_cleaned <- year_built_wide %>%
	rowwise() %>%
	mutate(built_2000 =
		# For 
		ifelse(YEAR < 2015, sum(B25034_002, B25034_003, B25034_004, na.rm=TRUE),
			ifelse(YEAR > 2014, sum(B25034_002, B25034_003, na.rm=TRUE), NA))) %>%
	mutate(new_built_pct = ifelse(is.na(B25034_001)==TRUE, NA, 100*built_2000/B25034_001)) %>%
	ungroup() %>%
	mutate(log_new_built_pct = log(1+new_built_pct)) %>%
	select(blkgrp_id, YEAR, new_built_pct, log_new_built_pct)


#* Section 3. Merge Variables & Table Datasets -----------------------

#** 3-1. Merge the datasets

data <- merge(data_cleaned, year_built_cleaned, by=c("blkgrp_id", "YEAR"))

#** 3-2. Merge with shp files

  if (region=="sfb") {
	shp <- sf::st_read("shp/ca_msa_blkgrp_place.shp") 

  } else if (region=="nyc") {
  	shp <- sf::st_read("shp/ny_msa_blkgrp_place.shp") 
  }

land_area <- shp %>%
  mutate(blkgrp_id = substr(GEO_ID, 10, 21)) %>%
  # Keep unique blkgrp_ids only to remove the duplicated block group rows
  distinct_at(vars(blkgrp_id), .keep_all = TRUE) %>%
  select(blkgrp_id, land_area = CENSUSAREA, placename_1 = NAME_2, placename_2 = NAMELSAD) %>%
  data.frame()

data_merged <- data %>% left_join(land_area, by = c("blkgrp_id"), by.x=TRUE) %>%
	mutate(density = ifelse(land_area==0, NA, pop_total/land_area)) %>%
	mutate(log_density = log(density+1)) %>%
	select(-c(land_area))

#** 3-3. Create a principal city variable

# Check Unique places names

  if (region=="sfb") {

  sf_pr_cities <- c("San Francisco city", "Oakland city", "Hayward city", "Berkeley city", "San Leandro city", "Redwood city",
					"San Ramon city", "Pleasanton city", "Walnut Creek city", "South San Francisco city", "San Rafael city")

  data_merged <- data_merged %>%
    mutate(pr_city=ifelse(placename_2 %in% sf_pr_cities, 1, 0))

  } else if (region=="nyc") {

  data_merged <- data_merged %>%
  	mutate(pr_city=1) 
  }

#** 3-3. Create 2013 & 2019 variables

data_2013 <- data_merged %>%
	filter(YEAR==2013) %>%
	select(-c(placename_1, placename_2, pr_city, geometry))

data_2019 <- data_merged %>%
	filter(YEAR==2019) %>%
	select(-c(placename_1, placename_2, pr_city, geometry))

# Merge the original data with 2013 & 2019 dataframes
data_joined <- data_merged %>%
	left_join(data_2013, by=c("blkgrp_id"), suffix = c("", "_13")) %>%
	left_join(data_2019, by=c("blkgrp_id"), suffix = c("", "_19"))

#** 3-4. Create 2013 - 2019 percentage increase variables
data_inc <- data_joined %>%
filter(YEAR==2019) %>%
rowwise() %>%
	mutate(pop_total_inc = ifelse(pop_total_13 == 0, NA, 100*(pop_total_19 - pop_total_13)/pop_total_13),
		   med_income_inc = ifelse(med_income_13 == 0, NA, 100*(med_income_19 - med_income_13)/med_income_13),
		   med_hmval_inc = ifelse(med_hmval_13 == 0, NA, 100*(med_hmval_19 - med_hmval_13)/med_hmval_13),
		   med_rent_inc = ifelse(med_rent_13 == 0, NA, 100*(med_rent_19 - med_rent_13)/med_rent_13),
		   non_hisp_pct_inc = ifelse(non_hisp_pct_13 == 0, NA, 100*(non_hisp_pct_19 - non_hisp_pct_13)/non_hisp_pct_13),
		   renter_pct_inc = ifelse(renter_pct_13 == 0, NA, 100*(renter_pct_19-renter_pct_13)/renter_pct_13),
		   college_pct_inc = ifelse(college_pct_13 == 0, NA, 100*(college_pct_19-college_pct_13)/college_pct_13),
		   # New_built has many 0 values and thus produces many NAs
		   new_built_pct_inc = ifelse(new_built_pct_13 == 0, NA, 100*(new_built_pct_19-new_built_pct_13)/new_built_pct_13),
		   log_new_built_pct_inc = ifelse(log_new_built_pct_13 == 0, NA, 100*(log_new_built_pct_19-log_new_built_pct_13)/log_new_built_pct_13),
		   density_inc = ifelse(density_13 == 0, NA, 100*(density_19-density_13)/density_13),
		   log_density_inc = ifelse(log_density_13 == 0, NA, 100*(log_density_19-log_density_13)/log_density_13)) %>%
	ungroup() %>%
	select("blkgrp_id", contains("inc")) %>%
	select(-c(med_income, med_income_13, med_income_19)) %>%
	ungroup()

# Join the increase variable with the original dataframe
data_joined <- data_joined %>%
	left_join(data_inc, by="blkgrp_id") %>%
	mutate(blkgrp_id = as.character(blkgrp_id))

#** 3-5. Filter to population > 100 for all years

data_100 <- data_joined %>% filter(pop_total >= 100 & YEAR==2019) %>% select(-c(YEAR)) 

#* Section 4. Create Gentrification Variables -----------------------

#** 4-1. gent_freeman -----------------------------------

gent_freeman <- data_100 %>%
#*** Con 1: Median Income, Mediaxn Home Value, and Median Rent
  mutate(freeman_c1 = ifelse(med_income_13 < quantile(med_income_13, probs=c(0.4), na.rm=TRUE), 1, 0),
        freeman_c2 = ifelse(new_built_pct_13 < quantile(new_built_pct_13, probs=c(0.4), na.rm=TRUE), 1, 0),
#*** Con 2. Home value increased from 2013 to 2019
  		freeman_c3 = ifelse(med_hmval_inc > 0.560882548, 1, 0),
#*** Con 4. Gentrifier population increased from 2013 to 2019
		freeman_c4 = ifelse(college_pct_inc > 0.166591527, 1, 0)) %>%

#*** Select only the conditions
  select(blkgrp_id, freeman_c1, freeman_c2, freeman_c3, freeman_c4, pr_city) %>%

#*** Create freeman gentrification dummies
  mutate(gent_freeman = ifelse(pr_city == 1 &
                          freeman_c1 == 1 &
                           freeman_c2 == 1 &
                           freeman_c3 == 1 &
                           freeman_c4 == 1
                           , 1, 0)) %>% 
  mutate(non_gent_freeman = ifelse(pr_city == 1 &
                          freeman_c1 == 1 &
                          freeman_c2 == 1 &
                          gent_freeman != 1, 1, 0)) %>%
  select(-c(pr_city)) %>%
# For merging later
  mutate(blkgrp_id = as.character(blkgrp_id))
 # Need to created a combined categorical variable

#** 4-2. gent_chapple -----------------------------------

# Import county-level ACS data

 data_list <- list()
 i <- 1
 
 for (yr in 2013:2019) {
   
   data <- get_acs(
     	 geography = "county",
          variables = c(pop_total = "B01003_001",
     			   med_income = "B19013_001",
     			   med_hmval = "B25077_001",
     			   med_rent = "B25064_001",
     			   total_pop = "B03002_001",
     			   non_hisp_pop = "B03002_003",
     			   total_house = "B25003_001",
     			   renter_house = "B25003_003",
     			   med_year_built = "B25035_001",
     			   edu_total = "B15003_001",
     			   edu_bachelor = "B15003_022",
     			   edu_master = "B15003_023",
     			   edu_professional = "B15003_024",
     			   edu_doctorate = "B15003_025"),
     state = state,
     county = counties,
     year = yr,
     survey = "acs5",
    cache_table = TRUE
   )
   
   data_list[[i]] <- data
   
   i <- i + 1  
 }

all_data <- bind_rows(data_list, .id = "column_label")

# Unlist the data and add variables for calculation
county_data <- all_data %>%
   data.frame() %>%
   mutate(YEAR = as.integer(column_label) + 2012) %>%
      select(county_id = GEOID, YEAR, variable, estimate) %>%
        arrange(county_id, YEAR)
# Transform from long format to wide format

county_wide <- spread(county_data, key = "variable", value = "estimate")

# Clean the data & create ratio variables
county_cleaned <- county_wide %>%
	rowwise() %>%
	mutate(edu_college = sum(edu_bachelor, edu_master, edu_professional, edu_doctorate, na.rm=TRUE)) %>%
	ungroup() %>%
	mutate(med_income = ifelse(med_income==250001, 250000, med_income),
# 2013 & 2014 are top-coded at 1,000,001 and 2015 ~ are top-coded at 2,000,001
		  med_hmval = ifelse(med_hmval==1000001, 1000000, ifelse(med_hmval==2000001, 2000000, med_hmval)),
# 2013 and 2014 are top coded at 2001 and 2015 ~ are top coded at 3501
		  med_rent = ifelse(med_rent==2001, 2000, ifelse(med_rent==3501, 3500, med_rent)),
# Replace < 1939 (0 & 18) codes as NAs
   		  med_year_built = ifelse(med_year_built < 1939, NA,  med_year_built),
		  non_hisp_pct = ifelse(is.na(total_pop)==TRUE, 0, 100*non_hisp_pop/total_pop),
		  renter_pct = ifelse(is.na(total_house)==TRUE, 0, 100*renter_house/total_house),
		  college_pct = ifelse(is.na(edu_total)==TRUE, 0, 100*edu_college/edu_total),
		  med_age_built = YEAR - med_year_built) %>%
	select(county_id, YEAR, pop_total, med_hmval, med_income, med_rent, non_hisp_pct, renter_pct, college_pct, med_year_built)

#** Create 2013 & 2019 variables
county_2013 <- county_cleaned %>%
	filter(YEAR==2013) %>% select(-c(YEAR))

county_2019 <- county_cleaned %>%
	filter(YEAR==2019) %>% select(-c(YEAR))

# Merge the original data with 2013 & 2019 dataframes
county_joined <- county_cleaned %>%
	left_join(county_2013, by=c("county_id"), all.x=TRUE, suffix = c("", "_13")) %>%
	left_join(county_2019, by=c("county_id"), all.x=TRUE, suffix = c("", "_19"))

#** 3-4. Create 2013 - 2019 percentage increase variables
county_inc <- county_joined %>%
	filter(YEAR==2013) %>%
	select(-c(YEAR)) %>%
	rowwise() %>%
	mutate(pop_total_inc = ifelse(pop_total_13 == 0, NA, 100*(pop_total_19 - pop_total_13)/pop_total_13),
		   med_income_inc = ifelse(med_income_13 == 0, NA, 100*(med_income_19 - med_hmval_13)/med_hmval_13),
		   med_hmval_inc = ifelse(med_hmval_13 == 0, NA, 100*(med_hmval_19 - med_hmval_13)/med_hmval_13),
		   med_rent_inc = ifelse(med_rent_13 == 0, NA, 100*(med_rent_19 - med_rent_13)/med_rent_13),
		   non_hisp_pct_inc = ifelse(non_hisp_pct_13 == 0, NA, 100*(non_hisp_pct_19 - non_hisp_pct_13)/non_hisp_pct_13),
		   renter_pct_inc = ifelse(renter_pct_13 == 0, NA, 100*(renter_pct_19-renter_pct_13)/renter_pct_13),
		   college_pct_inc = ifelse(college_pct_13 == 0, NA, 100*(college_pct_19-college_pct_13)/college_pct_13)) %>%
		   # New_built has many 0 values and thus produces many NAs
		   # new_built_pct_inc = ifelse(new_built_pct_13 == 0, NA, 100*(new_built_pct_19-new_built_pct_13)/new_built_pct_13),
		   # log_new_built_pct_inc = ifelse(log_new_built_pct_13 == 0, NA, 100*(log_new_built_pct_19-log_new_built_pct_13)/log_new_built_pct_13),
	ungroup()

# Import county-level income data from ACS
 data_list <- list()
 i <- 1
 
 for (yr in 2013:2019) {
   inc <- get_acs(
     geography = "county",
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

# Unlist the data and add variables for calculation
county_income <- all_data %>% 
   data.frame() %>%
   mutate(YEAR = as.integer(column_label) + 2012) %>%
   rename(county_id = GEOID) %>% 
   select(c(YEAR, county_id, variable, estimate))

# Convert into a long format
income_wide <- spread(county_income, key = "variable", value = "estimate")

# Calculate Proportions
county_income <- income_wide %>%
	rowwise() %>%
	mutate(inc_very_low_pct = 100*sum(B19001_002, B19001_003, B19001_004, B19001_005, B19001_006, na.rm=TRUE)/B19001_001,
	inc_low_pct = 100*sum(B19001_002, B19001_003, B19001_004, B19001_005, B19001_006, B19001_007, B19001_008, B19001_009, B19001_010, B19001_011, na.rm=TRUE)/B19001_001) %>%
	select(c(county_id, YEAR, inc_very_low_pct, inc_low_pct)) %>%
	ungroup()

county_income_2013 <- county_income %>%
	filter(YEAR==2013) %>%
	select(-c(YEAR))

county_income_2019 <- county_income %>%
	filter(YEAR==2019) %>%
	select(-c(YEAR))

county_income <- county_income %>%
	left_join(county_income_2013, by="county_id", suffix=c("", "_13")) %>%
	left_join(county_income_2019, by="county_id", suffix=c("", "_19")) %>%
	mutate(inc_very_low_pct_inc = ifelse(inc_very_low_pct_13==0, NA, 100*(inc_very_low_pct_19-inc_very_low_pct_13)/inc_very_low_pct_13),
		inc_low_pct_inc = ifelse(inc_low_pct_13==0, NA, 100*(inc_very_low_pct_19-inc_low_pct_13)/inc_low_pct_13)) %>%
	filter(YEAR==2019) %>%
	select(county_id, inc_very_low_pct_13, inc_low_pct_13, inc_very_low_pct_19, inc_low_pct_19, inc_very_low_pct_inc, inc_low_pct_inc)

 if (region=="sfb") {
 	seg <- read.csv("segregation_sfb.csv")

	seg <- seg %>%
		mutate(blkgrp_id = paste0("0", seg$blkgrp_id)) 

} else if (region=="nyc") {
 	seg <- read.csv("segregation_nyc.csv") 
 }

	seg <- seg%>%
		rowwise() %>%
		mutate(inc_very_low_pct = 100*inc_very_low/sum(inc_very_low, inc_low, inc_middle, inc_high, na.rm=TRUE),
		inc_low_pct = 100*sum(inc_very_low, inc_low, na.rm=TRUE)/sum(inc_very_low, inc_low, inc_middle, inc_high, na.rm=TRUE)) %>%
		select(blkgrp_id, YEAR, inc_very_low_pct, inc_low_pct) %>%
		ungroup()

	seg_13 <- seg %>%
		filter(YEAR==2013) %>%
		select(-c(YEAR))

	seg_19 <- seg %>%
		filter(YEAR==2019) %>%
		select(-c(YEAR))

	seg <- seg %>%
		left_join(seg_13, by="blkgrp_id", suffix=c("", "_13")) %>% 
		left_join(seg_19, by="blkgrp_id", suffix=c("", "_19")) %>%
		mutate(inc_very_low_pct_inc = ifelse(inc_very_low_pct_13==0, NA, 100*(inc_very_low_pct_19-inc_very_low_pct_13)/inc_very_low_pct_13),
		inc_low_pct_inc = ifelse(inc_low_pct_13==0, NA, 100*(inc_very_low_pct_19-inc_low_pct_13)/inc_low_pct_13)) %>%
	filter(YEAR==2019) %>%
	select(blkgrp_id, inc_very_low_pct_13, inc_low_pct_13, inc_very_low_pct_19, inc_low_pct_19, inc_very_low_pct_inc, inc_low_pct_inc) %>%
	# For merging later
	mutate(blkgrp_id = as.character(blkgrp_id))
 
# Merge chapple dataset with the original dataset
gent_chapple <- data_100 %>%
  mutate(county_id = substr(blkgrp_id, 1, 5)) %>%
  left_join(county_inc, by=c("county_id"), suffix =c("", "_county")) %>%
# Join with the segregation dataset
  left_join(seg, by=c("blkgrp_id"), by.x=TRUE) %>%
  left_join(county_income, by="county_id", suffix = c("", "_county"), by.x=TRUE) %>%
# Create Vulnerability Dummy
# Need to join the county increase variable as well as the 2013 baseline variable!!
  mutate(v1 = ifelse(med_rent_13 < med_rent_13_county | med_hmval_13 < med_hmval_13_county, 1, 0),
           v2_1 = ifelse(college_pct_13 < college_pct_13_county, 1, 0),
           v2_2 = ifelse(inc_low_pct_13 > inc_low_pct_13_county, 1, 0),
           v2_3 = ifelse(renter_pct_13 > renter_pct_13_county, 1, 0),
           v2_4 = ifelse(non_hisp_pct_13 < non_hisp_pct_13_county, 1, 0)) %>%
  rowwise() %>%
  mutate(v2_count = sum(v2_1, v2_2, v2_3, v2_4, na.rm=TRUE)) %>%
  mutate(vulnerable_chapple = ifelse(v1==1 & v2_count >= 2 & pr_city == 1, 1, 0)) %>%
  mutate(price_inc = ifelse(med_hmval_inc > med_hmval_inc_county |
                               med_rent_inc > med_rent_inc_county, 1, 0)) %>%
  mutate(gent_chapple = ifelse(vulnerable_chapple == 1 &
                               med_income_inc > med_income_inc_county &
                               price_inc == 1 &
                               college_pct_inc > college_pct_inc_county &
                               pr_city == 1, 1, 0),
         non_gent_chapple = ifelse(vulnerable_chapple == 1 &
                                   gent_chapple != 1 &
                                   pr_city == 1, 1, 0)) %>%
  ungroup() %>%
  select(blkgrp_id, v1, v2_1, v2_2, v2_3, v2_4, v2_count, price_inc, gent_chapple, non_gent_chapple, vulnerable_chapple) %>%
  # For merging later
  mutate(blkgrp_id = as.character(blkgrp_id))
  # Need to created a combined categorical variable

# Reduce the number of variables
data_joined <- data_joined %>%
	select(-contains('_13')) %>%
		select(-contains('_19')) %>%
			arrange(blkgrp_id, YEAR)

#* Section 5. Export the dataset -----------------------

# Merge the control & gentrification datasets
# Export the final dataset - something is happening here! A very big dataset.
gent_final <- merge(gent_freeman, gent_chapple, by="blkgrp_id") 

# Export the final dataset
write.csv(data_joined, paste0("controls_", region, ".csv"))

# Export the final dataset - something is happening here! A very big dataset.
write.csv(gent_final, paste0("gentrification_", region, ".csv"))

print("export complete")

} 

#* Section 6. Run the function -----------------------

# Define the variables
state_ca <- "CA"
state_ny <- "NY"

sfb_counties <- c("Alameda", "Marin", "Contra Costa", "San Francisco", "San Mateo")
nyc_counties <- c("New York", "Kings", "Bronx", "Richmond", "Queens")

# Run the functions to produce the datasets
gent_func(state_ca, sfb_counties, "sfb")
gent_func(state_ny, nyc_counties, "nyc")
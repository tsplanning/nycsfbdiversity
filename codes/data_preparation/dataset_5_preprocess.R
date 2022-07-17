##################################################################
# Section 5. Create geocodes
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

# ssh -o ServerAliveInterval=600 udp@klein.berkeley.edu

# Set working directory
setwd("~/data/projects/segregation/datasets")

# Section 1. Create the getmode function ----------------------------

# Define getmode function
getmode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  # Return the largest number when there are multiple modes
  max(ux[tab == max(tab)])
}

# Section 2. Create a function for renaming and recoding the variables -----------

income_func <- function(region) {

if (region=="sfb") {

  hh <- read.csv("hh_geocoded_sfb.csv")

} else if (region=="nyc") {

  hh <- read.csv("hh_geocoded_nyc.csv")

}

hh <- hh %>%
  select(c(YEAR, FAMILYID, GEOID, GE_CENSUS_STATE_2010, FIND_DIV_1000,
              OWNER_RENTER_STATUS, LOCATION_TYPE, HEAD_HH_AGE_CODE,
              MARITAL_STATUS, CHILDREN_IND, LENGTH = LENGTH_OF_RESIDENCE))

#* 2-1. Recode other variables ------------------------
hh <- hh %>% 
  mutate(
    AGE = case_when(
      HEAD_HH_AGE_CODE=='A' ~ '<25',
      HEAD_HH_AGE_CODE=='B' ~ '25-29',
      HEAD_HH_AGE_CODE=='C' ~ '30-34',
      HEAD_HH_AGE_CODE=='D' ~ '35-39',
      HEAD_HH_AGE_CODE=='E' ~ '40-44',
      HEAD_HH_AGE_CODE=='F' ~ '45-49',
      HEAD_HH_AGE_CODE=='G' ~ '50-54',
      HEAD_HH_AGE_CODE=='H' ~ '55-59',
      HEAD_HH_AGE_CODE=='I' ~ '60-64',
      HEAD_HH_AGE_CODE=='J' ~ '65+',
      HEAD_HH_AGE_CODE=='K' ~ '65-69',
      HEAD_HH_AGE_CODE=='L' ~ '70-74',
      HEAD_HH_AGE_CODE=='M' ~ '75+'),
    
    TENURE = case_when(
       OWNER_RENTER_STATUS <= 6 ~ "rent",
       OWNER_RENTER_STATUS > 6 ~ "own"),
    
    SFDU = case_when(
       LOCATION_TYPE == "S" ~ "SFDU",
       LOCATION_TYPE != "S" ~ "others"),
 
    MARRIED = case_when(
      MARITAL_STATUS <= 4 ~ "single",
      MARITAL_STATUS > 4 ~ "married"),

    CHILDREN = case_when(
      CHILDREN_IND == 0 ~ "not present",
      CHILDREN_IND == 1 ~ "present"),

    # LENGTH
    LN_LENGTH = log(LENGTH+1)

  )

hh <- hh %>%
    select(YEAR, FAMILYID, GEOID, FIND_DIV_1000, AGE, TENURE, SFDU, MARRIED,
         CHILDREN, LENGTH, LN_LENGTH)

#* 2-2. Smooth income ------------------------

# Subset to only household heads age >=25
 hh <- hh %>%
  filter(AGE != '<25') %>%
  mutate(GEOID=ifelse(nchar(as.character(GEOID))==11, paste0(0, as.character(GEOID)), GEOID)) %>%
  mutate(county_id = substr(GEOID, 1, 5))

if (region=="sfb") {

wts <- read.csv("weights_sfb.csv")
counties <- c("06001", "06013", "06041", "06075", "06081")

} else if (region=="nyc") {

wts <- read.csv("weights_nyc.csv")
counties <- c("36005", "36047", "36061", "36081", "36085")

}

wts <- wts %>%
  mutate(county_id = ifelse(nchar(as.character(county_id))==4, paste0(0, as.character(county_id)), county_id)) %>%
  mutate(county_id = as.character(county_id)) %>%
  select(YEAR, county_id, AMI80, AMI120, AMI165) %>%
  arrange(YEAR, county_id)

hh_wts <- merge(hh, wts,
            by = c('YEAR', 'county_id'),
            all.x=TRUE)

# Create income categories by comparing to AMI cutoffs
hh_wts <- hh_wts %>%
  mutate(inc_rank = case_when(
    # FIND_DIV_1000 <= AMI50 ~ 1,
    # FIND_DIV_1000 <= AMI100 ~ 2,
    # FIND_DIV_1000 <= AMI150 ~ 3,
    # FIND_DIV_1000 > AMI150 ~ 4,
    FIND_DIV_1000 <= AMI80/1000 ~ 1,
    FIND_DIV_1000 <= AMI120/1000 ~ 2,
    FIND_DIV_1000 <= AMI165/1000 ~ 3,
    FIND_DIV_1000 > AMI165/1000 ~ 4,
  )) %>%
  select(-c(AMI80, AMI120, AMI165))

glimpse(hh_wts)

write.csv(hh_wts, paste0("hh_full_", region, ".csv"))

# "Squeeze" the households
hh_final <- hh_wts %>%
  group_by(FAMILYID) %>%
  summarize(
    YEAR = YEAR[which(county_id %in% counties, arr.ind = TRUE)[1]],
    # The first place found in one of the counteis
    GEOID_first = GEOID[which(county_id %in% counties, arr.ind = TRUE)[1]],
    GEOID_19 = GEOID[YEAR==2019],
    # For income, get mode from years when the household was in the selected counties
    INCOME = getmode(inc_rank[which(county_id %in% counties, arr.ind = TRUE)]),
            # Select the first element observed when the household is in the county
            AGE = AGE[which(county_id %in% counties, arr.ind = TRUE)[1]],
            # The first when they are observed
            TENURE = TENURE[which(county_id %in% counties, arr.ind = TRUE)[1]],
            # The first when they are observed
            SFDU = SFDU[which(county_id %in% counties, arr.ind = TRUE)[1]],
            # The first when they are observed
            MARRIED = MARRIED[which(county_id %in% counties, arr.ind = TRUE)[1]],
            # The first when they are observed
            CHILDREN = CHILDREN[which(county_id %in% counties, arr.ind = TRUE)[1]],
            # The first when they are observed
            LENGTH = LENGTH[which(county_id %in% counties, arr.ind = TRUE)[1]],
            # The first when they are observed
            LN_LENGTH = LN_LENGTH[which(county_id %in% counties, arr.ind = TRUE)[1]]) %>%
    mutate(INCOME = case_when(
    # inc_rank_new==1 ~ 'very_low',
    # inc_rank_new==2 ~ 'low',
    # inc_rank_new==3 ~ 'middle',
    # inc_rank_new==4 ~ 'high'
    INCOME == 1 ~ 'low', # Why is everyone classified as "low" ?
    INCOME == 2 ~ 'moderate',
    INCOME == 3 ~ 'middle',
    INCOME == 4 ~ 'high'),
    MOVE_OUT = ifelse(GEOID_first != GEOID_19, 1, 0)) %>%
    data.frame() %>%
    ungroup()

glimpse(hh_final)

write.csv(hh_final, paste0("hh_compressed_", region, ".csv"))

}

income_func("sfb") # 15,361,613 -> 1,4,811,927 -> 4,014,096 (unique: 4,177,493)
income_func("nyc") # #28,984,295 -> 7,751,303 (unique: 8,216,747)

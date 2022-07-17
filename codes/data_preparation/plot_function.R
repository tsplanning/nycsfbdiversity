# Open Libraries
library(dplyr)
library(nnet)
library(ggplot2)
library(effects)
library(broom)
library(stringr)
library(tidyr)
library(ggpubr)

# Load Datasets
setwd("~/data/projects/segregation/datasets")

# Section 1. Preprocess the Data ------------------------------

df_func <- function(region, nb_type) {

df_gent <- read.csv(paste0("gentrification_nyc.csv"))
df_seg <- read.csv(paste0("segregation_nyc.csv"))
df_controls <- read.csv2(paste0("controls_nyc.csv"))

# Read the Data
df_gent <- read.csv(paste0("gentrification_", region, ".csv"))
df_seg <- read.csv(paste0("segregation_", region, ".csv"))
df_controls <- read.csv2(paste0("controls_", region, ".csv"))

# Join the datasets together
df <- full_join(df_seg, df_gent, by=c("blkgrp_id"))
df <- full_join(df, df_controls, by=c("blkgrp_id", "YEAR"))

df <- df %>%
  mutate(blkgrp_id = as.character(ifelse(nchar(blkgrp_id)==11, paste0("0", as.character(blkgrp_id)),
        blkgrp_id)),
         blkgrp_pop = race_asian_blkgrp + race_black_blkgrp + race_latinx_blkgrp + race_other_blkgrp + race_white_blkgrp,
         YEAR = as.numeric(YEAR)) %>%
  select(-c(X.x, X.y, pr_city.x, pr_city.y)) %>%
  filter(blkgrp_pop >= 100)

if (nb_type=="nb_freeman") {

df_final <- df %>%
  group_by(nb_freeman, YEAR) %>%
    summarize(entropy_income = mean(entropy_income, na.rm=TRUE),
              entropy_race = mean(entropy_race, na.rm=TRUE),
              entropy_income_tract = mean(entropy_income_tract, na.rm=TRUE),
              entropy_race_tract = mean(entropy_race_tract, na.rm=TRUE),
              inc_low_pct = mean(inc_low_pct, na.rm=TRUE),
              inc_moderate_pct = mean(inc_moderate_pct, na.rm=TRUE),
              inc_middle_pct = mean(inc_middle_pct, na.rm=TRUE),
              inc_high_pct = mean(inc_high_pct, na.rm=TRUE)) %>%
     # Filter NA values for the variable to plot
    filter(is.na(nb_freeman)!=TRUE & nb_freeman!="Non-city") %>%
    ungroup()

    } else if (nb_type=="nb_chapple") {

df_final <- df %>%
  group_by(nb_chapple, YEAR) %>%
    summarize(entropy_income = mean(entropy_income, na.rm=TRUE),
              entropy_race = mean(entropy_race, na.rm=TRUE),
              entropy_income_tract = mean(entropy_income_tract, na.rm=TRUE),
              entropy_race_tract = mean(entropy_race_tract, na.rm=TRUE),
              inc_low_pct = mean(inc_low_pct, na.rm=TRUE),
              inc_moderate_pct = mean(inc_moderate_pct, na.rm=TRUE),
              inc_middle_pct = mean(inc_middle_pct, na.rm=TRUE),
              inc_high_pct = mean(inc_high_pct, na.rm=TRUE)) %>%
     # Filter NA values for the variable to plot
    filter(is.na(nb_chapple)!=TRUE & nb_chapple!="Non-city") %>%
    ungroup()

   }

  return(df_final)

}

# Run the functions
nyc_freeman <- df_func("nyc", "nb_freeman") # Why does it not have entropy_race value
nyc_chapple <- df_func("nyc", "nb_chapple")

sfb_freeman <- df_func("sfb", "nb_freeman")
sfb_chapple <- df_func("sfb", "nb_chapple")


# Section 2. Plot the variables ------------------------------

setwd("~/data/projects/segregation/datasets/figures")

plot_func <- function(df, nb_type, yvar, ylab) {

  df_name <- deparse(substitute(df))
  yvar_name <- deparse(substitute(yvar))

  plot <- ggplot(df, aes(x=YEAR, y = yvar, color = nb_type)) + geom_line() +
    scale_x_continuous(name="Year", limits=c(2013, 2019), breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019)) + ylab(ylab)

  ggsave(paste0(yvar_name, ".jpg"))

  return(plot)
}

# NYC - Freeman
nyc_f1 <- plot_func(nyc_freeman, nyc_freeman$nb_freeman, nyc_freeman$entropy_income,  "Income Diversity")
nyc_f2 <- plot_func(nyc_freeman, nyc_freeman$nb_freeman, nyc_freeman$entropy_race,  "Racial Diversity")
# plot_func(nyc_freeman, nyc_freeman$nb_freeman, nyc_freeman$entropy_income_tract,  "Income Diversity")
# plot_func(nyc_freeman, nyc_freeman$nb_freeman, nyc_freeman$entropy_race_tract,  "Racial Diversity")
nyc_f1A <- plot_func(nyc_freeman, nyc_freeman$nb_freeman, nyc_freeman$inc_low_pct,  "Low-income Households (%)")
nyc_f1B <- plot_func(nyc_freeman, nyc_freeman$nb_freeman, nyc_freeman$inc_moderate_pct,  "Moderate-income Households (%)")
nyc_f1C <- plot_func(nyc_freeman, nyc_freeman$nb_freeman, nyc_freeman$inc_middle_pct,  "Middle-income Households (%)")
nyc_f1D <- plot_func(nyc_freeman, nyc_freeman$nb_freeman, nyc_freeman$inc_high_pct,  "High-income Households (%)")

# SFB - Freeman
sfb_f1 <- plot_func(sfb_freeman, sfb_freeman$nb_freeman, sfb_freeman$entropy_income,  "Income Diversity")
sfb_f2 <- plot_func(sfb_freeman, sfb_freeman$nb_freeman, sfb_freeman$entropy_race,  "Racial Diversity")

sfb_f1A <- plot_func(sfb_freeman, sfb_freeman$nb_freeman, sfb_freeman$inc_low_pct,  "Low-income Households (%)")
sfb_f1B <- plot_func(sfb_freeman, sfb_freeman$nb_freeman, sfb_freeman$inc_moderate_pct,  "Moderate-income Households (%)")
sfb_f1C <- plot_func(sfb_freeman, sfb_freeman$nb_freeman, sfb_freeman$inc_middle_pct,  "Middle-income Households (%)")
sfb_f1D <- plot_func(sfb_freeman, sfb_freeman$nb_freeman, sfb_freeman$inc_high_pct,  "High-income Households (%)")

# NYC - Chapple
nyc_c1 <- plot_func(nyc_chapple, nyc_chapple$nb_chapple, nyc_chapple$entropy_income,  "Income Diversity")
nyc_c2 <- plot_func(nyc_chapple, nyc_chapple$nb_chapple, nyc_chapple$entropy_race,  "Racial Diversity")
# plot_func(nyc_chapple, nyc_chapple$nb_chapple, nyc_chapple$entropy_income_tract,  "Income Diversity")
# plot_func(nyc_chapple, nyc_chapple$nb_chapple, nyc_chapple$entropy_race_tract,  "Racial Diversity")

nyc_c1A <- plot_func(nyc_chapple, nyc_chapple$nb_chapple, nyc_chapple$inc_low_pct,  "Low-income Households (%)")
nyc_c1B <- plot_func(nyc_chapple, nyc_chapple$nb_chapple, nyc_chapple$inc_moderate_pct,  "Moderate-income Households (%)")
nyc_c1C <- plot_func(nyc_chapple, nyc_chapple$nb_chapple, nyc_chapple$inc_middle_pct,  "Middle-income Households (%)")
nyc_c1D <- plot_func(nyc_chapple, nyc_chapple$nb_chapple, nyc_chapple$inc_high_pct,  "High-income Households (%)")


# SFB - Chapple
sfb_c1 <- plot_func(sfb_chapple, sfb_chapple$nb_chapple, sfb_chapple$entropy_income,  "Income Diversity")
sfb_c2 <- plot_func(sfb_chapple, sfb_chapple$nb_chapple, sfb_chapple$entropy_race,  "Racial Diversity")
# plot_func(sfb_chapple, sfb_chapple$nb_chapple, sfb_chapple$entropy_income_tract,  "Income Diversity")
# plot_func(sfb_chapple, sfb_chapple$nb_chapple, sfb_chapple$entropy_race_tract,  "Racial Diversity")

sfb_c1A <- plot_func(sfb_chapple, sfb_chapple$nb_chapple, sfb_chapple$inc_low_pct,  "Low-income Households (%)")
sfb_c1B <- plot_func(sfb_chapple, sfb_chapple$nb_chapple, sfb_chapple$inc_moderate_pct,  "Moderate-income Households (%)")
sfb_c1C <- plot_func(sfb_chapple, sfb_chapple$nb_chapple, sfb_chapple$inc_middle_pct,  "Middle-income Households (%)")
sfb_c1D <- plot_func(sfb_chapple, sfb_chapple$nb_chapple, sfb_chapple$inc_high_pct,  "High-income Households (%)")


# Section 3. Combine the plots ------------------------------

merge_func <- function(f1, f2, f3, f4, name) {

ggarrange(f1, f2, f3, f4, labels = c("NYC/Chapple et al. (2021)", "NYC/Freeman (2005)", "SFB/Chapple et al. (2021)", "SFB/Freeman (2005)"),
          common.legend = TRUE, legend = "bottom",
          font.label = list(size = 9, face = "plain", color ="black"))

  ggsave(paste0(name, ".jpg"), bg="white")

}

merge_func(nyc_c1, nyc_f1, sfb_c1, sfb_f1, "entropy_income")
merge_func(nyc_c2, nyc_f2, sfb_c2, sfb_f2, "entropy_race")
merge_func(nyc_c1A, nyc_f1A, sfb_c1A, sfb_f1A, "inc_low")
merge_func(nyc_c1B, nyc_f1B, sfb_c1B, sfb_f1B, "inc_moderate")
merge_func(nyc_c1C, nyc_f1C, sfb_c1C, sfb_f1C, "inc_middle")
merge_func(nyc_c1D, nyc_f1D, sfb_c1D, sfb_f1D, "inc_high")

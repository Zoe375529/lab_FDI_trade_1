#################################
#### Lab Exports Top 10 Analysis
#### Huan Hong
#### Date: 20 May 2026
#################################

# Clear environment
rm(list = ls())

# Load required libraries
library(censusapi)
library(tidyverse)

## Part 1: Pull exports data from Census API ----

# Endpoint: timeseries/intltrade/exports/naics
# Variable: ALL_VAL_YR (total annual export value, cumulative by month)
# MONTH = "12" gives us the year-end value (i.e., annual total)

exports_cty_yr <- getCensus(
  name = "timeseries/intltrade/exports/naics",
  vars = c("ALL_VAL_YR", "YEAR", "CTY_CODE", "CTY_NAME"),
  time = "from 2000",
  MONTH = "12",
  show_call = TRUE
)

# Check what we got
head(exports_cty_yr)

## Part 2: Clean the data ----

# Filter out region codes and other aggregation codes
# We only want individual country data, not regional totals
exports_cty_yr_clean <- exports_cty_yr %>%
  filter(!(substr(CTY_CODE, 1, 1) == "0" |
             substr(CTY_CODE, 2, 2) == "X" |
             substr(CTY_CODE, 1, 1) == "-"))

# Convert ALL_VAL_YR and YEAR to numeric, rescale to billions of dollars
exports_cty_yr_clean <- exports_cty_yr_clean %>%
  mutate(ALL_VAL_YR = as.numeric(ALL_VAL_YR) / 1000000000,
         YEAR = as.numeric(YEAR))

# Check for NAs from conversion (should be 0 or very small)
sum(is.na(exports_cty_yr_clean$ALL_VAL_YR))

# Preview cleaned data
head(exports_cty_yr_clean)
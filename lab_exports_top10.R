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
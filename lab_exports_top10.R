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

## Part 3: Build Top 10 list by year ----

# For each year, keep only the top 10 countries by export value
top10_exports <- exports_cty_yr_clean %>%
  group_by(YEAR) %>%
  slice_max(order_by = ALL_VAL_YR, n = 10, with_ties = FALSE) %>%
  arrange(YEAR, desc(ALL_VAL_YR))

# Assign a rank (1-10) within each year
top10_exports <- top10_exports %>%
  group_by(YEAR) %>%
  arrange(-ALL_VAL_YR, CTY_NAME) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# Take a look
print(top10_exports)

## Part 4: Plot top 10 destinations for 2015 ----

yrplot <- 2015

p_2015 <- ggplot(top10_exports %>% filter(YEAR == yrplot),
                 aes(group = CTY_NAME, y = rank)) +
  geom_tile(aes(x = ALL_VAL_YR / 2,
                width = ALL_VAL_YR,
                height = .5,
                color = CTY_NAME,
                fill = CTY_NAME),
            show.legend = FALSE) +
  geom_text(aes(x = ALL_VAL_YR, y = rank, label = CTY_NAME),
            nudge_x = 15, show.legend = FALSE) +
  scale_y_reverse(breaks = 1:10, minor_breaks = NULL) +
  labs(x = "Export Value (billions USD)",
       y = "Ranking by Exports",
       title = paste("Top 10 Destination Countries for U.S. Exports,", yrplot)) +
  theme_minimal()

# Display the plot
print(p_2015)

# Save the plot as PNG
ggsave("top10_exports_2015.png", plot = p_2015,
       width = 10, height = 6, dpi = 150)

## Part 5: Plot top 10 destinations for 2025 ----

yrplot <- 2025

p_2025 <- ggplot(top10_exports %>% filter(YEAR == yrplot),
                 aes(group = CTY_NAME, y = rank)) +
  geom_tile(aes(x = ALL_VAL_YR / 2,
                width = ALL_VAL_YR,
                height = .5,
                color = CTY_NAME,
                fill = CTY_NAME),
            show.legend = FALSE) +
  geom_text(aes(x = ALL_VAL_YR, y = rank, label = CTY_NAME),
            nudge_x = 15, show.legend = FALSE) +
  scale_y_reverse(breaks = 1:10, minor_breaks = NULL) +
  labs(x = "Export Value (billions USD)",
       y = "Ranking by Exports",
       title = paste("Top 10 Destination Countries for U.S. Exports,", yrplot)) +
  theme_minimal()

# Display the plot
print(p_2025)

# Save as PNG
ggsave("top10_exports_2025.png", plot = p_2025,
       width = 10, height = 6, dpi = 150)

#install packages
install.packages("gifski")
install.packages("gganimate")
library(gifski)
library(gganimate)
# Build the base plot using all years of top10 data
p_anim <- ggplot(top10_exports, aes(group = CTY_NAME, y = rank)) +
  geom_tile(aes(x = ALL_VAL_YR / 2,
                width = ALL_VAL_YR,
                height = .5,
                color = CTY_NAME,
                fill = CTY_NAME),
            show.legend = FALSE) +
  geom_text(aes(x = ALL_VAL_YR, y = rank, label = CTY_NAME),
            nudge_x = 15, show.legend = FALSE) +
  scale_y_reverse(breaks = 1:10, minor_breaks = NULL) +
  labs(x = "Export Value (billions USD)",
       y = "Ranking by Exports",
       title = 'Top 10 Destination Countries for U.S. Exports: Year {closest_state}') +
  theme_minimal()

# Add animation across YEAR
animated_plot <- p_anim +
  transition_states(YEAR, transition_length = 2, state_length = 2, wrap = FALSE) +
  ease_aes('linear')

# Render and save the animation
anim <- animate(animated_plot,
                nframes = 100,
                fps = 10,
                width = 800,
                height = 600,
                start_pause = 10,
                end_pause = 10,
                renderer = gifski_renderer())

anim_save("top10_exports_over_time.gif", animation = anim)
print(anim)

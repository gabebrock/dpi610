
#' Crosswalk Opportunity Atlas; 2010 to 2020 tract boundaries
#' for Harris County, TX
#'
#' A U.S. census tract typically contains between 1,200 and 8,000 people, 
#' with an optimum size of roughly 4,000 residents.
#' Each Census, as a population grows, the Bureau will split tracts into smaller
#' tracts of optimal size. The crosswalk files allow for re-calibration of legacy
#' Census data to current GEOIDs and therefore geometries. 
#' 
#' The 2020 Census Tract (TRACT) to 2010 TRACT Relationship File provides 
#' a list of unique relationships between the 2020 tracts and 2010 tracts. 
#' Each record in the file represents one relationship that is formed when a 2020 
#' TRACT intersects a 2010 TRACT. This file represents intersections of geography 
#' only.
#' 
#' Census tract crosswalk math enables comparing data across different census 
#' years (e.g., 2010 to 2020) by apportioning data from source tracts to target 
#' tracts based on weights. 
#' It involves multiplying source data (e.g., population, households) by a 
#' ratio (typically population), housing units, or area—and summing the 
#' proportional parts in overlapping areas.
#' 
#' See Explanation of the 2020 Census Tract to 2010 Census Tract Relationship File
#' [here](https://www2.census.gov/geo/pdfs/maps-data/data/rel2020/tract/explanation_tab20_tract20_tract10.pdf)

library(tidyverse)
library(tidycensus)
library(haven)

# --- 1. Load & prep Opportunity Atlas data ---
opp_atlas <- read_dta("~/Projects/dpi610/final/data/atlas.dta") |>
  mutate(across(c(state, county, tract), as.character)) |>
  filter(state == "48", county == "201") |>
  mutate(GEOID_TRACT_10 = paste0( # construct census GEOID manually
    str_pad(state,  2, pad = "0"),
    str_pad(county, 3, pad = "0"),
    str_pad(tract,  6, pad = "0")
  ))

# --- 2. Load crosswalk ---
crosswalk <- read_csv(
  "~/Projects/dpi610/final/data/harris_tract_crosswalk_2010_2020.csv",
  col_types = cols(GEOID_TRACT_10 = col_character(), # read a char. so R doesn't drop leading '48'
                   GEOID_TRACT_20 = col_character())
)

# --- 3. Get 2010 tract populations (denominators for rate weighting) ---
pop_2010 <- get_decennial(
  geography = "tract",
  variables = c(population = "P001001"),
  state     = "TX",
  county    = "Harris County",
  year      = 2010
) |>
  select(GEOID_TRACT_10 = GEOID, pop_2010 = value)

# --- 4. Join crosswalk + OA data + population ---
merged <- crosswalk |>
  left_join(opp_atlas, by = "GEOID_TRACT_10") |>
  left_join(pop_2010,  by = "GEOID_TRACT_10") |>
  mutate(pop_alloc = pop_2010 * weight_2010_to_2020)

# --- 5. Interpolate variables to 2020 tract boundaries ---
#' Population-weighted average: sum(rate * pop_slice) / sum(pop_slice)
#' Area-weighted average used for pm25 (geographic, not person-based)

oa_vars <- c(
  # Household income ranks
  "kfr_pooled_pooled_p25", "kfr_natam_pooled_p25", "kfr_asian_pooled_p25",
  "kfr_black_pooled_p25",  "kfr_hisp_pooled_p25",  "kfr_white_pooled_p25",
  # Individual income ranks
  "kir_pooled_female_p25", "kir_pooled_male_p25",
  "kir_natam_female_p25",  "kir_asian_female_p25", "kir_black_female_p25",
  "kir_hisp_female_p25",   "kir_white_female_p25",
  "kir_natam_male_p25",    "kir_asian_male_p25",   "kir_black_male_p25",
  "kir_hisp_male_p25",     "kir_white_male_p25",
  # Incarceration rates
  "jail_pooled_pooled_p25", "jail_natam_pooled_p25", "jail_asian_pooled_p25",
  "jail_black_pooled_p25",  "jail_hisp_pooled_p25",  "jail_white_pooled_p25",
  "jail_pooled_female_p25", "jail_pooled_male_p25",
  "jail_natam_female_p25",  "jail_asian_female_p25", "jail_black_female_p25",
  "jail_hisp_female_p25",   "jail_white_female_p25",
  "jail_natam_male_p25",    "jail_asian_male_p25",   "jail_black_male_p25",
  "jail_hisp_male_p25",     "jail_white_male_p25",
  # Teen birth rates
  "teenbrth_pooled_female_p25", "teenbrth_asian_female_p25",
  "teenbrth_natam_female_p25",  "teenbrth_black_female_p25",
  "teenbrth_hisp_female_p25",   "teenbrth_white_female_p25"
)

atlas_hc_2020 <- merged |>
  # multiply each variable by its allocated population slice
  mutate(across(all_of(oa_vars), ~ .x * pop_alloc,
                .names = "{.col}_x_pop")) |>
  summarize(
    pop_2020_est = sum(pop_alloc,    na.rm = TRUE),
    pm25_1982    = sum(pm25_1982 * weight_2010_to_2020, na.rm = TRUE), # area-weighted
    across(ends_with("_x_pop"), ~ sum(.x, na.rm = TRUE)),
    .by = GEOID_TRACT_20
  ) |>
  # recover rates by dividing numerators by total allocated population
  mutate(across(ends_with("_x_pop"),
                ~ .x / pop_2020_est,
                .names = "{str_remove(.col, '_x_pop')}")) |>
  select(GEOID_TRACT_20, pop_2020_est, pm25_1982, all_of(oa_vars))

rm(merged, opp_atlas, pop_2010, crosswalk)

# Note: NAs in result_2020 reflect suppressed cells in the source data
# (small sample sizes in Opportunity Atlas), not a join problem.
# HOLC_grade is categorical handle separately if needed.
# zcta_to_tract.R
# ---------------------------------------------------------------
# Crosswalk ZCTA-level data to 2010 Census tracts using the
# Census Bureau's official ZCTA-Tract relationship file.
#
# Relationship file source:
#   zcta_tract_rel_10.txt  (Census 2010, nationwide)
#   https://www.census.gov/geographies/reference-files/2010/geo/relationship-files.2010.html
#
# Key weight columns in the relationship file:
#   ZPOPPCT  – share of the ZCTA's population in this ZCTA×Tract cell
#   ZHUPCT   – share of the ZCTA's housing units in this cell
#   ZAREAPCT – share of the ZCTA's total area in this cell
#
# Usage:
#   1. Set the paths in the CONFIG section below.
#   2. List your numeric ZCTA-level variables in `sc_vars`.
#   3. Choose a weight (population, housing unit, or area).
#   4. source("zcta_to_tract.R")  — writes tract_output.csv
# ---------------------------------------------------------------

library(tidyverse)
library(tidycensus)

# ── LOAD DATA ────────────────────────────────────────────────────

# Read 2010 ZCTA to Tract Census relationship (crosswalk) file
rel <- read_csv("~/Projects/dpi610/final/data/zcta_tract_rel_10.txt",
                col_types = cols(.default = "c")) |>
  mutate(
    ZCTA5  = str_pad(ZCTA5,  5, pad = "0"),
    GEOID  = str_pad(GEOID, 11, pad = "0"),   # 11-digit tract FIPS
    weight = as.numeric(.data[["ZPOPPCT"]]) / 100  # convert % to proportion
  ) |>
  select(ZCTA5, GEOID, weight)

#' Used `ZPOPPCT` (population share) as allocation weight

# Read ZCTA-level Social Capital data
message("Loading your ZCTA data...")

# Values to crosswalk to tracts
sc_vars <- c("ec_zip", "nbhd_ec_zip")

zcta_data <- read_csv("~/Projects/dpi610/final/data/social_capital.csv", 
                      col_types = cols(.default = "c")) |>
  rename(ZCTA5 = all_of("zip")) |>
  mutate(
    ZCTA5 = str_pad(ZCTA5, 5, pad = "0"),
    across(all_of(sc_vars), as.numeric)
  )


# ── CROSSWALK ────────────────────────────────────────────────────
# For each ZCTA × Tract pair:
#   tract_value = zcta_value × (weight / sum_of_weights_for_that_zcta)
#
# Weights don't always sum to exactly 100 due to rounding, so we
# normalise within each ZCTA to ensure values are fully allocated.

message("Crosswalking ZCTAs to 2010 Tracts...")
crosswalk <- rel |>
  group_by(ZCTA5) |>
  mutate(weight_norm = weight / sum(weight, na.rm = TRUE)) |>
  ungroup()

tract_data <- zcta_data |>
  inner_join(crosswalk, by = "ZCTA5", relationship = "many-to-many") |>
  mutate(across(all_of(sc_vars), \(x) x * weight_norm)) |>
  group_by(GEOID) |>
  summarize(across(all_of(sc_vars), \(x) sum(x, na.rm = TRUE)),
            .groups = "drop")


# ── DIAGNOSTICS ──────────────────────────────────────────────────

n_zcta_in   <- n_distinct(zcta_data$ZCTA5)
n_zcta_match <- n_distinct(
  semi_join(zcta_data, crosswalk, by = "ZCTA5")$ZCTA5
)
n_zcta_drop  <- n_zcta_in - n_zcta_match

message(sprintf(
  "ZCTAs in your data : %d\n  matched in crosswalk : %d\n  unmatched (dropped) : %d",
  n_zcta_in, n_zcta_match, n_zcta_drop
))
if (n_zcta_drop > 0) {
  unmatched <- anti_join(zcta_data, crosswalk, by = "ZCTA5")$ZCTA5
  message("  Unmatched ZCTAs: ", paste(head(unmatched, 20), collapse = ", "),
          if (length(unmatched) > 20) " ..." else "")
}
message(sprintf("Output tracts: %d", nrow(tract_data)))


# ── CROSSWALK to 2020 ────────────────────────────────────────────

message("Crosswalking 2010 Tracts to 2020 Tracts...")

rel_20 <- read_csv(
  "~/Projects/dpi610/final/data/harris_tract_crosswalk_2010_2020.csv",
  col_types = cols(GEOID_TRACT_10 = col_character(), # read a char. so R doesn't drop leading '48'
                   GEOID_TRACT_20 = col_character())
)

tract_data_hc <- tract_data |>
  filter(str_detect(GEOID, "48201")) 

# --- Get 2010 tract populations (denominators for rate weighting) ---
pop_2010 <- get_decennial(
  geography = "tract",
  variables = c(population = "P001001"),
  state     = "TX",
  county    = "Harris County",
  year      = 2010
) |>
  select(GEOID_TRACT_10 = GEOID, pop_2010 = value)

# --- 4. Join crosswalk + OA data + population ---
merged <- rel_20 |>
  left_join(tract_data_hc, by = join_by("GEOID_TRACT_10" == "GEOID")) |>
  left_join(pop_2010,  by = "GEOID_TRACT_10") |>
  mutate(pop_alloc = pop_2010 * weight_2010_to_2020)

sc_hc_2020 <- merged |>
  # multiply each variable by its allocated population slice
  mutate(across(all_of(sc_vars), ~ .x * pop_alloc,
                .names = "{.col}_x_pop")) |>
  summarize(
    pop_2020_est = sum(pop_alloc,    na.rm = TRUE),
    across(ends_with("_x_pop"), ~ sum(.x, na.rm = TRUE)),
    .by = GEOID_TRACT_20
  ) |>
  # recover rates by dividing numerators by total allocated population
  mutate(across(ends_with("_x_pop"), ~ .x / pop_2020_est,
                .names = "{str_remove(.col, '_x_pop')}")) |>
  select(GEOID_TRACT_20, pop_2020_est, all_of(sc_vars))


# ── WRITE OUTPUT ─────────────────────────────────────────────────

write_csv(tract_data, "~/Projects/dpi610/final/data/social_capital_tracts.csv")
message("Done — wrote: ", "~/Projects/dpi610/final/data/social_capital_tracts.csv")
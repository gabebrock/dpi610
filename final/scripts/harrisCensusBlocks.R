# -----------------------------------------------------------------------------
# harrisCensusBlocks.R
# Purpose: Pull 2020 Census block data for Harris County, TX.
# Returns an sf object with block geometries, total population/VAP,
# and 4-category (White, Black, Hispanic, Other) race shares for both
# total population and voting-age population.
# -----------------------------------------------------------------------------

# ---- 0. Load Packages -------------------------------------------------------
pkgs <- c("sf", "tidyverse", "censable", "tigris")

check_pkgs <- function(pkgs) {
  for (pkg in pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    library(pkg, character.only = TRUE)
  }
}
check_pkgs(pkgs)

# Use planar geometry for Texas
sf_use_s2(FALSE)

# ---- 1. Get Harris County, TX 2020 Census Block Geometries ------------------
# tigris provides block geometries (2020) with basic fields
cat("Downloading 2020 Census block geometries for Harris County, TX...\n")
tx_blocks <- blocks(state = "TX", county = "Harris County", year = 2020)

# ---- 2. Pull 2020 PL 94-171 race data (total pop and VAP) -------------------
cat("Downloading 2020 PL 94-171 race data (population and VAP)...\n")
# Use censable to pull block-level PL 94-171 data for Harris County
# This provides standard redistricting variables including race breakdowns
hc_blocks_raw <- censable::build_dec(
  geography = "block",
  state = "TX",
  county = "Harris County",
  geometry = FALSE  # geometry will be joined from tigris
)

# ---- 3. Join geometry and demographic data ----------------------------------
cat("Joining geometries to demographic table...\n")
# Ensure consistent GEOID formatting
hc_blocks <- tx_blocks %>%
  mutate(GEOID20 = as.character(GEOID20)) %>%
  left_join(
    hc_blocks_raw %>% mutate(GEOID = as.character(GEOID)),
    by = c("GEOID20" = "GEOID")
  )

# ---- 4. Collapse race categories to 4 (White, Black, Hispanic, Other) -------
cat("Collapsing race categories to 4 (White, Black, Hispanic, Other)...\n")
# censable::collapse4 collapses detailed race groups into the 4-category scheme
# It works on columns with the specified prefixes
hc_blocks_4cat <- hc_blocks %>%
  collapse4(prefix = c("pop_", "vap_"))

# ---- 5. Compute race share variables (proportions) -------------------------
cat("Computing race share variables for population and VAP...\n")
hc_blocks_4cat <- hc_blocks_4cat %>%
  mutate(
    # Total population race shares
    pct_white = pop_white / pop,
    pct_black = pop_black / pop,
    pct_hisp  = pop_hisp  / pop,
    pct_other = pop_other / pop,
    # Voting-age population race shares
    pct_vap_white = vap_white / vap,
    pct_vap_black = vap_black / vap,
    pct_vap_hisp  = vap_hisp  / vap,
    pct_vap_other = vap_other / vap
  )

# ---- 6. Select and reorder final columns ------------------------------------
cat("Preparing final sf object...\n")
hc_blocks_final <- hc_blocks_4cat %>%
  st_as_sf() %>%
  select(
    GEOID20,
    pop, vap,
    starts_with("pop_"),
    starts_with("vap_"),
    starts_with("pct_"),
    geometry
  )

# ---- 7. Summary / Save ------------------------------------------------------
cat("\nSummary of final sf object:\n")
print(hc_blocks_final)

cat("\nNumber of blocks:", nrow(hc_blocks_final), "\n")
cat("Coordinate reference system:", st_crs(hc_blocks_final)$NameInput, "\n")

# Optional: Save to an RDS file for fast loading later
save_path <- here::here("data", "harris_census_blocks_2020_4cat.rds")
if (!dir.exists(dirname(save_path))) dir.create(dirname(save_path), recursive = TRUE)
saveRDS(hc_blocks_final, save_path)
cat("\nSaved to:", save_path, "\n")

# ---- 8. Plot Map of Blocks --------------------------------------------------
cat("\nPlotting map of Harris County census blocks...\n")
# Ensure a clean graphics device
if (dev.cur() != 1) dev.off()
# Open a new device and plot
png(filename = "harris_blocks_map.png", width = 10, height = 8, units = "in", res = 300)
p <- ggplot() +
  geom_sf(data = hc_blocks_final, fill = "steelblue", color = "white", size = 0.1) +
  theme_minimal() +
  labs(title = "Harris County, TX: 2020 Census Blocks",
       subtitle = paste0("Number of blocks: ", nrow(hc_blocks_final))) +
  theme(plot.title = element_text(face = "bold"),
        plot.subtitle = element_text(color = "gray30"))
print(p)
dev.off()
cat("Map saved to: harris_blocks_map.png\n")

# -----------------------------------------------------------------------------
# End of script
# -----------------------------------------------------------------------------
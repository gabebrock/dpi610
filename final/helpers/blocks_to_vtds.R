
#  ---------------------------------------
#' Crosswalk census blocks up to VTDs
#' get demographic statistics at the VTD-level

# plot VTDs
z1 <- ggplot2::ggplot(hcVotes_2026) +
  ggplot2::geom_sf(aes(fill = log(totalVotes))) +
  theme_void()

# get Harris County census blocks
harris <- tidycensus::get_acs(geography = "tract", 
                              variables = "B19013_001",
                              state = "TX", county = "Harris County",
                              geometry = TRUE, year = 2024)
# plot census blocks
z2 <- ggplot2::ggplot(harris) +
  ggplot2::geom_sf(aes(fill = log(estimate))) +
  theme_void()

# display block and VTD plots together
cowplot::plot_grid(z1, z2, nrow = 2)

# ----------------------------------------
# crosswalk blocks to VTD

# ensure both datasets have the same CRS
blocks <- st_transform(harris, st_crs(hcVotes_2026))

#' create crosswalk
#' assign each block to a VTD based on polygon intersection
block_vtd_crosswalk <- st_intersection(blocks, hcVotes_2026) %>%
  st_drop_geometry() %>%
  select(GEOID, OBJECTID) %>%
  rename(block_geoid = GEOID, vtd_objectid = OBJECTID)

#' aggregate demographic data to VTD level
#' population-weighted average of block-level estimates
vtd_demographics <- blocks %>%
  st_drop_geometry() %>%
  left_join(block_vtd_crosswalk, 
            by = c("GEOID" = "block_geoid")) %>%
  group_by(vtd_objectid) %>%
  summarize(
    median_income = weighted.mean(estimate, w = estimate, na.rm = TRUE),
    total_blocks = n(),
    .groups = 'drop'
  ) %>%
  filter(!is.na(vtd_objectid))

# Merge demographic data with voting data
hcVotes_2026_with_demographics <- hcVotes_2026 %>%
  left_join(vtd_demographics, by = c("OBJECTID" = "vtd_objectid"))

# Display summary statistics
cat("Crosswalk summary:\n")
cat("Total blocks processed:", nrow(blocks), "\n")
cat("Total VTDs matched:", nrow(vtd_demographics), "\n")
cat("VTDs with demographic data:", sum(!is.na(hcVotes_2026_with_demographics$median_income)), "\n")

# Validation checks
cat("\nValidation checks:\n")
cat("Blocks with valid income data:", sum(!is.na(blocks$estimate)), "\n")
cat("VTDs with complete data:", sum(complete.cases(vtd_demographics)), "\n")

# Create visualization of the crosswalk results
if (requireNamespace("ggplot2", quietly = TRUE) && requireNamespace("cowplot", quietly = TRUE)) {
  # Plot VTDs with demographic data
  vtd_plot <- ggplot2::ggplot(hcVotes_2026_with_demographics) +
    ggplot2::geom_sf(aes(fill = median_income)) +
    ggplot2::scale_fill_viridis_c(na.value = "gray") +
    ggplot2::labs(title = "VTDs with Median Household Income", fill = "Median Income") +
    ggplot2::theme_void()
  
  # Plot blocks for comparison
  block_plot <- ggplot2::ggplot(blocks) +
    ggplot2::geom_sf(aes(fill = estimate)) +
    ggplot2::scale_fill_viridis_c(na.value = "gray") +
    ggplot2::labs(title = "Census Blocks - Median Household Income", fill = "Median Income") +
    ggplot2::theme_void()
  
  # Display plots together
  cowplot::plot_grid(block_plot, vtd_plot, nrow = 2)
}

# Save the crosswalk results for future use
save(block_vtd_crosswalk, vtd_demographics, hcVotes_2026_with_demographics, 
     file = "data/block_vtd_crosswalk.rda")

cat("\nCrosswalk results saved to data/block_vtd_crosswalk.rda\n")


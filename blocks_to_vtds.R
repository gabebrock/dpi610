
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
                              geometry = TRUE, year = 2020)
# plot census blocks
z2 <- ggplot2::ggplot(harris) +
  ggplot2::geom_sf(aes(fill = log(estimate))) +
  theme_void()

# display block and VTD plots together 
cowplot::plot_grid(z1, z2, nrow = 2)


# crosswalk blocks to VTD



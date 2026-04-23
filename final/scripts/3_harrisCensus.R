
# ---- Harris County Census Data (Tract-level) ----

#' Create a dataset, using the decennial census information, with the standard variables used for redistricting. 
hc <- censable::build_dec(geography = 'tract', # get tract-level data
                          state = 'TX', county = 'Harris County', # for Harris County, TX
                          geometry = TRUE) # include tract sfs

#' Collapse Full Race Categories into 4 Categories
#' Collapses Other, AIAN, Asian, NHPI, and Two+ into other. 
hc_4cat <- hc %>% collapse4(prefix = c('pop_', 'vap_')) 

# Create race-share variables (as proportions) for total population and VAP
hc_4cat <- hc_4cat %>%
  mutate(
    # percent population
    pct_white = pop_white / pop,
    pct_black = pop_black / pop,
    pct_hisp  = pop_hisp  / pop,
    pct_other = pop_other / pop,
    # percent VAP
    pct_vap_white = vap_white / vap,
    pct_vap_black = vap_black / vap,
    pct_vap_hisp  = vap_hisp  / vap,
    pct_vap_other = vap_other / vap
  )

# ---- Harris County Census Data (Block-level) ----

hc_blocks <- censable::build_dec(geography = 'block', # get block-level data
                                 state = 'TX', county = 'Harris County', # for Harris County, TX
                                 geometry = TRUE) # include block sfs

#' Collapse Full Race Categories into 4 Categories
hc_blocks_4cat <- hc_blocks %>% collapse4(prefix = c('pop_', 'vap_')) 

# Create race-share variables (as proportions) for total population and VAP
hc_blocks_4cat <- hc_blocks_4cat %>%
  mutate(
    # percent population
    pct_white = pop_white / pop,
    pct_black = pop_black / pop,
    pct_hisp  = pop_hisp  / pop,
    pct_other = pop_other / pop,
    # percent VAP
    pct_vap_white = vap_white / vap,
    pct_vap_black = vap_black / vap,
    pct_vap_hisp  = vap_hisp  / vap,
    pct_vap_other = vap_other / vap
  )

# ---- Save Census Data ----
#' Fetching block-level data takes a long time
#' Save the four census df to an RData file 
#' so the quarto document can load from the file instead of calling API each render
save(
  hc,
  hc_4cat,
  hc_blocks,
  hc_blocks_4cat,
  file = "~/Projects/dpi610/final/data/harris_county_census_data.RData"
)



# ---- R precinct-level vote share map ----

# Compute winner + vote share per precinct
precinct_results <- hcVotes_2026 %>%
  mutate(
    max_votes = pmax(`JAMES TALARICO`, `JASMINE CROCKETT`, `AHMAD R. HASSAN`, na.rm = TRUE),
    winner = case_when(
      `JAMES TALARICO` == max_votes ~ "JAMES TALARICO",
      `JASMINE CROCKETT` == max_votes ~ "JASMINE CROCKETT",
      `AHMAD R. HASSAN`  == max_votes ~ "AHMAD R. HASSAN"
    ),
    vote_share = max_votes / totalVotes
  )

precinct_results <- st_as_sf(precinct_results)
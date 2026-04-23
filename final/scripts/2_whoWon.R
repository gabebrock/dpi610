# ---- who won the election ----
who_won <- hcVotes_2026 %>%
  pivot_longer(
    cols = c(`JAMES TALARICO`, `JASMINE CROCKETT`, `AHMAD R. HASSAN`),
    names_to = "candidate",
    values_to = "votes"
  ) %>%
  group_by(Precinct, candidate) %>%
  summarize(total_votes = sum(votes, na.rm = TRUE), .groups = "drop") 

# ---- vote tally ----
who_won_tally <- who_won %>%
  st_drop_geometry() %>%
  group_by(candidate) %>%
  summarize(total_votes = sum(total_votes, na.rm = TRUE)) %>%
  mutate(prop = total_votes / sum(total_votes)) 

# who won each precinct?
who_won_pct <- who_won %>%
  group_by(Precinct) %>%
  slice_max(total_votes, n = 1, with_ties = FALSE)

# ---- Candidate Vote Share ----
# how many precincts did each candidate win?
who_won_pct %>%
  group_by(candidate) %>%
  count()

# largest precinct-level vote share (precincts > 10 votes)
who_won %>%
  group_by(Precinct) %>%
  mutate(tally = sum(total_votes),
         vote_share = total_votes / tally) %>%
  slice_max(vote_share, n = 1, with_ties = FALSE) %>%
  filter(total_votes > 10) %>%
  ungroup() %>%
  slice_max(vote_share, n = 10, with_ties = FALSE)

# smallest precinct-level vote share (precincts > 10 votes)
who_won %>%
  group_by(Precinct) %>%
  mutate(tally = sum(total_votes),
         vote_share = total_votes / tally) %>%
  slice_max(vote_share, n = 1, with_ties = FALSE) %>%
  filter(total_votes > 10) %>%
  ungroup() %>%
  slice_min(vote_share, n = 10, with_ties = FALSE)
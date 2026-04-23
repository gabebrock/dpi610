
# ---- Load and prep voter data ----
hcVotes_2026 <- read_csv("data/election_results_clean.csv")

  # convert columns to integer
  hcVotes_2026 <- hcVotes_2026 %>%
    mutate(across(everything(), as.integer))
  
  hcVotes_2026 <- hcVotes_2026 %>%
    select(Precinct, `JAMES TALARICO`, `JASMINE CROCKETT`, `AHMAD R. HASSAN`,
           `Early Voting`, `Election Day`, `Total Ballots Cast`) %>%
    left_join(pcts %>% select(OBJECTID, VPCT_txt, VPCT, JP_Constab,
                              Shape.STArea.., Shape.STLength.., geometry), 
              by = join_by(Precinct == VPCT))
  
  # get Republican votes so we can tally total votes
  hcVotes_2026R <- read_csv("data/election_results_republican_clean.csv")
  hcVotes_2026R <- hcVotes_2026R %>%
    select(Precinct, `Total Ballots Cast`) %>%
    rename(totalVotesR = `Total Ballots Cast`) %>%
    mutate(Precinct = as.integer(Precinct),
           totalVotesR = as.integer(totalVotesR))

hc_voters <- read_csv("data/reg_precinct_20240210.csv")
hc_voters <- hc_voters %>% rename(Precinct = precinct, 
                                  registerVoters = count_precinct)

# append Republican primary votes to df
hcVotes_2026 <- hcVotes_2026 %>%
  left_join(hcVotes_2026R, by = join_by(Precinct))

# append number of registered voters in each voter precinct
hcVotes_2026 <- hcVotes_2026 %>%
  left_join(hc_voters, by = join_by(Precinct))

  # rename df variables
  lookup <- c(earlyVoting = "Early Voting", electionDay = "Election Day",
              totalVotesD = "Total Ballots Cast",
              constable = "JP_Constab",
              shape_area = "Shape.STArea..", shape_length = "Shape.STLength..")
  
  hcVotes_2026 <- hcVotes_2026 %>%
    rename(all_of(lookup))

# create vote tally and turnout numbers
hcVotes_2026 <- hcVotes_2026 %>%  
  mutate(totalVotes = totalVotesD + totalVotesR,
         turnout = totalVotes / registerVoters)
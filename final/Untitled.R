library(tidyverse)
library(spdep)

# ---- Create Dem Vote Share Variable
hcVotes_2026 <- hcVotes_2026 %>%
  mutate(dvs = totalVotesD / totalVotes,
         max_votes = pmax(`JAMES TALARICO`, `JASMINE CROCKETT`, `AHMAD R. HASSAN`, na.rm = TRUE),
         winner = case_when(
           `JAMES TALARICO` == max_votes ~ "JAMES TALARICO",
           `JASMINE CROCKETT` == max_votes ~ "JASMINE CROCKETT",
           `AHMAD R. HASSAN`  == max_votes ~ "AHMAD R. HASSAN"
         ),
         vote_share = max_votes / totalVotes)

# ---- Step 1: Define neighboring census tracts ----

hcVotes_2026_clean <- hcVotes_2026[!st_is_empty(hcVotes_2026), ]

#' construct a list of neighbors based on areas with contiguous boundaries
#' poly2nb() accepts a list of tracts and 
#' returns a list of class nb with the neighbors of each area
nb <- poly2nb(hcVotes_2026_clean, queen = TRUE)
# head(nb)

#' plot the tracts map and overlap the neighborhood structure with the 
#' plot.nb() function passing the neighbor list and the coordinates of the map
plot(st_geometry(hcVotes_2026_clean), border = "lightgray")
plot.nb(nb, st_geometry(hcVotes_2026_clean), add = TRUE)

#' We can plot the neighbors of a given area 
#' by adding a new column in map representing the neighbors of the area
cards <- card(nb) # number of neighbors per tract
max_id <- which.max(cards) # tract with the most neighbors

hcVotes_2026_clean$neighbors <- "other"
hcVotes_2026_clean$neighbors[max_id] <- "area"
hcVotes_2026_clean$neighbors[nb[[max_id]]] <- "neighbors"

ggplot(hcVotes_2026_clean) + 
  geom_sf(aes(fill = neighbors)) +
  scale_fill_manual(values = c("red2", "green2", "white")) +
  theme_void()

# ---- Step 2: Assign weights to the neighbors ----

#' Each neighboring tract will be assigned equal weight when 
#' computing the neighboring mean income values.
#' 
#' To compute a spatial weights matrix based on a binary neighbor list, 
nbw <- nb2listw(nb, # list with neighbors
                style = "W", #' coding scheme chosen
                zero.policy = TRUE) # take into account polys with 0 neighbors

# see the weight of the first three tracts’s neighbors
# nbw$weights[1:3]
#' These are the weights each neighboring income value 
#' will be multiplied by before being summed
#' If a polygon has 5 neighbors, each neighbor will have a weight of 1/5 or 0.2 
#' This is equivalent to summing all five income values then dividing by 5

# ---- Step 3: Compute the weighted neighbor mean income values ----

#' Democratic Vote Share ----
#' Compute the average neighbor income value for each polygon
#' These are the spatially lagged values
#' Spatial Lag
keep <- is.finite(hcVotes_2026_clean$dvs)
nbw2 <- subset(nbw, subset = keep, zero.policy = TRUE)
y2 <- hcVotes_2026_clean$dvs[keep]

Vx <- lag.listw(nbw2, y2, zero.policy = TRUE)
df <- data.frame(dvs = y2,
                 lag_dvs = Vx,
                 totalVotes = hcVotes_2026_clean$totalVotes[keep],
                 constable = hcVotes_2026_clean$constable[keep])

ggplot(df, aes(x = dvs, y = lag_dvs, 
               color = "blue",
               alpha = 0.1)) +
  geom_point(shape = 19) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(
    x = "Democratic Vote Share (DVS)",
    y = "Spatial Lag of DVS"
  ) +
  theme_minimal()

# Fit the linear model
model <- lm(lag_dvs ~ dvs, data = df)

# ---- Step 4: Computing the Moran’s I statistic ---
moran <- moran.test(y2, nbw2, zero.policy = TRUE)
moran

moran.plot(y2, nbw2, zero.policy = TRUE)

#' Candidate Vote Share ----
keep <- is.finite(hcVotes_2026_clean$vote_share)
nbw2 <- subset(nbw, subset = keep, zero.policy = TRUE)
y2 <- hcVotes_2026_clean$vote_share[keep]

Vx <- lag.listw(nbw2, y2, zero.policy = TRUE)
df <- data.frame(vote_share = y2,
                 lag_vote_share = Vx,
                 totalVotes = hcVotes_2026_clean$totalVotes[keep],
                 constable = hcVotes_2026_clean$constable[keep])

ggplot(df, aes(x = vote_share, y = lag_vote_share, 
               size = vote_share, color = constable,
               alpha = 0.1)) +
  geom_point(shape = 19) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  scale_color_viridis_c(option = "viridis") +
  labs(
    x = "Candidate Vote Share (CVS)",
    y = "Spatial Lag of CVS"
  ) +
  theme_minimal()

# Fit the linear model
model <- lm(lag_vote_share ~ vote_share, data = df)

# ---- Step 4: Computing the Moran’s I statistic ---
moran <- moran.test(y2, nbw2, zero.policy = TRUE)
moran

moran.plot(y2, nbw2, zero.policy = TRUE)



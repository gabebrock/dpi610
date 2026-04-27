
# ---- Install/Load Packages ----
# list all packages required for project
pkgs <- c("sf", "tidyverse", "kableExtra", "urbnthemes",
          "censable", "cvap", "patchwork", "extrafont", "magrittr",
          "haven", "tigris", "rticles", "corrplot", "scales")

# define function to check install and load pkgs
check_pkgs <- function(pkgs) {
  for (pkg in pkgs) { # for each package
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg) }
    library(pkg, character.only = TRUE)
  }
}

check_pkgs(pkgs)
sf_use_s2(FALSE)  # use planar geometry; avoids edge cases with s2 spherical geometry

# ---- Load Harris county GIS data ----
if (file.exists("data/gis-harris.rda")) {
  # cat("Harris Counter GIS data exists already. Reading to env")
  load("data/gis-harris.rda")
} else {
  # read Harris Counter voter precincts and polling locations
  # cat("The file does not exist. Getting GIS data.")
  url <- "https://www.gis.hctx.net/arcgis/rest/services/Voting/VotingPct/MapServer/0/query?where=1=1&outFields=*&f=geojson"
  pcts <- st_read(url)
  url <- "https://www.gis.hctx.net/arcgis/rest/services/Voting/VotingPolls/MapServer/0/query?where=1=1&outFields=*&f=geojson"
  polls <- st_read(url)
  url <- "https://services3.arcgis.com/FsUrhUGHe9VfghT8/arcgis/rest/services/Harris_County_Highways/FeatureServer/0/query?where=1=1&outFields=*&f=geojson"
  roads <- st_read(url)
  save(pcts, polls, roads, file = "data/gis-harris.rda")
}

.setup_complete <- TRUE

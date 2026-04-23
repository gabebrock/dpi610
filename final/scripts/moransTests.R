library(sf)
library(mapview)

plot(hc_4cat["pct_black"])

# Neighbors
library(spdep)

nb <- spdep::poly2nb(hc_4cat, queen = TRUE) # queen shares point or border
nbw <- spdep::nb2listw(nb, style = "W")

# Global Moran's I
valid <- !is.na(hc_4cat$pct_black)
nbw_sub <- spdep::subset.listw(nbw, valid)
gmoran <- spdep::moran.test(
  hc_4cat$pct_black[valid],
  nbw_sub,
  alternative = "greater"
)
gmoran
gmoran[["estimate"]][["Moran I statistic"]] # Moran's I
gmoran[["statistic"]] # z-score
gmoran[["p.value"]] # p-value

# Moran’s I scatterplot
spdep::moran.plot(hc_4cat$pct_black[valid], nbw_sub)

# The localmoran() function
lmoran <- localmoran(hc_4cat$pct_black[valid], nbw_sub, alternative = "greater")
head(lmoran)

# ============================================================
# RQ2: Global Moran's I — Spatial Autocorrelation
# Geographic Entrenchment During the 2026 Texas Primary
# ============================================================

# ============================================================
# 1. BUILD SPATIAL WEIGHTS MATRIX
#    Queen contiguity: neighbors share any border > 0 (incl. corners)
# ============================================================

# --- Demographic layer ---
nb_demo  <- poly2nb(hc_4cat, queen = TRUE)
lw_demo  <- nb2listw(nb_demo, style = "W", zero.policy = TRUE)

# --- Election layer ---
# First filter precincts with area > 0
pct_morans <- precinct_results[as.numeric(st_area(precinct_results)) > 0, ]

# Then build spatial weights matrix from filtered data
nb_elec  <- poly2nb(pct_morans, queen = TRUE)
lw_elec  <- nb2listw(nb_elec, style = "W", zero.policy = TRUE)

# Check for any islands (precincts with no neighbors)
cat("Demographic layer — precincts with no neighbors:",
    sum(card(nb_demo) == 0), "\n")
cat("Election layer    — precincts with no neighbors:",
    sum(card(nb_elec) == 0), "\n")

# ============================================================
# 2. HELPER: RUN GLOBAL MORAN'S I AND RETURN TIDY ROW
# ============================================================

run_morans <- function(data, var, listw, label = NULL) {
  cat("Processing variable:", var, "\n")
  x <- data[[var]]
  cat("Variable class:", class(x), "\n")
  cat("Variable type:", typeof(x), "\n")
  cat("First few values:", head(x, 3), "\n")
  
  # Drop rows where x is NA (zero.policy handles isolated units)
  if (any(is.na(x))) {
    warning(paste("NA values found in", var, "— they will be removed."))
    x[is.na(x)] <- mean(x, na.rm = TRUE)   # mean-impute; flag in paper
  }
  
  # Ensure x is numeric
  if (!is.numeric(x)) {
    cat("Converting to numeric...\n")
    x <- as.numeric(x)
  }
  
  cat("Final variable class:", class(x), "\n")
  test <- moran.test(x, listw,
                     randomisation = TRUE,
                     zero.policy   = TRUE)
  
  tibble(
    variable    = if (!is.null(label)) label else var,
    moran_i     = round(test$estimate["Moran I statistic"],       4),
    expected_i  = round(test$estimate["Expectation"],             4),
    variance    = round(test$estimate["Variance"],                6),
    z_score     = round(test$statistic,                           4),
    p_value     = round(test$p.value,                             4),
    significant = ifelse(test$p.value < 0.05, "Yes", "No")
  )
}

# ============================================================
# 3. DEMOGRAPHIC VARIABLES: Global Moran's I
# ============================================================

demo_vars <- list(
  pct_white = "% White",
  pct_black = "% Black",
  pct_hisp  = "% Hispanic",
  pct_other = "% Other"
)

# Debug: Check demographic variables
cat("Demographic variables in hc_4cat:\n")
print(names(hc_4cat))
cat("Variables to test:", paste(names(demo_vars), collapse=", "), "\n")
cat("Types of variables:\n")
for(var in names(demo_vars)) {
  cat(var, ":", class(hc_4cat[[var]]), "\n")
}

demo_results <- map2_dfr(
  names(demo_vars),
  demo_vars,
  ~ run_morans(hc_4cat, .x, lw_demo, label = .y)
)

# ============================================================
# 4. ELECTION VARIABLES — Global Moran's I
# ============================================================

# Compute vote share proportions if raw tallies are stored
# Adjust column names to match your actual data
pct_morans <- pct_morans %>%
  mutate(
    talarico_share   = `JAMES TALARICO`  / totalVotesD,
    crockett_share   = `JASMINE CROCKETT` / totalVotesD
  )

elec_vars <- list(
  talarico_share = "Talarico Vote Share",
  crockett_share = "Crockett Vote Share"
)

# Debug: Check election variables
cat("\nElection variables in pct_morans:\n")
print(names(pct_morans))
cat("Variables to test:", paste(names(elec_vars), collapse=", "), "\n")
cat("Types of variables:\n")
for(var in names(elec_vars)) {
  cat(var, ":", class(pct_morans[[var]]), "\n")
}

elec_results <- map2_dfr(
  names(elec_vars),
  elec_vars,
  ~ run_morans(pct_morans, .x, lw_elec, label = .y)
)

# ============================================================
# 5. COMBINED RESULTS TABLE
# ============================================================

global_moran_results <- bind_rows(demo_results %>% mutate(layer = "Demography"),
                                  elec_results %>% mutate(layer = "Election")) %>%
  select(layer, variable, moran_i, expected_i, z_score, p_value, significant)

# print(global_moran_results)


# ============================================================
# 6. MORAN SCATTER PLOTS
#    Visualizes spatial lag vs. observed value for each variable
# ============================================================

moran_scatter <- function(data, var, listw, label) {
  x      <- data[[var]]
  
  # Handle non-finite values before standardization
  if (any(is.na(x))) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  }
  
  x_std  <- scale(x)[, 1]                          # standardize
  
  # Handle non-finite values from standardization (zero variance)
  if (any(!is.finite(x_std))) {
    x_std[!is.finite(x_std)] <- 0  # set to mean (which is 0 after scaling)
  }
  
  lag_x  <- lag.listw(listw, x_std, zero.policy = TRUE)  # spatial lag
  
  df <- tibble(x = x_std, lag = lag_x)
  
  ggplot(df, aes(x = x, y = lag)) +
    geom_point(alpha = 0.6, size = 1.8, color = "#2166ac") +
    geom_smooth(method = "lm", se = FALSE, color = "#d6604d", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    labs(
      title    = paste("Moran Scatter:", label),
      subtitle = paste0("I = ",
                        round(moran.test(x, listw, zero.policy = TRUE)$estimate[1], 3),
                        ",  p = ",
                        round(moran.test(x, listw, zero.policy = TRUE)$p.value,     3)),
      x        = paste("Standardized", label),
      y        = "Spatial Lag"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title    = element_text(face = "bold", size = 11),
          plot.subtitle = element_text(color = "grey40", size = 9))
}

# Demographic scatter plots
scatter_plots_demo <- imap(
  demo_vars,
  ~ moran_scatter(hc_4cat, .y, lw_demo, .x)
)

# Election scatter plots
scatter_plots_elec <- list(
  moran_scatter(pct_morans, "talarico_share", lw_elec, "Talarico Vote Share"),
  moran_scatter(pct_morans, "crockett_share", lw_elec, "Crockett Vote Share")
)

# Save combined demographic scatter grid
library(patchwork)
demo_scatter_grid <- wrap_plots(scatter_plots_demo, ncol = 2)
# ggsave("final/imgs/rq2_moran_scatter_demo.png", demo_scatter_grid, width = 10, height = 8, dpi = 300)

elec_scatter_grid <- wrap_plots(scatter_plots_elec, ncol = 2)
# ggsave("final/imgs/rq2_moran_scatter_elec.png", elec_scatter_grid, width = 10, height = 4, dpi = 300)

# ============================================================
# 7. SAVE RESULTS TABLE
# ============================================================

# write_csv(all_results, "output/tables/rq2_global_morans_i.csv")
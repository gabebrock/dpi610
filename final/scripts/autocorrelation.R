# ============================================================
# RQ4: Spatial Autocorrelation Between Demography & Voting
# Global Moran's I, Local Moran's I (LISA), Bivariate Moran's I
# Geographic Entrenchment During the 2026 Texas Primary


dir.create("output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output/tables",  recursive = TRUE, showWarnings = FALSE)

# Compute vote shares if not already present
precinct_results <- precinct_results %>%
  mutate(
    talarico_share = `JAMES TALARICO`  / totalVotesD,
    crockett_share = `JASMINE CROCKETT` / totalVotesD
  )

# --- Election layer ---
# First filter precincts with area > 0
pct_morans <- precinct_results[as.numeric(st_area(precinct_results)) > 0, ]

# Then build spatial weights matrix from filtered data
nb_elec  <- poly2nb(pct_morans, queen = TRUE)
lw_elec  <- nb2listw(nb_elec, style = "W", zero.policy = TRUE)

# Check for any islands (precincts with no neighbors)
cat("VTDS with no neighbors:", sum(card(nb_elec) == 0), "\n")

# Variable sets
demo_vars <- c("pct_white", "pct_black", "pct_hisp", "pct_other")
elec_vars <- c("talarico_share", "crockett_share")

demo_labels <- c("% White", "% Black", "% Hispanic", "% Other")
elec_labels <- c("Talarico Share", "Crockett Share")

# ============================================================
# 1. GLOBAL MORAN'S I  (same helper as RQ2, reproduced here
#    for self-contained script)
# ============================================================

global_morans <- function(data, var, listw, label = NULL) {
  x <- data[[var]]
  if (any(is.na(x))) x[is.na(x)] <- mean(x, na.rm = TRUE)

  test <- moran.test(x, listw, randomisation = TRUE, zero.policy = TRUE)

  tibble(
    variable    = if (!is.null(label)) label else var,
    moran_i     = round(test$estimate["Moran I statistic"], 4),
    expected_i  = round(test$estimate["Expectation"],       4),
    z_score     = round(test$statistic,                     4),
    p_value     = round(test$p.value,                       4),
    significant = ifelse(test$p.value < 0.05, "Yes", "No")
  )
}

global_demo <- map2_dfr(demo_vars, demo_labels, 
                        ~ global_morans(precinct_results, .x, lw, .y))

global_elec <- map2_dfr(elec_vars, elec_labels,
                        ~ global_morans(precinct_results, .x, lw_elec, .y))

global_all <- bind_rows(
  global_demo %>% mutate(layer = "Demography"),
  global_elec %>% mutate(layer = "Election")
) %>% select(layer, variable, moran_i, expected_i, z_score, p_value, significant)

cat("\n--- Global Moran's I ---\n")
print(global_all)

knitr::kable(global_all,
  col.names = c("Layer", "Variable", "Moran's I", "E[I]",
                "Z-Score", "p-value", "Sig."),
  caption   = "Table X: Global Moran's I — Demography & Electoral Variables")

write_csv(global_all, "output/tables/rq4_global_morans_i.csv")

# ============================================================
# 2. LOCAL MORAN'S I (LISA)
# ============================================================

run_local_morans <- function(data, var, listw) {
  x <- data[[var]]
  if (any(is.na(x))) x[is.na(x)] <- mean(x, na.rm = TRUE)

  lisa <- localmoran(x, listw, zero.policy = TRUE, alternative = "two.sided")

  # Classify quadrants based on standardized x and spatial lag
  x_std  <- scale(x)[, 1]
  lag_x  <- lag.listw(listw, x_std, zero.policy = TRUE)

  quadrant <- case_when(
    x_std >  0 & lag_x >  0 & lisa[, "Pr(z != E(Ii))"] < 0.05 ~ "High-High",
    x_std <  0 & lag_x <  0 & lisa[, "Pr(z != E(Ii))"] < 0.05 ~ "Low-Low",
    x_std >  0 & lag_x <  0 & lisa[, "Pr(z != E(Ii))"] < 0.05 ~ "High-Low",
    x_std <  0 & lag_x >  0 & lisa[, "Pr(z != E(Ii))"] < 0.05 ~ "Low-High",
    TRUE ~ "Not Significant"
  )

  data %>%
    mutate(
      lisa_i    = lisa[, "Ii"],
      lisa_p    = lisa[, "Pr(z != E(Ii))"],
      quadrant  = quadrant
    )
}

# Run LISA for all variables
all_vars   <- c(demo_vars, elec_vars)
all_labels <- c(demo_labels, elec_labels)

lisa_results <- map(all_vars, ~ run_local_morans(merged, .x, lw))
names(lisa_results) <- all_vars

# ---- LISA Cluster Maps ----

quad_colors <- c(
  "High-High"       = "#d73027",   # red
  "Low-Low"         = "#4575b4",   # blue
  "High-Low"        = "#fc8d59",   # orange
  "Low-High"        = "#91bfdb",   # light blue
  "Not Significant" = "grey85"
)

plot_lisa_map <- function(lisa_sf, label) {
  ggplot(lisa_sf) +
    geom_sf(aes(fill = quadrant), color = "white", linewidth = 0.1) +
    scale_fill_manual(values = quad_colors, name = "LISA Cluster",
                      drop = FALSE) +
    labs(title = paste("LISA Cluster Map:", label)) +
    theme_void(base_size = 10) +
    theme(
      plot.title      = element_text(face = "bold", size = 10, hjust = 0.5),
      legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      legend.text     = element_text(size = 7)
    )
}

lisa_maps <- map2(lisa_results, all_labels, plot_lisa_map)

# Save demographic LISA maps
demo_lisa_grid <- wrap_plots(lisa_maps[demo_vars], ncol = 2) +
  plot_annotation(title = "Figure X: LISA Cluster Maps — Demographic Variables")

ggsave("output/figures/rq4_lisa_maps_demo.png",
       demo_lisa_grid, width = 11, height = 9, dpi = 300)

# Save election LISA maps
elec_lisa_grid <- wrap_plots(lisa_maps[elec_vars], ncol = 2) +
  plot_annotation(title = "Figure X: LISA Cluster Maps — Electoral Variables")

ggsave("output/figures/rq4_lisa_maps_elec.png",
       elec_lisa_grid, width = 11, height = 5, dpi = 300)

# ---- LISA Summary Table ----

lisa_summary <- map2_dfr(lisa_results, all_labels, function(sf_obj, label) {
  sf_obj %>%
    st_drop_geometry() %>%
    count(quadrant) %>%
    mutate(variable = label, pct = round(n / sum(n) * 100, 1))
}) %>%
  select(variable, quadrant, n, pct) %>%
  arrange(variable, quadrant)

cat("\n--- LISA Cluster Summary ---\n")
print(lisa_summary)

knitr::kable(lisa_summary,
  col.names = c("Variable", "Cluster Type", "N Precincts", "%"),
  caption   = "Table X: LISA Cluster Counts by Variable")

write_csv(lisa_summary, "output/tables/rq4_lisa_summary.csv")

# ============================================================
# 3. BIVARIATE MORAN'S I
#    Tests whether the value of a DEMOGRAPHIC variable in unit i
#    is correlated with the ELECTORAL variable in neighboring units j
#    (and vice versa)
# ============================================================

# spdep does not have a single bivariate moran function;
# we implement it manually following Anselin (2019):
#   I_bv = (x_std)' W (y_std) / N

bivariate_morans <- function(data, x_var, y_var, listw,
                              x_label = NULL, y_label = NULL,
                              nsim = 999) {

  x <- data[[x_var]]
  y <- data[[y_var]]

  if (any(is.na(x))) x[is.na(x)] <- mean(x, na.rm = TRUE)
  if (any(is.na(y))) y[is.na(y)] <- mean(y, na.rm = TRUE)

  x_std <- scale(x)[, 1]
  y_std <- scale(y)[, 1]

  # Observed bivariate Moran's I
  lag_y  <- lag.listw(listw, y_std, zero.policy = TRUE)
  I_obs  <- sum(x_std * lag_y) / length(x_std)

  # Permutation test: shuffle x, keep W and y fixed
  set.seed(42)
  I_perm <- replicate(nsim, {
    x_perm  <- sample(x_std)
    sum(x_perm * lag_y) / length(x_perm)
  })

  p_val <- (sum(abs(I_perm) >= abs(I_obs)) + 1) / (nsim + 1)

  tibble(
    x_var       = if (!is.null(x_label)) x_label else x_var,
    y_var       = if (!is.null(y_label)) y_label else y_var,
    bv_moran_i  = round(I_obs,  4),
    p_value     = round(p_val,  4),
    significant = ifelse(p_val < 0.05, "Yes", "No")
  )
}

# Run all demo x elec combinations
bv_pairs <- expand_grid(
  demo = seq_along(demo_vars),
  elec = seq_along(elec_vars)
)

bv_results <- map2_dfr(bv_pairs$demo, bv_pairs$elec, function(d, e) {
  bivariate_morans(
    merged,
    x_var   = demo_vars[d],  y_var   = elec_vars[e],
    x_label = demo_labels[d], y_label = elec_labels[e],
    listw   = lw
  )
})

cat("\n--- Bivariate Moran's I ---\n")
print(bv_results)

knitr::kable(bv_results,
  col.names = c("Demographic Var (x)", "Electoral Var (y)",
                "Bivariate I", "p-value (permutation)", "Sig."),
  caption   = "Table X: Bivariate Moran's I — Demography × Electoral Variables")

write_csv(bv_results, "output/tables/rq4_bivariate_morans_i.csv")

# ---- Bivariate Moran Scatter Plots ----

plot_bv_scatter <- function(data, x_var, y_var, listw, x_label, y_label) {
  x     <- data[[x_var]]
  y     <- data[[y_var]]

  if (any(is.na(x))) x[is.na(x)] <- mean(x, na.rm = TRUE)
  if (any(is.na(y))) y[is.na(y)] <- mean(y, na.rm = TRUE)

  x_std <- scale(x)[, 1]
  y_std <- scale(y)[, 1]
  lag_y <- lag.listw(listw, y_std, zero.policy = TRUE)

  I_bv  <- round(sum(x_std * lag_y) / length(x_std), 3)

  tibble(x = x_std, lag_y = lag_y) %>%
    ggplot(aes(x = x, y = lag_y)) +
    geom_point(alpha = 0.55, size = 1.8, color = "#4575b4") +
    geom_smooth(method = "lm", se = FALSE,
                color = "#d73027", linewidth = 1) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
    labs(
      title    = paste0(x_label, " × W(", y_label, ")"),
      subtitle = paste0("Bivariate I = ", I_bv),
      x        = paste("Standardized", x_label),
      y        = paste("Spatial Lag of", y_label)
    ) +
    theme_minimal(base_size = 10) +
    theme(plot.title    = element_text(face = "bold", size = 10),
          plot.subtitle = element_text(color = "grey40", size = 8))
}

bv_scatter_plots <- map2(bv_pairs$demo, bv_pairs$elec, function(d, e) {
  plot_bv_scatter(
    merged,
    x_var   = demo_vars[d],  y_var   = elec_vars[e],
    x_label = demo_labels[d], y_label = elec_labels[e],
    listw   = lw
  )
})

bv_grid <- wrap_plots(bv_scatter_plots, ncol = 2) +
  plot_annotation(
    title    = "Figure X: Bivariate Moran Scatter Plots",
    subtitle = "x = standardized demographic variable; y = spatial lag of electoral variable"
  )

ggsave("output/figures/rq4_bivariate_scatter.png",
       bv_grid, width = 11, height = 14, dpi = 300)

# ============================================================
# 4. LOCAL BIVARIATE MORAN'S I (LISA for bivariate)
#    Classifies each precinct into HH/LL/HL/LH quadrants
#    for each demo x elec pair — useful for mapping
# ============================================================

local_bivariate_morans <- function(data, x_var, y_var, listw,
                                    x_label, y_label, nsim = 499) {
  x <- data[[x_var]]
  y <- data[[y_var]]

  if (any(is.na(x))) x[is.na(x)] <- mean(x, na.rm = TRUE)
  if (any(is.na(y))) y[is.na(y)] <- mean(y, na.rm = TRUE)

  x_std <- scale(x)[, 1]
  y_std <- scale(y)[, 1]
  lag_y <- lag.listw(listw, y_std, zero.policy = TRUE)

  # Local statistic: x_i * W(y_i)
  local_i <- x_std * lag_y

  # Permutation p-values (row-wise)
  set.seed(42)
  perm_mat <- replicate(nsim, {
    y_perm  <- sample(y_std)
    lag_perm <- lag.listw(listw, y_perm, zero.policy = TRUE)
    x_std * lag_perm
  })

  p_vals <- rowMeans(abs(perm_mat) >= abs(local_i))

  quadrant <- case_when(
    x_std >  0 & lag_y >  0 & p_vals < 0.05 ~ "High-High",
    x_std <  0 & lag_y <  0 & p_vals < 0.05 ~ "Low-Low",
    x_std >  0 & lag_y <  0 & p_vals < 0.05 ~ "High-Low",
    x_std <  0 & lag_y >  0 & p_vals < 0.05 ~ "Low-High",
    TRUE ~ "Not Significant"
  )

  data %>%
    mutate(
      bv_local_i = local_i,
      bv_p       = p_vals,
      bv_quad    = quadrant,
      x_label    = x_label,
      y_label    = y_label
    )
}

# Map local bivariate for the most theoretically important pairs:
#   % Black × Crockett, % White × Talarico, % Hispanic × both

key_pairs <- list(
  list(x = "pct_black", y = "crockett_share",
       xl = "% Black",  yl = "Crockett Share"),
  list(x = "pct_white", y = "talarico_share",
       xl = "% White",  yl = "Talarico Share"),
  list(x = "pct_hisp",  y = "talarico_share",
       xl = "% Hispanic", yl = "Talarico Share"),
  list(x = "pct_hisp",  y = "crockett_share",
       xl = "% Hispanic", yl = "Crockett Share")
)

bv_lisa_maps <- map(key_pairs, function(p) {
  sf_out <- local_bivariate_morans(
    merged, p$x, p$y, lw, p$xl, p$yl
  )

  ggplot(sf_out) +
    geom_sf(aes(fill = bv_quad), color = "white", linewidth = 0.1) +
    scale_fill_manual(values = quad_colors, name = "Cluster",
                      drop = FALSE) +
    labs(title    = paste0("Bivariate LISA: ", p$xl, " × ", p$yl),
         subtitle = "Significant clusters only (p < 0.05)") +
    theme_void(base_size = 10) +
    theme(
      plot.title      = element_text(face = "bold", size = 10, hjust = 0.5),
      plot.subtitle   = element_text(size = 8, hjust = 0.5, color = "grey40"),
      legend.position = "bottom",
      legend.key.size = unit(0.4, "cm"),
      legend.text     = element_text(size = 7)
    )
})

bv_lisa_grid <- wrap_plots(bv_lisa_maps, ncol = 2) +
  plot_annotation(
    title = "Figure X: Local Bivariate LISA Maps — Key Demographic × Electoral Pairs"
  )

ggsave("output/figures/rq4_bivariate_lisa_maps.png",
       bv_lisa_grid, width = 12, height = 10, dpi = 300)

cat("\nAll RQ4 outputs saved to output/figures/ and output/tables/\n")
sessionInfo()
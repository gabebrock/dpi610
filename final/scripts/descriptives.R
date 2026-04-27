# -------------------------------------------
# RQ1: Descriptive Statistics — Race in Harris County
# -------------------------------------------

library(sf)
library(tidyverse)
library(spdep)
library(ggplot2)
library(patchwork)      # combine ggplots
library(corrplot)       # correlation matrix visualization
library(knitr)          # summary tables
library(scales)         # axis formatting

# -------------------------------------------
# 1. SUMMARY STATISTICS TABLE
# -------------------------------------------

demo_vars <- c("pct_white", "pct_black", "pct_hisp", "pct_other")

summary_stats <- hc_4cat %>%
  st_drop_geometry() %>%
  select(all_of(demo_vars)) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(
    n      = n(),
    mean   = mean(value,   na.rm = TRUE),
    median = median(value, na.rm = TRUE),
    sd     = sd(value,     na.rm = TRUE),
    min    = min(value,    na.rm = TRUE),
    max    = max(value,    na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# print(summary_stats)

# Clean label map for display
var_labels <- c(
  pct_white    = "% White",
  pct_black    = "% Black",
  pct_hispanic = "% Hispanic",
  pct_asian    = "% Other"
)

# Debug: Check what variables are in summary_stats
# print("Variables in summary_stats:")
# print(unique(summary_stats$variable))

summary_stats <- summary_stats # %>%
  # mutate(variable = recode(variable, !!!var_labels))

# knitr::kable(summary_stats, caption = "Table 1: Descriptive Statistics — Demographics by Precinct")

# ============================================================
# 2. DENSITY PLOTS — Distribution of Each Demographic Variable
# ============================================================

plot_density <- function(var, label, fill_color) {
  ggplot(hc_4cat, aes(x = .data[[var]])) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 20, fill = fill_color, alpha = 0.5, color = "white") +
    geom_density(color = fill_color, linewidth = 1) +
    scale_x_continuous(labels = label_percent()) +
    labs(title = label, x = NULL, y = "Density") +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 11))
}

p_white <- plot_density("pct_white", "% White",    "#2166ac")
p_black <- plot_density("pct_black", "% Black",    "#d6604d")
p_hisp  <- plot_density("pct_hisp",  "% Hispanic", "#4dac26")
p_other <- plot_density("pct_other", "% Other",    "#b8860b")

density_grid <- (p_white | p_black) / (p_hisp | p_other)

# density_grid + plot_annotation(
#  title    = "Figure X: Distribution of Demographic Variables Across Harris County Precincts",
#  subtitle = "Each panel shows histogram + kernel density estimate"
# )

# ggsave("final/imgs/rq1_density_plots.png", density_grid, width = 10, height = 6, dpi = 300)

# ============================================================
# 3. CORRELATION MATRIX — Demographic Variables
# ============================================================

demo_df <- hc_4cat %>%
  st_drop_geometry() %>%
  select(all_of(demo_vars)) # %>%
  # rename(!!!var_labels)   # apply clean labels

cor_matrix <- cor(demo_df, use = "complete.obs")

# Base R corrplot
png("final/imgs/rq1_correlation_matrix.png",
    width = 7, height = 6, units = "in", res = 300)

dev.off()
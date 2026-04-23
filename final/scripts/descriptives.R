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

summary_stats <- summary_stats %>%
  mutate(variable = recode(variable, !!!var_labels))

knitr::kable(summary_stats,
             caption = "Table 1: Descriptive Statistics — Demographics by Precinct")

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

p_mhi <- ggplot(harris, aes(x = mhi)) +
  geom_histogram(aes(y = after_stat(density)),
                 bins = 20, fill = "#7b2d8b", alpha = 0.5, color = "white") +
  geom_density(color = "#7b2d8b", linewidth = 1) +
  scale_x_continuous(labels = label_dollar(scale = 1e-3, suffix = "k")) +
  labs(title = "Median Household Income", x = NULL, y = "Density") +
  theme_minimal(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 11))

density_grid <- (p_white | p_black) / (p_hispanic | p_other)

density_grid + plot_annotation(
  title    = "Figure X: Distribution of Demographic Variables Across Harris County Precincts",
  subtitle = "Each panel shows histogram + kernel density estimate"
)

ggsave("output/figures/rq1_density_plots.png",
       density_grid, width = 10, height = 6, dpi = 300)

# ============================================================
# 3. CORRELATION MATRIX — Demographic Variables
# ============================================================

demo_df <- hc_4cat %>%
  st_drop_geometry() %>%
  select(all_of(demo_vars)) # %>%
  rename(!!!var_labels)   # apply clean labels

cor_matrix <- cor(demo_df, use = "complete.obs")

# Base R corrplot
png("output/figures/rq1_correlation_matrix.png",
    width = 7, height = 6, units = "in", res = 300)

corrplot(cor_matrix,
         method     = "color",
         type       = "upper",
         addCoef.col = "black",
         tl.col     = "black",
         tl.srt     = 45,
         number.cex = 0.8,
         col        = COL2("RdBu", 200),
         title      = "Figure X: Correlation Matrix — Demographic Variables",
         mar        = c(0, 0, 2, 0))

dev.off()

# ggplot version (alternative, easier to customize)
cor_long <- as.data.frame(cor_matrix) %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation")

ggplot(cor_long, aes(x = var1, y = var2, fill = correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(correlation, 2)), size = 3.5) +
  scale_fill_gradient2(low = "#d6604d", mid = "white", high = "#2166ac",
                       midpoint = 0, limits = c(-1, 1)) +
  labs(title = "Correlation Matrix: Demographic Variables",
       x = NULL, y = NULL, fill = "r") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/figures/rq1_correlation_ggplot.png",
       width = 6, height = 5, dpi = 300)

# ============================================================
# 4. BOX PLOTS — Variation by County Region (1–8)
# ============================================================

region_long <- hc_4cat %>%
  st_drop_geometry() %>%
  select(region, all_of(demo_vars)) %>%
  pivot_longer(-region, names_to = "variable", values_to = "value") %>%
  mutate(variable = recode(variable, !!!var_labels),
         region   = factor(region))

# Separate MHI from race/ethnicity (different scale)
race_vars <- c("% White", "% Black", "% Hispanic", "% Asian")

p_box_race <- region_long %>%
  filter(variable %in% race_vars) %>%
  ggplot(aes(x = region, y = value, fill = variable)) +
  geom_boxplot(outlier.size = 1.2, alpha = 0.8) +
  facet_wrap(~variable, nrow = 2) +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c(
    "% White"    = "#2166ac",
    "% Black"    = "#d6604d",
    "% Hispanic" = "#4dac26",
    "% Asian"    = "#b8860b"
  )) +
  labs(title    = "Figure X: Racial Composition by County Region",
       x        = "Region",
       y        = "Population Share",
       fill     = NULL) +
  theme_minimal() +
  theme(legend.position = "none")

p_box_mhi <- harris %>%
  st_drop_geometry() %>%
  mutate(region = factor(region)) %>%
  ggplot(aes(x = region, y = mhi, fill = region)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1.2) +
  scale_y_continuous(labels = label_dollar(scale = 1e-3, suffix = "k")) +
  scale_fill_viridis_d(option = "plasma") +
  labs(title = "Figure X: Median Household Income by County Region",
       x     = "Region",
       y     = "Median HH Income",
       fill  = "Region") +
  theme_minimal()

ggsave("output/figures/rq1_boxplot_race.png",
       p_box_race, width = 9, height = 6, dpi = 300)

ggsave("output/figures/rq1_boxplot_mhi.png",
       p_box_mhi, width = 7, height = 5, dpi = 300)

# ============================================================
# 5. QUICK BIVARIATE SCATTER — Race vs. Income
#    (motivates the correlation matrix finding visually)
# ============================================================

p_scatter <- harris %>%
  st_drop_geometry() %>%
  ggplot(aes(x = pct_white, y = mhi)) +
  geom_point(aes(color = pct_black), size = 2.5, alpha = 0.75) +
  geom_smooth(method = "lm", se = TRUE, color = "grey30", linewidth = 0.8) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar(scale = 1e-3, suffix = "k")) +
  scale_color_gradient(low = "#f7f7f7", high = "#d6604d",
                       labels = label_percent(), name = "% Black") +
  labs(title    = "Figure X: % White vs. Median Household Income",
       subtitle = "Point color = % Black",
       x        = "% White",
       y        = "Median HH Income") +
  theme_minimal()

ggsave("output/figures/rq1_scatter_white_mhi.png",
       p_scatter, width = 7, height = 5, dpi = 300)

# ============================================================
# 6. PRINT SESSION INFO (reproducibility)
# ============================================================
sessionInfo()
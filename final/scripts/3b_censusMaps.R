
# ---- Population density map (per sq km) ----
p0 <- hc_4cat %>%
  mutate(pop_density = pop / (as.numeric(st_area(.)) / 1e6),
         vap_density = vap / (as.numeric(st_area(.)) / 1e6)) %>%
  filter(pop > 50) %>%
  ggplot() +
  geom_sf(aes(fill = pop_density)) +
  scale_fill_viridis_c(option = "turbo", 
                       direction = 1, trans = "log") +
  labs(fill = "Population Density (Log)") + 
  theme_urbn_map(scale = "continuous", base_size = 8.5,
                 base_family = "sans", base_line_size = 0.5, base_rect_size = 0.5) +
  theme(legend.position = "right")
ggsave("imgs/hc_pop_dens.png", width = 6, height = 4, dpi = 300)

# ---- Race share maps (proportions) ----
# Percent White (population)
p1 <- hc_4cat %>%
  filter(pop > 50) %>%
  ggplot() +
  scale_fill_viridis_c(option = "turbo", direction = 1) +
  geom_sf(aes(fill = pct_white)) +
  labs(fill = "Population % White") + 
  theme(legend.position = "bottom") +
  theme_urbn_map(scale = "continuous", base_size = 8.5,
                 base_family = "sans", base_line_size = 0.5, base_rect_size = 0.5)

# Percent Black (population)
p2 <- hc_4cat %>%
  filter(pop > 50) %>%
  ggplot() +
  scale_fill_viridis_c(option = "turbo", direction = 1) +
  geom_sf(aes(fill = pct_black)) +
  labs(fill = "Population % Black") + 
  theme(legend.position = "bottom") +
  theme_urbn_map(scale = "continuous", base_size = 8.5,
                 base_family = "sans", base_line_size = 0.5, base_rect_size = 0.5)

# Percent Hispanic (population)
p3 <- hc_4cat %>%
  filter(pop > 50) %>%
  ggplot() +
  scale_fill_viridis_c(option = "turbo", direction = 1) +
  geom_sf(aes(fill = pct_hisp)) +
  labs(fill = "Population %\nHispanic") + 
  theme(legend.position = "bottom") +
  theme_urbn_map(scale = "continuous", base_size = 8.5,
                 base_family = "sans", base_line_size = 0.5, base_rect_size = 0.5)
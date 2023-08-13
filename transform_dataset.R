library(tidyverse)

dataset <- readRDS("dataset_sardinia.RDA")

dataset <- dataset %>% 
  rename(
    urbanization_level = urban_rural_label,
    metropolitan_area = metro,
    coastal_area = coastal_label,
    mountain_area = mountain_region_label,
    border_area = border_region_label,
    remoteness_level = remote_region_label,
    island_area = island_region
  )

dataset_long <- dataset %>% 
  mutate(
    nuts_level_1 = paste(nuts_level_1, "(NUTS 1)"),
    nuts_level_2 = paste(nuts_level_2, "(NUTS 2)"),
    nuts_level_3 = paste(nuts_level_3, "(NUTS 3)"),
  ) %>% 
  pivot_longer(
    cols = 3:6,
    names_to = "nuts_level",
    values_to = "nuts_name"
  ) %>% 
  group_by(date, nuts_name) %>% 
  summarize(
    across(2:27, mean),
    across(28:31, sum),
    across(33:35, mean),
    across(36:57, sum),
    across(65:66, mean),
    across(67:73, sum),
    across(97:109, sum)) %>% 
  ungroup()


dataset_nuts <- dataset %>% 
  select(3:5, 63:69) %>% 
  mutate(across(1:length(.), as.factor)) %>% 
  distinct(.keep_all = TRUE)

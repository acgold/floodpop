library(sf)
library(tidyverse)
library(extrafont)
library(foreach)
library(tictoc)
library(patchwork)


############### Set paths ##################
results_path = "your_results_path"
data_path = "your_path"

############### Functions ##################
# Compute precision, recall, and F1 score for each class
calculate_metrics <- function(class_label, df) {
  true_positive <- sum(df$val_res_or_not == class_label & df$res_or_not == class_label)
  false_positive <- sum(df$val_res_or_not != class_label & df$res_or_not == class_label)
  false_negative <- sum(df$val_res_or_not == class_label & df$res_or_not != class_label)
  
  precision <- ifelse(true_positive + false_positive > 0, true_positive / (true_positive + false_positive), 0)
  recall <- ifelse(true_positive + false_negative > 0, true_positive / (true_positive + false_negative), 0)
  f1_score <- ifelse(precision + recall > 0, 2 * (precision * recall) / (precision + recall), 0)
  
  tibble(Class = class_label, Precision = precision, Recall = recall, F1_Score = f1_score)
}

#################### Building classification validation ##################
cities <- c("meck", "miami", "sacramento")

city_results <- foreach(i = cities, .combine = "bind_rows") %do% {
  cat("Working on: ", i,"\n")
  
  b_validation <- sf::st_read(file.path(results_path, "validation/sj_cities.gdb"), layer = paste0(i,"_validation"))
  
  b_buildings <- sf::st_read(file.path(results_path, "validation/buildings_w_lu.gdb"), layer=  paste0(i,"_buildings_w_lu")) %>%
    as_tibble()
  
  b_validation_df <- b_validation %>% 
    as_tibble() %>% 
    mutate(val_res_or_not = replace_na(val_res_or_not, -1),
           res_or_not = replace_na(res_or_not, -1)) %>% 
    mutate(val_res_or_not = ifelse(res_or_not == -1, -1, val_res_or_not),
           res_or_not = ifelse(val_res_or_not == -1, -1, res_or_not))
  
  # parcel count
  parcel_count <- b_validation_df %>% 
    group_by(val_res_or_not) %>% 
    summarize(parcel_count = n()) %>% 
    rename(res_or_not = val_res_or_not)
  
  # building count
  building_count <- b_buildings %>% 
    group_by(res_or_not) %>% 
    summarize(building_count = n(),
              building_area = sum(SHAPE_Area)/1000000)
  
  # parcel and building count + area
  val_count_area <- parcel_count %>% 
    left_join(building_count) %>% 
    arrange(-res_or_not)
  
  
  # Get all unique classes from both columns
  unique_classes <- union(unique(b_validation_df$val_res_or_not), unique(b_validation_df$res_or_not))
  
  # Compute metrics for each unique class
  metrics <- map_dfr(unique_classes, calculate_metrics, df = b_validation_df) %>% 
    arrange(-Class) %>% 
    left_join(val_count_area, by = c("Class" = "res_or_not")) %>% 
    mutate(location = i, .before = Class)
  
  cat(i,"\n")
  print(metrics)
  
}

more_city_results <- city_results %>% 
  group_by(location) %>% 
  mutate(percent_parcels = parcel_count / sum(parcel_count) * 100,
         percent_buildings = building_count / sum(building_count) * 100,
         percent_buildings_area = building_area / sum(building_area) * 100)

# Table 2
more_city_results %>% 
  ungroup() %>% 
  transmute(Location = ifelse(location == "meck", "Mecklenburg County, NC", ifelse(location == "miami", "Miami-Dade County, FL", "Sacramento, CA")), 
                              Classification = ifelse(Class == 1, "Residential", ifelse(Class == 0, "Non-residential", "None")), 
                                                      `Parcels with a building` = parcel_count, 
                                                      Buildings = building_count, 
                                                      `Building area in km^2` = building_area) %>% 
  mutate_if(.predicate = is.numeric, .funs = scales::comma)

# Table 3
more_city_results %>% 
  filter(Class != -1) %>% 
  ungroup() %>% 
  group_by(location) %>% 
  mutate(Avg_F1_Score = mean(F1_Score)) %>% 
  ungroup() %>% 
  transmute(Location = ifelse(location == "meck", "Mecklenburg County, NC", ifelse(location == "miami", "Miami-Dade County, FL", "Sacramento, CA")), 
            Classification = ifelse(Class == 1, "Residential", ifelse(Class == 0, "Non-residential", "None")), 
            Precision = round(Precision, 2),
            Recall = round(Recall, 2),
            F1_Score = round(F1_Score, 2),
            Avg_F1_Score = round(Avg_F1_Score, 2)) 

####################### Fig 4 - Building amount validation ####################
# Miami
m_blocks <- sf::st_read(file.path(results_path, "validation/validation_blocks.gdb"), layer = "miami_blocks")

m_buildings <- sf::st_read(file.path(results_path, "validation/buildings_w_lu.gdb"), layer=  "miami_buildings_w_lu") %>%
  as_tibble()

counts_by_block_miami <- m_buildings %>%
  group_by(GEOID20) %>% 
  filter(all(landuse %in% c(10,11,13))) %>% 
  filter(res_or_not == 1) %>% 
  group_by(GEOID20) %>% 
  summarize(n = n(), area =sum(SHAPE_Area)) %>% 
  left_join(m_blocks %>%
              select(geoid20, p1_001n, h1_001n, h1_002n, tot_hu_either_sfha), by = c("GEOID20" = "geoid20")) 

m_eq <- lm(n ~ h1_001n, data = counts_by_block_miami)
summary(m_eq)

max_limit <- max(max(counts_by_block_miami$h1_001n, na.rm = TRUE) + 1, 
                 max(counts_by_block_miami$n, na.rm = TRUE) + 1)

m_building_count <- scales::comma(counts_by_block_miami %>% pull(n) %>% sum())
m_unit_count <- scales::comma(counts_by_block_miami %>% pull(h1_001n) %>% sum())
m_blocks_count <- scales::comma(counts_by_block_miami %>% nrow())

m_buildings %>% filter(res_or_not == 1) %>% group_by(GEOID20) %>% count()


miami_val_sf <- counts_by_block_miami %>%
  ggplot() +
  geom_hex(aes(x = h1_001n, y = n), bins = 50) +
  geom_abline(aes(slope = 1, intercept = 0), lwd = 0.15, alpha = 0.5) +
  theme_bw() +
  coord_equal() +
  scale_fill_viridis_c(name="Count")+
  xlab("2020 Census Block\nTotal Housing Units")+
  ylab("# of residential buildings")+
  xlim(-1, max_limit) +
  ylim(-1, max_limit)+
  theme(
    text = element_text(family="Arial", size = 10),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(size = 8, vjust = 1),  
    legend.text = element_text(size = 8, angle = 45, hjust = 1),  
    plot.title = element_text(family = "Arial", size = 10, face = "bold", hjust = 0.5),
    panel.grid = element_blank(),
    legend.key.height = unit(0.25, "cm"),
    legend.position = "bottom",       
    legend.justification = "top",   
    legend.box.just = "center" ,
    plot.margin = margin(5,5,5,5)
  )+
  annotate("text", x = -Inf, y = max_limit * 1, label = paste0("'  '~R^2 == ", summary(m_eq)$r.squared %>% round(2)), size = 2, hjust = 0, parse = TRUE)+
  annotate("text", x = -Inf, y = max_limit * 0.9, label = paste0("   ", m_blocks_count, " blocks"), size = 2, hjust = 0)+
  annotate("text", x = -Inf, y = max_limit * 0.8, label = paste0("   ", m_unit_count," units"), size = 2, hjust = 0)+
  annotate("text", x = -Inf, y = max_limit * 0.7, label = paste0("   ", m_building_count," buildings"), size = 2, hjust = 0)+
  ggtitle("Miami-Dade County, FL")


# Meck
meck_blocks <- sf::st_read(file.path(results_path, "validation/validation_blocks.gdb"), layer = "meck_blocks")

meck_buildings <- sf::st_read(file.path(results_path, "validation/buildings_w_lu.gdb"), layer=  "meck_buildings_w_lu") %>%
  as_tibble()

counts_by_block_meck <- meck_buildings %>%
  group_by(GEOID20) %>% 
  filter(all(landuse %in% c("Single-Family"))) %>% 
  filter(res_or_not == 1) %>% 
  group_by(GEOID20) %>% 
  summarize(n = n(), area =sum(SHAPE_Area)) %>% 
  left_join(meck_blocks %>%
              select(geoid20, p1_001n, h1_001n, h1_002n, tot_hu_either_sfha), by = c("GEOID20" = "geoid20")) 

meck_eq <- lm(n ~ h1_001n, data = counts_by_block_meck)
summary(meck_eq)

max_limit <- max(max(counts_by_block_meck$h1_001n, na.rm = TRUE) + 1, 
                 max(counts_by_block_meck$n, na.rm = TRUE) + 1)

meck_building_count <- scales::comma(counts_by_block_meck %>% pull(n) %>% sum())
meck_unit_count <- scales::comma(counts_by_block_meck %>% pull(h1_001n) %>% sum())
meck_blocks_count <- scales::comma(counts_by_block_meck %>% nrow())

# 
meck_buildings %>% filter(res_or_not == 1) %>% group_by(GEOID20) %>% count()

meck_val_sf <- counts_by_block_meck %>%
  ggplot() +
  geom_hex(aes(x = h1_001n, y = n), bins = 50) +
  geom_abline(aes(slope = 1, intercept = 0), lwd = 0.15, alpha = 0.5) +
  theme_bw() +
  coord_equal() +
  scale_fill_viridis_c(name="Count")+
  xlab("2020 Census Block\nTotal Housing Units")+
  ylab("# of residential buildings")+
  xlim(-1, max_limit) +
  ylim(-1, max_limit)+
  theme(
    text = element_text(family="Arial", size = 10),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(size = 8, vjust = 1),  #
    legend.text = element_text(size = 8, angle = 45, hjust = 1),  
    plot.title = element_text(family = "Arial", size = 10, face = "bold", hjust = 0.5),
    panel.grid = element_blank(),
    legend.key.height = unit(0.25, "cm"),
    legend.position = "bottom",       
    legend.justification = "top",    
    legend.box.just = "center",
    plot.margin = margin(5,5,5,5)
  )+
  annotate("text", x = -Inf, y = max_limit * 1, label = paste0("'  '~R^2 == ", summary(meck_eq)$r.squared %>% round(2)), size = 2, hjust = 0, parse = TRUE)+
  annotate("text", x = -Inf, y = max_limit * 0.9, label = paste0("   ", meck_blocks_count, " blocks"), size = 2, hjust = 0)+
  annotate("text", x = -Inf, y = max_limit * 0.8, label = paste0("   ", meck_unit_count," units"), size = 2, hjust = 0)+
  annotate("text", x = -Inf, y = max_limit * 0.7, label = paste0("   ", meck_building_count," buildings"), size = 2, hjust = 0)+
  annotate("text", x = 310, y = 290, label = "1:1 line", angle = 45, color = "black", alpha = 0.5, size = 2)+
  ggtitle("Mecklenburg County, NC")

# Sacramento
sac_blocks <- sf::st_read(file.path(results_path, "validation/validation_blocks.gdb"), layer = "sacramento_blocks")

sac_buildings <- sf::st_read(file.path(results_path, "validation/buildings_w_lu.gdb"), layer=  "sacramento_buildings_w_lu") %>%
  as_tibble()

counts_by_block_sac <- sac_buildings %>%
  group_by(GEOID20) %>% 
  filter(all(landuse %in% c("Residential") & sec_landuse %in% c("Single Family"))) %>% 
  filter(res_or_not == 1) %>% 
  group_by(GEOID20) %>% 
  summarize(n = n(), area =sum(SHAPE_Area)) %>% 
  left_join(sac_blocks %>%
              select(geoid20, p1_001n, h1_001n, h1_002n, tot_hu_either_sfha), by = c("GEOID20" = "geoid20")) 

sac_eq <- lm(n ~ h1_001n, data = counts_by_block_sac)
summary(sac_eq)

max_limit <- max(max(counts_by_block_sac$h1_001n, na.rm = TRUE) + 1, 
                 max(counts_by_block_sac$n, na.rm = TRUE) + 1)

sac_building_count <- scales::comma(counts_by_block_sac %>% pull(n) %>% sum())
sac_unit_count <- scales::comma(counts_by_block_sac %>% pull(h1_001n) %>% sum())
sac_blocks_count <- scales::comma(counts_by_block_sac %>% nrow())

sac_buildings %>% filter(res_or_not == 1) %>% group_by(GEOID20) %>% count()

sac_val_sf <- counts_by_block_sac %>%
  ggplot() +
  geom_hex(aes(x = h1_001n, y = n), bins = 50) +
  geom_abline(aes(slope = 1, intercept = 0), lwd = 0.15, alpha = 0.5) +
  theme_bw() +
  coord_equal() +
  scale_fill_viridis_c(name="Count")+
  xlab("2020 Census Block\nTotal Housing Units")+
  ylab("# of residential buildings")+
  xlim(-1, max_limit) +
  ylim(-1, max_limit)+
  theme(
    text = element_text(family="Arial", size = 10),
    legend.direction = "horizontal",
    legend.box = "horizontal",
    legend.title = element_text(size = 8, vjust = 1),  
    legend.text = element_text(size = 8, angle = 45, hjust = 1),   
    plot.title = element_text(family = "Arial", size = 10, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid = element_blank(),
    legend.key.height = unit(0.25, "cm"),
    legend.position = "bottom",       
    legend.justification = "top",    
    legend.box.just = "center",
    plot.margin = margin(5,5,5,5)
  )+
  annotate("text", x = -Inf, y = max_limit * 1, label = paste0("'  '~R^2 == ", summary(sac_eq)$r.squared %>% round(2)), size = 2, hjust = 0, parse = TRUE)+
  annotate("text", x = -Inf, y = max_limit * 0.9, label = paste0("   ", sac_blocks_count, " blocks"), size = 2, hjust = 0)+
  annotate("text", x = -Inf, y = max_limit * 0.8, label = paste0("   ", sac_unit_count," units"), size = 2, hjust = 0)+
  annotate("text", x = -Inf, y = max_limit * 0.7, label = paste0("   ", sac_building_count," buildings"), size = 2, hjust = 0)+
  ggtitle("Sacramento County, CA")


sf_validation_plot <-  meck_val_sf + miami_val_sf +sac_val_sf + plot_annotation(tag_levels = "a", tag_suffix = ")") + plot_layout(ncol=3, axes = "collect", guides = "auto")
ggsave(file.path(results_path, "figures/validation_buildings.png"), sf_validation_plot, width = 180, height = 90, units = "mm")


############## Figure S1 #################
# Doubles
counts_by_block_dub_sac <- sac_buildings %>%
  group_by(GEOID20) %>% 
  filter(all(landuse %in% c("Residential") & sec_landuse %in% c("Two Family"))) %>% 
  filter(res_or_not == 1) %>% 
  group_by(GEOID20) %>% 
  summarize(n = n(), area =sum(SHAPE_Area)) %>% 
  left_join(sac_blocks %>%
              select(geoid20, h1_001n, h1_002n, tot_hu_either_sfha), by = c("GEOID20" = "geoid20")) %>% 
  mutate(Location = "Sacramento\nCounty, CA")

counts_by_block_dub_miami <- m_buildings %>%
  group_by(GEOID20) %>% 
  filter(all(landuse %in% c(20))) %>% 
  filter(res_or_not == 1) %>%
  group_by(GEOID20) %>% 
  summarize(n = n()) %>% 
  left_join(m_blocks %>%
              select(geoid20, h1_001n, h1_002n, tot_hu_either_sfha), by = c("GEOID20" = "geoid20")) %>% 
  filter(h1_001n < 100) %>% 
  mutate(Location = "Miami-Dade\nCounty, FL")

eq <- lm(n ~ h1_001n, data = counts_by_block_dub_miami)
summary(eq)

eq <- lm(n ~ h1_001n, data = counts_by_block_dub_sac)
summary(eq)


sac_val_dub_sf <- counts_by_block_dub_sac %>%
  bind_rows(counts_by_block_dub_miami) %>% 
  ggplot() +
  geom_point(aes(x = h1_001n, y = n, color = Location), alpha = 1, pch = 1) +
  geom_abline(aes(slope = 0.5, intercept = 0)) +
  theme_bw() +
  coord_equal() +
  facet_grid(. ~ Location)+
  scale_fill_viridis_c(name="Count")+
  xlab("2020 Census Block\nTotal Housing Units")+
  ylab("# of residential buildings")+
  theme(
    plot.title = element_text(family = "Arial", size = 10, face = "bold", hjust = 0.5),
    panel.grid.minor = element_blank(),
    legend.position = "none",       
    legend.justification = "top",    
    legend.box.just = "right",
    plot.margin = margin(5,5,5,5)
  )+
  ggtitle("Duplexes")

ggsave(file.path(results_path, "figures/supp_validation_duplexes.eps"),device = cairo_ps, sac_val_dub_sf, width = 180, height = 70, units = "mm")

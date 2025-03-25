library(scales)
library(classInt)
library(patchwork)
library(sf)
library(tidyverse)
library(terra)
library(data.table)
library(tidycensus)
library(arrow)
library(extrafont)
library(foreach)
library(extrafont)

############### Set paths ##################
results_path = "your_results_path"
data_path = "your_path"

############## Read data ###################
cart_counties <- sf::st_read(file.path(results_path, "census/cartographic/cartographic.gdb"), layer = "carto_counties")
cart_states <- sf::st_read(file.path(results_path, "census/cartographic/cartographic.gdb"), layer = "carto_states")
cart_tracts <- sf::st_read(file.path(results_path, "census/cartographic/cartographic.gdb"), layer = "carto_tracts")

# Define coordinates and close the polygon
coords <- matrix(c(
  1573604.47, 435926.76,  # upper_left
  1593240.81, 435897.12,  # upper_right
  1593314.91, 423403.96,  # lower_right
  1573678.57, 423389.14,  # lower_left
  1573604.47, 435926.76   # upper_left (closed)
), ncol = 2, byrow = TRUE)

# Create an sf object with the defined CRS "ESRI:102039"
polygon_sf <- st_sfc(st_polygon(list(coords)), crs = "ESRI:102039")

# counties <- sf::st_read(file.path(results_path, "census/counties.gdb"), layer = "counties_floodpop") %>% 
#   sf::st_transform(crs = "ESRI:102039")


################## Figure 1 maps ###############
## County
num_bins <- 8
bin_method <- "quantile" # Or "quantile"

breaks <- classIntervals(cart_counties$pop_either_sfha, n = num_bins, style = bin_method)$brks

binned_cart_counties_pop <- cart_counties %>%
  mutate(z_bins = cut(pop_either_sfha, breaks = breaks, include.lowest = TRUE))

formatted_labels_pop <- scales::comma(breaks)

pop_plot <- binned_cart_counties_pop %>% 
  ggplot()+
  geom_sf(aes(fill = z_bins), color = "lightgrey",lwd = 0.01)+
  scale_fill_manual(values = RColorBrewer::brewer.pal(num_bins, "PuBuGn"),
                    labels = paste(head(formatted_labels_pop, -1), 
                                   formatted_labels_pop[-1], 
                                   sep = " - "),
                    name="Estimated population\nin the SFHA or\nestimated SFHA")+
  theme_void()+
  theme(text = element_text(family = "Arial", size = 8),
        legend.key.size = unit(0.25, "cm"))+
  geom_sf(data = cart_states %>% filter(STUSPS == "FL"), fill = NA, color = "black", lwd = 0.5)

ggsave(file.path(results_path, "figures/pop_plot.eps"), pop_plot,width = 180, height = 90, units = "mm", device = cairo_ps)

# Tracts
num_bins <- 8
bin_method <- "quantile" 

florida_tracts <- cart_tracts %>% 
  filter(state == "FL")

breaks <- classIntervals(florida_tracts$pop_either_sfha, n = num_bins, style = bin_method)$brks

binned_cart_tracts_pop <- florida_tracts %>%
  mutate(z_bins = cut(pop_either_sfha, breaks = breaks, include.lowest = TRUE))

formatted_labels_pop <- scales::comma(breaks)

fl_pop_plot <- binned_cart_tracts_pop %>% 
  ggplot()+
  geom_sf(aes(fill = z_bins), color = "lightgrey",lwd = 0.01)+
  scale_fill_manual(values = RColorBrewer::brewer.pal(num_bins, "PuBuGn"),
                    labels = paste(head(formatted_labels_pop, -1), 
                                   formatted_labels_pop[-1], 
                                   sep = " - "),
                    name="Estimated population\nin the SFHA or\nestimated SFHA")+
  theme_void()+
  theme(text = element_text(family = "Arial", size = 8),
        legend.key.size = unit(0.25, "cm"),
        legend.position.inside = c(0.35, 0.5))+
  geom_sf(data = cart_states %>% filter(STUSPS == "FL"), fill = NA, color = "black", lwd = 0.25)+
  geom_sf(data = polygon_sf, fill = NA, color = "black", lwd = 0.75)

ggsave(file.path(results_path, "figures/fl_pop_plot.eps"), fl_pop_plot,width = 90, height = 90, units = "mm", device = cairo_ps)

# Legend-only plot b/c the block-level map is from arcgis
new_breaks <- c(0, 2, 8, 18, 34, 58, 105, 192, 363)

num_bins_new <- length(new_breaks) - 1  
colors_new <- RColorBrewer::brewer.pal(num_bins_new, "PuBuGn")

formatted_labels_new <- scales::comma(new_breaks[-length(new_breaks)])

dummy_data <- data.frame(
  x = 1, y = 1,
  fill = factor(c(1:8), levels = 1:8)
)

fl_pop_legend_plot <- ggplot(dummy_data) +
  geom_tile(aes(x = x, y = y, fill = fill), width = 1, height = 1) +
  scale_fill_manual(values = colors_new,
                    breaks = 1:8,  
                    labels = paste(formatted_labels_new, 
                                   scales::comma(new_breaks[-1]), 
                                   sep = " - "),
                    name = "Estimated population\nin the SFHA or\nestimated SFHA") +
  theme_void() +
  theme(text = element_text(family = "Arial", size = 8),
        legend.key.size = unit(0.25, "cm"),
        legend.position = "right")

ggsave(file.path(results_path, "figures/miami_pop_plot_legend.eps"), fl_pop_legend_plot,width = 90, height = 90, units = "mm", device = cairo_ps)

############### Tables ##############
state_summary_table <- readr::read_csv(file.path(results_path, "census/summaries/state_summary.csv"))
state_summary <- state_summary_table  %>% 
  summarize_if(.predicate = is.numeric, .funs = sum)

# Table 1
factorial_table <- tibble(
  Category = c("Total", "SFHA", "Best-available SFHA", "Either SFHA"),
  Population = c(
    comma(state_summary$P1_001N),
    paste0(comma(state_summary$pop_sfha), " (", comma(state_summary$pop_sfha_low), " - ", comma(state_summary$pop_sfha_high), ")"),
    paste0(comma(state_summary$pop_best_sfha), " (", comma(state_summary$pop_best_sfha_low), " - ", comma(state_summary$pop_best_sfha_high), ")"),
    paste0(comma(state_summary$pop_either_sfha), " (", comma(state_summary$pop_either_sfha_low), " - ", comma(state_summary$pop_either_sfha_high), ")")
  ),
  `Total Housing Units` = c(
    comma(state_summary$H1_001N),
    comma(state_summary$tot_hu_sfha),
    comma(state_summary$tot_hu_best_sfha),
    comma(state_summary$tot_hu_either_sfha)
  ),
  `Occupied Housing Units` = c(
    comma(state_summary$H1_002N),
    paste0(comma(state_summary$occ_hu_sfha), " (", comma(state_summary$occ_hu_sfha_low), " - ", comma(state_summary$occ_hu_sfha_high), ")"),
    paste0(comma(state_summary$occ_hu_best_sfha), " (", comma(state_summary$occ_hu_best_sfha_low), " - ", comma(state_summary$occ_hu_best_sfha_high), ")"),
    paste0(comma(state_summary$occ_hu_either_sfha), " (", comma(state_summary$occ_hu_either_sfha_low), " - ", comma(state_summary$occ_hu_either_sfha_high), ")")
  )
)

# Print the table
factorial_table %>% 
  clipr::write_clip()

# Supplementary Table 2
state_pop_table <- state_summary_table %>% 
  select(NAME, P1_001N:occ_hu_either_sfha_high) %>% 
  arrange(-pop_either_sfha) %>% 
  transmute(State = NAME,
            `Total population` = comma(P1_001N),
            `Population in SFHA` = paste0(comma(pop_sfha), " (", comma(pop_sfha_low), " - ", comma(pop_sfha_high), ")"),
            `Population in best-available SFHA` = paste0(comma(pop_best_sfha), " (", comma(pop_best_sfha_low), " - ", comma(pop_best_sfha_high), ")"),
            `Population in either SFHA` = paste0(comma(pop_either_sfha), " (", comma(pop_either_sfha_low), " - ", comma(pop_either_sfha_high), ")")
  )

state_pop_table %>% 
  clipr::write_clip()

# Supplementary Table 3
state_tot_hu_table <- state_summary_table %>% 
  select(NAME, P1_001N:occ_hu_either_sfha_high) %>% 
  arrange(-tot_hu_either_sfha) %>% 
  transmute(State = NAME,
            `Total housing units` = comma(H1_001N),
            `Total housing units in SFHA` = paste0(comma(tot_hu_sfha)),
            `Total housing units in best-available SFHA` = paste0(comma(tot_hu_best_sfha)),
            `Total housing units in either SFHA` = paste0(comma(tot_hu_either_sfha))
  )

state_tot_hu_table %>% 
  clipr::write_clip()

# Supplementary Table 4
state_occ_hu_table <- state_summary_table %>% 
  select(NAME, P1_001N:occ_hu_either_sfha_high) %>% 
  arrange(-pop_either_sfha) %>% 
  transmute(State = NAME,
            `Occupied housing units` = comma(H1_002N),
            `Occupied housing units in SFHA` = paste0(comma(occ_hu_sfha), " (", comma(occ_hu_sfha_low), " - ", comma(occ_hu_sfha_high), ")"),
            `Occupied housing units in best-available SFHA` = paste0(comma(occ_hu_best_sfha), " (", comma(occ_hu_best_sfha_low), " - ", comma(occ_hu_best_sfha_high), ")"),
            `Occupied housing units in either SFHA` = paste0(comma(occ_hu_either_sfha), " (", comma(occ_hu_either_sfha_low), " - ", comma(occ_hu_either_sfha_high), ")")
  )

state_occ_hu_table %>% 
  clipr::write_clip()


# Supplementary Table 1 
state_res_or_not <- readr::read_csv(file.path(results_path, "census/summaries/state_res_or_not_summary.csv"))

source_table <- state_res_or_not %>% 
  group_by(res_or_not) %>% 
  summarize(perc_count_us = sum(count_usa)/sum(count_total)*100,
            perc_count_osm = sum(count_osm)/sum(count_total)*100,
            perc_count_nsi = sum(count_nsi)/sum(count_total)*100,
            count_total = scales::comma(sum(count_total)),
            perc_area_us = sum(area_usa)/sum(area_total)*100,
            perc_area_osm = sum(area_osm)/sum(area_total)*100,
            perc_area_nsi = sum(area_nsi)/sum(area_total)*100,
            area_total = scales::comma(sum(area_total)*1e-6)) %>% 
  arrange(-res_or_not)

source_table %>% clipr::write_clip()


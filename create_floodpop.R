library(sf)
library(tidyverse)
library(terra)
library(data.table)
library(tidycensus)
library(arrow)
library(extrafont)
library(foreach)
library(tictoc)

############### Set paths ##################
results_path = "your_results_path"
data_path = "your_path"

###############  Functions ############### 
source_summary <- function(state_name){
  cat("Working on ", state_name, "\n")
  cat("- Reading in structures \n")
  structures <- arrow::read_parquet(paste0(results_path,"/building_footprint_dfs/", state_name, "_buildings_fp.parquet")) %>% 
    as.data.table()
  
  source_classified <- structures[, .(res_or_not = res_or_not, area = SHAPE_Area, source = fifelse(
    !is.na(subtype),
    fifelse(subtype == "residential", fifelse(
      class %in% c("garage", "garages", "parking"),
      "osm",
      fifelse(
        !is.na(MAX_OCC_CLS) | !is.na(MAX_st_damcat),
        "osm",
        fifelse(update_time < "2021-01-01", "osm", "none")
      )
    ), "osm"),
    fifelse(
      !is.na(MAX_OCC_CLS) & MAX_OCC_CLS != "Unclassified",
      fifelse(
        MAX_OCC_CLS == "Residential" & MAX_PRIM_OCC != "Temporary Lodging",
        "usa",
        "usa"
      ),
      fifelse(!is.na(MAX_st_damcat), fifelse(MAX_st_damcat == "RES", "nsi", "nsi"), "none")
    )
  ))]
  
  # Summarize the counts by the source column
  source_summary_df <- source_classified[, .(count = .N, area = sum(area)), by = .(source, res_or_not)] %>% as_tibble() %>% 
    pivot_wider(names_from = c(source), values_from = c(count, area))
  
  source_summary_df[is.na(source_summary_df)] <- 0
  
  source_summary_df <- source_summary_df %>% 
    rowwise() %>% 
    mutate(count_total = sum(count_usa, count_none, count_nsi, count_osm),
           area_total = sum(area_usa, area_none, area_nsi, area_osm)) %>% 
    ungroup() %>% 
    mutate(state = state_name, .before=res_or_not)
  
  source_summary_df
}

# Add fema model or not to buiildings_to_blocks
buildings_to_blocks <- function(state_name, variables = c("P1_001N", "H1_001N", "H1_002N")){
  cat("Working on ", state_name, "\n")
  cat("- Reading in structures \n")
  structures <- arrow::read_parquet(paste0(results_path,"/building_footprint_dfs/", state_name, "_buildings_fp.parquet")) %>% 
    as.data.table()
  
  structures[is.na(within_sfha), within_sfha := 0]
  structures[is.na(within_est_sfha), within_est_sfha := 0]
  structures[is.na(fema_model), fema_model := 0]
  
  structures[, best_sfha := fifelse(fema_model == 1, as.integer(within_sfha == 1), as.integer(within_est_sfha == 1))]
  structures[, either_sfha := as.integer(within_sfha == 1 | within_est_sfha == 1)]
  structures[, tract := stringr::str_sub(GEOID20, 1, -5)]
  
  cat("- Calculating floodplain overlaps \n")
  
  # sum up the building area (square meters) by census block and in/out of each type of sfha (or either)
  sfha_area <- structures[res_or_not == 1, .(Area = sum(SHAPE_Area)), by = .(GEOID20, within_sfha)]
  reshaped_sfha_area <- dcast(sfha_area, GEOID20 ~ within_sfha, value.var = "Area", fill = 0)
  setnames(reshaped_sfha_area, c("0","1"), c("out_sfha_area", "within_sfha_area"))
  reshaped_sfha_area[, ratio_area_sfha := within_sfha_area / (within_sfha_area + out_sfha_area)]
  
  est_sfha_area <- structures[res_or_not  == 1, .(Area = sum(SHAPE_Area)), by = .(GEOID20, within_est_sfha)]
  reshaped_est_sfha_area <- dcast(est_sfha_area, GEOID20 ~ within_est_sfha, value.var = "Area", fill = 0)
  setnames(reshaped_est_sfha_area, c("0", "1"), c("out_est_sfha_area", "within_est_sfha_area"))
  reshaped_est_sfha_area[, ratio_area_est_sfha := within_est_sfha_area / (within_est_sfha_area + out_est_sfha_area)]
  
  best_sfha_area <- structures[res_or_not  == 1, .(Area = sum(SHAPE_Area)), by = .(GEOID20, best_sfha)]
  reshaped_best_sfha_area <- dcast(best_sfha_area, GEOID20 ~ best_sfha, value.var = "Area", fill = 0)
  setnames(reshaped_best_sfha_area, c("0", "1"), c("out_best_sfha_area", "within_best_sfha_area"))
  reshaped_best_sfha_area[, ratio_area_best_sfha := within_best_sfha_area / (within_best_sfha_area + out_best_sfha_area)]
  
  either_area <- structures[res_or_not == 1, .(Area = sum(SHAPE_Area)), by = .(GEOID20, either_sfha)]
  reshaped_either_area <- dcast(either_area, GEOID20 ~ either_sfha, value.var = "Area", fill = 0)
  setnames(reshaped_either_area, c("0", "1"), c("out_either_sfha_area", "within_either_sfha_area"))
  reshaped_either_area[, ratio_area_either_sfha := within_either_sfha_area / (within_either_sfha_area + out_either_sfha_area)]
  
  combined_cb_area <- merge(reshaped_sfha_area, reshaped_est_sfha_area, by = "GEOID20", all = TRUE)
  combined_cb_area <- merge(combined_cb_area, reshaped_best_sfha_area, by = "GEOID20", all = TRUE)
  combined_cb_area <- merge(combined_cb_area, reshaped_either_area, by = "GEOID20", all = TRUE)
  
  cat("- Downloading census data \n")
  # Download the block tabular data
  census_data <- get_decennial(
    geography = "block",
    variables = variables,
    state = state_name,
    year = 2020, 
    output = "wide"  
  )

  cat("- Final calculation \n")
  state_cb <- sf::st_read(dsn = file.path(data_path, "census/blocks/blocks_by_state.gdb"), layer = paste0(state_name, "_blocks")) %>% 
    left_join(combined_cb_area) %>% 
    left_join(census_data %>% select(-c(NAME)), by = c("GEOID20" = "GEOID")) %>% 
    left_join(census_block_pop_and_ci %>% 
                select(GEOID20, CI_LOW, CI_HIGH)) %>% 
    left_join(census_block_hu_and_ci %>% 
                select(GEOID20, CI_LOW_HU, CI_HIGH_HU)) %>% 
    mutate(CI_HIGH_HU = ifelse(CI_HIGH_HU > H1_001N, H1_001N, CI_HIGH_HU)) %>%
    mutate(across(out_sfha_area:ratio_area_either_sfha , ~ replace_na(.x, 0))) %>% 
    mutate(pop_sfha = round(P1_001N * ratio_area_sfha),
           pop_sfha_low = round(CI_LOW * ratio_area_sfha),
           pop_sfha_high = round(CI_HIGH * ratio_area_sfha),
           pop_best_sfha = round(P1_001N * ratio_area_best_sfha),
           pop_best_sfha_low = round(CI_LOW * ratio_area_best_sfha),
           pop_best_sfha_high = round(CI_HIGH * ratio_area_best_sfha),
           pop_either_sfha = round(P1_001N * ratio_area_either_sfha),
           pop_either_sfha_low = round(CI_LOW * ratio_area_either_sfha),
           pop_either_sfha_high = round(CI_HIGH * ratio_area_either_sfha),
           
           tot_hu_sfha = round(H1_001N * ratio_area_sfha),
           tot_hu_best_sfha = round(H1_001N * ratio_area_best_sfha),
           tot_hu_either_sfha = round(H1_001N * ratio_area_either_sfha),

           occ_hu_sfha = round(H1_002N * ratio_area_sfha),
           occ_hu_sfha_low = round(CI_LOW_HU * ratio_area_sfha),
           occ_hu_sfha_high = round(CI_LOW_HU * ratio_area_sfha),
           occ_hu_best_sfha = round(H1_002N * ratio_area_best_sfha),
           occ_hu_best_sfha_low = round(CI_LOW_HU * ratio_area_best_sfha),
           occ_hu_best_sfha_high = round(CI_HIGH_HU * ratio_area_best_sfha),
           occ_hu_either_sfha = round(H1_002N * ratio_area_either_sfha),
           occ_hu_either_sfha_low = round(CI_LOW_HU * ratio_area_either_sfha),
           occ_hu_either_sfha_high = round(CI_HIGH_HU * ratio_area_either_sfha)) %>% 
    mutate(state = state_name, .before = 1) %>%  
    select(-SHAPE, SHAPE) 
  
  state_cb[is.na(state_cb)] <- 0
  
  state_cb %>% 
    sf::st_write(dsn = file.path(results_path, "census/blocks.gdb"), layer = "blocks_floodpop", append = T)
  
  
  if(file.exists(file.path(results_path, "census/summaries/block_summary.csv"))){
    state_cb %>% 
      as_tibble() %>% 
      select(-SHAPE) %>% 
      readr::write_csv(file.path(results_path, "census/summaries/block_summary.csv"), append=T)
    
  }else{
    state_cb %>% 
      as_tibble() %>% 
      select(-SHAPE) %>% 
      readr::write_csv(file.path(results_path, "census/summaries/block_summary.csv"), append=F)
    
  }
  
  return(state_cb)
}

blocks_to_tracts <- function(state_cb, state_name){
  tracts <- state_cb %>%
    as_tibble() %>%
    select(-SHAPE) %>%
    mutate(tract = stringr::str_sub(GEOID20, 1, -5)) %>%
    group_by(tract) %>%
    summarize(
      pop_sfha = sum(pop_sfha, na.rm = T),
      pop_sfha_low = sum(pop_sfha_low, na.rm = T),
      pop_sfha_high = sum(pop_sfha_high, na.rm = T),
      pop_best_sfha = sum(pop_best_sfha, na.rm = T),
      pop_best_sfha_low = sum(pop_best_sfha_low, na.rm = T),
      pop_best_sfha_high = sum(pop_best_sfha_high, na.rm = T),
      pop_either_sfha = sum(pop_either_sfha, na.rm = T),
      pop_either_sfha_low = sum(pop_either_sfha_low, na.rm = T),
      pop_either_sfha_high = sum(pop_either_sfha_high, na.rm = T),
      
      tot_hu_sfha = sum(tot_hu_sfha, na.rm=T),
      tot_hu_best_sfha = sum(tot_hu_best_sfha, na.rm=T),
      tot_hu_either_sfha = sum(tot_hu_either_sfha, na.rm=T),
      
      occ_hu_sfha = sum(occ_hu_sfha, na.rm=T),
      occ_hu_sfha_low = sum(occ_hu_sfha_low, na.rm=T),
      occ_hu_sfha_high = sum(occ_hu_sfha_high, na.rm=T),
      occ_hu_best_sfha = sum(occ_hu_best_sfha, na.rm=T),
      occ_hu_best_sfha_low = sum(occ_hu_best_sfha_low, na.rm=T),
      occ_hu_best_sfha_high = sum(occ_hu_best_sfha_high, na.rm=T),
      occ_hu_either_sfha = sum(occ_hu_either_sfha, na.rm=T),
      occ_hu_either_sfha_low = sum(occ_hu_either_sfha_low, na.rm=T),
      occ_hu_either_sfha_high = sum(occ_hu_either_sfha_high, na.rm=T)) %>% 
    mutate(state = state_name, .before = 1) 
  
  tract_data <- get_decennial(
    geography = "tract",
    geometry = F,
    variables = c("P1_001N", "H1_001N", "H1_002N"),
    state = state_name,
    year = 2020,  
    output = "wide" 
  ) 
  
  state_tracts <- sf::st_read(dsn = file.path(data_path, "census/tlgdb_2020_a_us_substategeo.gdb"), layer = "Census_Tract") %>% 
    filter(GEOID %in% tract_data$GEOID) %>% 
    left_join(tract_data) %>% 
    left_join(tracts, by = c("GEOID" = "tract")) %>% 
    select(-SHAPE, SHAPE)
  
  state_tracts %>% 
    sf::st_write(dsn = file.path(results_path, "census/tracts.gdb"), layer = "tracts_floodpop", append = T)
  
  
  if(file.exists(file.path(results_path, "census/summaries/tracts_summary.csv"))){
    state_tracts %>% 
      as_tibble() %>% 
      select(-SHAPE) %>% 
      readr::write_csv(file.path(results_path, "census/summaries/tracts_summary.csv"), append=T)
    
  }else{
    state_tracts %>% 
      as_tibble() %>% 
      select(-SHAPE) %>% 
      readr::write_csv(file.path(results_path, "census/summaries/tracts_summary.csv"), append=F)
  }
  
}


blocks_to_counties <- function(state_cb, state_name){
  counties <- state_cb %>%
    as_tibble() %>%
    select(-SHAPE) %>%
    mutate(county = stringr::str_sub(GEOID20, 1, 5)) %>%
    group_by(county) %>%
    summarize(
      pop_sfha = sum(pop_sfha, na.rm = T),
      pop_sfha_low = sum(pop_sfha_low, na.rm = T),
      pop_sfha_high = sum(pop_sfha_high, na.rm = T),
      pop_best_sfha = sum(pop_best_sfha, na.rm = T),
      pop_best_sfha_low = sum(pop_best_sfha_low, na.rm = T),
      pop_best_sfha_high = sum(pop_best_sfha_high, na.rm = T),
      pop_either_sfha = sum(pop_either_sfha, na.rm = T),
      pop_either_sfha_low = sum(pop_either_sfha_low, na.rm = T),
      pop_either_sfha_high = sum(pop_either_sfha_high, na.rm = T),
      
      tot_hu_sfha = sum(tot_hu_sfha, na.rm=T),
      tot_hu_best_sfha = sum(tot_hu_best_sfha, na.rm=T),
      tot_hu_either_sfha = sum(tot_hu_either_sfha, na.rm=T),
      
      occ_hu_sfha = sum(occ_hu_sfha, na.rm=T),
      occ_hu_sfha_low = sum(occ_hu_sfha_low, na.rm=T),
      occ_hu_sfha_high = sum(occ_hu_sfha_high, na.rm=T),
      occ_hu_best_sfha = sum(occ_hu_best_sfha, na.rm=T),
      occ_hu_best_sfha_low = sum(occ_hu_best_sfha_low, na.rm=T),
      occ_hu_best_sfha_high = sum(occ_hu_best_sfha_high, na.rm=T),
      occ_hu_either_sfha = sum(occ_hu_either_sfha, na.rm=T),
      occ_hu_either_sfha_low = sum(occ_hu_either_sfha_low, na.rm=T),
      occ_hu_either_sfha_high = sum(occ_hu_either_sfha_high, na.rm=T)) %>% 
    mutate(state = state_name, .before = 1) 
  
  county_data <- get_decennial(
    geography = "county",
    geometry = F,
    variables = c("P1_001N", "H1_001N", "H1_002N"),
    state = state_name,
    year = 2020,  
    output = "wide" 
  )
  
  county_geo <- sf::st_read(dsn = file.path(data_path, "census/tlgdb_2020_a_us_substategeo.gdb"), layer = "County") %>% 
    filter(GEOID %in% county_data$GEOID) %>% 
    left_join(county_data) %>% 
    left_join(counties, by = c("GEOID" = "county")) %>% 
    select(-SHAPE, SHAPE)
  
  county_geo %>% 
    sf::st_write(dsn = file.path(results_path, "census/counties.gdb"), layer = "counties_floodpop", append = T)
  
  
  if(file.exists(file.path(results_path, "census/summaries/county_summary.csv"))){
    county_geo %>% 
      as_tibble() %>% 
      select(-SHAPE) %>% 
      readr::write_csv(file.path(results_path, "census/summaries/county_summary.csv"), append=T)
    
  }else{
    county_geo %>% 
      as_tibble() %>% 
      select(-SHAPE) %>% 
      readr::write_csv(file.path(results_path, "census/summaries/county_summary.csv"), append=F)
  }
  
}


blocks_to_states <- function(state_cb, state_name){
  states <- state_cb %>%
    as_tibble() %>%
    select(-SHAPE) %>%
    mutate(state_fips = stringr::str_sub(GEOID20, 1, 2)) %>%
    group_by(state_fips) %>%
    summarize(
      pop_sfha = sum(pop_sfha, na.rm = T),
      pop_sfha_low = sum(pop_sfha_low, na.rm = T),
      pop_sfha_high = sum(pop_sfha_high, na.rm = T),
      pop_best_sfha = sum(pop_best_sfha, na.rm = T),
      pop_best_sfha_low = sum(pop_best_sfha_low, na.rm = T),
      pop_best_sfha_high = sum(pop_best_sfha_high, na.rm = T),
      pop_either_sfha = sum(pop_either_sfha, na.rm = T),
      pop_either_sfha_low = sum(pop_either_sfha_low, na.rm = T),
      pop_either_sfha_high = sum(pop_either_sfha_high, na.rm = T),
      
      tot_hu_sfha = sum(tot_hu_sfha, na.rm=T),
      tot_hu_best_sfha = sum(tot_hu_best_sfha, na.rm=T),
      tot_hu_either_sfha = sum(tot_hu_either_sfha, na.rm=T),
      
      occ_hu_sfha = sum(occ_hu_sfha, na.rm=T),
      occ_hu_sfha_low = sum(occ_hu_sfha_low, na.rm=T),
      occ_hu_sfha_high = sum(occ_hu_sfha_high, na.rm=T),
      occ_hu_best_sfha = sum(occ_hu_best_sfha, na.rm=T),
      occ_hu_best_sfha_low = sum(occ_hu_best_sfha_low, na.rm=T),
      occ_hu_best_sfha_high = sum(occ_hu_best_sfha_high, na.rm=T),
      occ_hu_either_sfha = sum(occ_hu_either_sfha, na.rm=T),
      occ_hu_either_sfha_low = sum(occ_hu_either_sfha_low, na.rm=T),
      occ_hu_either_sfha_high = sum(occ_hu_either_sfha_high, na.rm=T)) 
  
  state_data <- get_decennial(
    geography = "state",
    geometry = F,
    variables = c("P1_001N", "H1_001N", "H1_002N"),
    state = state_name,
    year = 2020,  
    output = "wide" 
  )
  
  state_geo <- sf::st_read(dsn = file.path(data_path, "census/tlgdb_2020_a_us_substategeo.gdb"), layer = "State") %>% 
    filter(GEOID %in% state_data$GEOID) %>% 
    left_join(state_data) %>% 
    left_join(states, by = c("GEOID" = "state_fips")) %>% 
    select(-SHAPE, SHAPE)
  
  
  state_geo %>% 
    sf::st_write(dsn = file.path(results_path, "census/states.gdb"), layer = "states_floodpop", append = T)
  
  
  if(file.exists(file.path(results_path, "census/summaries/state_summary.csv"))){
    state_geo %>% 
      as_tibble() %>% 
      select(-SHAPE) %>% 
      readr::write_csv(file.path(results_path, "census/summaries/state_summary.csv"), append=T)
    
  }else{
    state_geo %>% 
      as_tibble() %>% 
      select(-SHAPE) %>% 
      readr::write_csv(file.path(results_path, "census/summaries/state_summary.csv"), append=F)
  }
  
}

###############  Read census data  ############### 
census_block_pop_and_ci <- arrow::read_parquet(file.path(data_path, "census/PPMF/parquet/final_results.parquet")) %>% 
  as.data.table()

census_block_hu_and_ci <- arrow::read_parquet(file.path(data_path, "census/PPMF/parquet/final_results_hu.parquet")) %>% 
  as.data.table()

############### Create FloodPop ################
state_names <-  sf::st_read(dsn = file.path(data_path, "census/tlgdb_2020_a_us_substategeo.gdb"), layer = "State") %>% 
  as_tibble() %>% 
  filter(!STUSPS %in% c("AK", "HI", "AS","GU","MP","PR","VI")) %>% 
  pull(STUSPS)

# OR
# state_names <- c(state.abb[!state.abb %in% c("AK", "HI", "AS","GU","MP","PR","VI")], "DC") %>% sort()

for(i in 1:length(state_names)){
  tictoc::tic()
  st <- state_names[i]
  state_cb <- buildings_to_blocks(st)
  
  blocks_to_tracts(state_cb, st)
  blocks_to_counties(state_cb, st)
  blocks_to_states(state_cb, st)
  tictoc::toc()
}

# Get summary of classification sources
source_summary_all <- foreach(i = state_names, .combine = "bind_rows") %do% {
  source_summary(i)
}

source_summary_all %>% 
  readr::write_csv(file.path(results_path, "census/summaries/state_res_or_not_summary.csv"), append=F)


source_summary_all %>% 
  group_by(res_or_not) %>% 
  summarize_if(is.numeric, .funs = sum) %>% 
  transmute(res_or_not,
            perc_count_usa = round(count_usa/count_total * 100,3),
            perc_count_osm = round(count_osm/count_total * 100,3),
            perc_count_nsi = round(count_nsi/count_total * 100,3),
            comma(count_total),
            perc_area_usa = round(area_usa/area_total * 100,3),
            perc_area_osm = round(area_osm/area_total * 100,3),
            perc_area_nsi = round(area_nsi/area_total * 100,3),
            comma(area_total)) %>% 
  arrange(-res_or_not) %>% 
  clipr::write_clip()


############### Read summary tables ################
state_summary_table <- readr::read_csv(file.path(results_path, "census/summaries/state_summary.csv"))
county_summary_table <- readr::read_csv(file.path(results_path, "census/summaries/county_summary.csv"))
tract_summary_table <- readr::read_csv(file.path(results_path, "census/summaries/tracts_summary.csv"))
block_summary_table <- readr::read_csv(file.path(results_path, "census/summaries/block_summary.csv"))

############ Create Cartographic summaries #####################
cart_tracts <- sf::st_read(file.path(data_path, "census/cartographic/cb_2020_us_all_500k.gdb"), layer = "cb_2020_us_tract_500k") %>% 
  left_join(tract_summary_table %>% 
              select(GEOID, state, P1_001N:occ_hu_either_sfha_high)) %>% 
  sf::st_transform(crs = "ESRI:102039")

cart_tracts %>% 
  sf::st_write(file.path(results_path, "census/cartographic/cartographic.gdb"), layer = "carto_tracts", append = F)


cart_counties <- sf::st_read(file.path(data_path, "census/cartographic/cb_2020_us_all_5m.gdb"), layer = "cb_2020_us_county_5m") %>% 
  right_join(county_summary_table %>% 
              select(GEOID, P1_001N:occ_hu_either_sfha_high)) %>% 
  sf::st_transform(crs = "ESRI:102039")

cart_counties %>% 
  sf::st_write(file.path(results_path, "census/cartographic/cartographic.gdb"), layer = "carto_counties", append = F)


cart_states <- sf::st_read("D:/wotus_wetlands_downstream/data/census/cartographic/cb_2020_us_all_5m.gdb", layer = "cb_2020_us_state_5m") %>% 
  right_join(state_summary_table %>% 
               select(GEOID, P1_001N:occ_hu_either_sfha_high)) %>% 
  sf::st_transform(crs = "ESRI:102039")

cart_states %>% 
  sf::st_write(file.path(results_path,"census/cartographic/cartographic.gdb"), layer = "carto_states", append = F)

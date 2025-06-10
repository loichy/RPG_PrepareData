#===============================================================================
# Description: Rscript to assemble all individual yearly and regional .rds fils
# clean data and create big single data frame 
#===============================================================================

#===============================================================================
# 1). Prepare environment ------
#===============================================================================

# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, here, sf, tmap, units, dplyr)

# List directories 
dir <- list()
dir$root <- here()
dir$data <- here(dir$root, "data")
dir$raw <- here(dir$data, "raw")
dir$derived <- here(dir$data, "derived")
dir$derived <- "C:/Users/loihenry/Dropbox/Recherche_Dauphine/DataArchive/RPG_data/Data/aggregated_communes"
dir$final <- here(dir$data, "final")
dir$script <- here(dir$root, "script")
dir$output <- here(dir$root, "output")

# Create non existing directories
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))

#===============================================================================
# 2) Combine data from regions and years into a single table ------
#===============================================================================

# List all RPG_Aggregated_<region_code>_<year>.rds files
rds_files <- list.files(path = dir$derived, 
                        pattern = "RPG_Aggregated*", 
                        full.names = TRUE)

# To correct mistakes: list of rds files in 2013, or 2014  or R94
# rds_files <- rds_files[grepl("2013|2014|R94", rds_files)]

# Read all the files and combine them into a single large table
all_data_list <- lapply(rds_files, readRDS) 

all_data_df <- bind_rows(all_data_list)

#Clean Data
all_data_clean <- all_data_df %>%
  filter(
    as.numeric(surf_agri_geo_unit_m2) != 0 # Check conditions
  ) %>% 
  filter(CODE_GROUP != "")

# Quick check 
glimpse(all_data_clean)
table(as.numeric(all_data_clean$year))
unique(all_data_clean$region_code)
table(all_data_clean$CODE_GROUP)

#===============================================================================
#3) Delete doublons associated with the spatial misaggregation  ------
#===============================================================================
#Clean Data 2 : problem of border towns 
# Les villes à la frontière des régions ont été capturé au moins deux fois, car des parcelles agricoles
# peuvent se retrouver à la frontière de deux régions. Il faut donc supprimer les observations 
# associées aux surfaces agricoles des cultures qui ne correspondent pas à l'assemblage des surfaces des
# parcelles issus du bon fichier régional.

# Créer une variable "id" pour identifier chaque ligne 
all_data_clean_final <- all_data_clean %>%
  ungroup() %>% 
  mutate(id = row_number())

# Identifier les doublons potentiels : mêmes year, insee et CODE_GROUP
tableau_doublons <- all_data_clean_final %>%
  group_by(year, insee, CODE_GROUP) %>%
  filter(n() > 1) %>%
  arrange(year, insee, CODE_GROUP, desc(surf_agri_geo_unit_m2)) %>%
  ungroup()

# Identifier les doublons à ne pas conserver :
# on garde la ligne avec la plus grande surface agricole pour la commune (permet d'identifier dans la quasi majorité les observations issus du bon fichier régional)
tableau_doublons_nonretenu <- tableau_doublons %>%
  group_by(year, insee, CODE_GROUP) %>%
  filter(surf_agri_geo_unit_m2 != max(surf_agri_geo_unit_m2)) %>% # La ligne avec la plus grande surface agricole de la commune entre tous les doublons
  ungroup()

# Supprimer les doublons non retenus du jeu de données principal
all_data_final <- all_data_clean_final %>%
  filter(!(id %in% tableau_doublons_nonretenu$id))

# Check that we indeed have the sum of the share of crop areas and parcels per commune and year which sums to one 
table_stat <- all_data_final %>% 
  group_by(year, insee) %>% 
  summarize(
    first(name),
    total_surface_perc = sum(as.numeric(surf_cult_perc), na.rm = TRUE),
    total_parcels_perc = sum(as.numeric(parcel_cult_perc), na.rm = TRUE),
    .groups = "drop"
  ) 

table_stat_large <- table_stat %>% 
  filter(
    as.numeric(total_surface_perc) > 1.05 | as.numeric(total_parcels_perc) > 1.05
  )

table_stat_small <- table_stat %>% 
  filter(
    as.numeric(total_surface_perc) < 0.9 | as.numeric(total_parcels_perc) < 0.9
  )

#===============================================================================
# 3) Reaggregate data at the culture group level for years after 2015 ------
#===============================================================================
result_df_aggreg <- all_data_final %>%
  group_by(year, insee, CODE_GROUP) %>% 
  summarise(
    name = first(name),
    data_type = first(data_type),
    region_code = first(region_code),
    surf_tot_geo_unit_m2 = first(surf_tot_geo_unit_m2),
    surf_agri_geo_unit_m2 = first(surf_agri_geo_unit_m2),
    surf_code_group_m2 = sum(surf_cult_m2),
    surf_code_group_perc = sum(surf_cult_perc),
    parcel_cult_code_group_n = sum(parcel_cult_n),
    N_Parcels = first(N_Parcels),
    parcel_cult_code_group_perc = sum(parcel_cult_perc),
    .groups = "drop"
  )
#===============================================================================
# 4) Merging of crop group codes and corresponding labels ------
#===============================================================================
cultures <- read_delim(here(dir$raw,"REF_CULTURES_GROUPES_CULTURES_2020.csv"), delim = ";")
cultures <- cultures %>%
  mutate(CODE_GROUPE_CULTURE = as.character(CODE_GROUPE_CULTURE))

cultures_clean <- cultures %>%
  select(-CODE_CULTURE, -LIBELLE_CULTURE) %>% 
  group_by(CODE_GROUPE_CULTURE) %>%
  summarise(
    CODE_GROUP = first(CODE_GROUPE_CULTURE),
    LIBELLE_GROUPE_CULTURE = first(LIBELLE_GROUPE_CULTURE),
    .groups = "drop"
  ) %>% 
  select(-CODE_GROUPE_CULTURE)

result_df_aggreg_final <- result_df_aggreg |>
  left_join(cultures_clean, by = c("CODE_GROUP"))

# We miss some culture codes: 10, 12, 13, 27

# Check that we indeed have the sum of the share of crop areas and parcels per commune and year which sums to one 
table_stat <- result_df_aggreg_final %>% 
  group_by(year, insee) %>% 
  summarize(
    first(name),
    total_surface_perc = sum(as.numeric(surf_code_group_perc), na.rm = TRUE),
    total_parcels_perc = sum(as.numeric(parcel_cult_code_group_perc), na.rm = TRUE),
    .groups = "drop"
  ) 

table_stat_large <- table_stat %>% 
  filter(
    as.numeric(total_surface_perc) > 1.05 | as.numeric(total_parcels_perc) > 1.05
  ) %>% 
  arrange(insee)

table_stat_small <- table_stat %>% 
  filter(
    as.numeric(total_surface_perc) < 0.9 | as.numeric(total_parcels_perc) < 0.9
  ) %>% 
  arrange(insee)

# When correcting mistakes: combine with old table
# Load old table
# old_table <- readRDS(here(dir$final, "RPG_Aggregated_ALL.rds")) %>% 
#   filter(region_code != "R94") # Remove Corsica
# result_df_aggreg_final <- old_table %>%
#   bind_rows(result_df_aggreg_final) %>% 
#   arrange(year, region_code, insee, CODE_GROUP)

# Create df for Brittany only
result_df_aggreg_final_brittany <- result_df_aggreg_final %>%
  filter(region_code == "R53") 

# Create wide df
result_df_aggreg_final_wide <- result_df_aggreg_final %>%
  mutate(
    CODE_GROUP = paste0("G", CODE_GROUP),  # Optional: Prefix to make column names clearer,
    parcel_cult_code_group_n = as.numeric(parcel_cult_code_group_n),
    parcel_cult_code_group_perc = as.numeric(parcel_cult_code_group_perc),
    surf_code_group_m2 = as.numeric(surf_code_group_m2),
    surf_code_group_perc = as.numeric(surf_code_group_perc)
  ) %>%
  pivot_wider(
    id_cols = c(year, insee, name, data_type, region_code, surf_tot_geo_unit_m2, surf_agri_geo_unit_m2, N_Parcels),
    names_from = CODE_GROUP,
    values_from = c(
      parcel_cult_code_group_n,
      parcel_cult_code_group_perc,
      surf_code_group_m2,
      surf_code_group_perc
    ),
    names_glue = "{.value}_{CODE_GROUP}",
    values_fill = 0  # Use NA for cultures not cultivated in that commune-year
  )

result_df_aggreg_final_wide_R53 <- result_df_aggreg_final_wide %>%
  filter(region_code == "R53")

#===============================================================================
# 5) Save the final tables ------
#===============================================================================

# Long full df
saveRDS(result_df_aggreg_final, file = file.path(dir$final, "RPG_Aggregated_ALL.rds"))

# Wide full df
saveRDS(result_df_aggreg_final_wide, file = file.path(dir$final, "RPG_Aggregated_ALL_wide.rds"))

# Long Brittany df
saveRDS(result_df_aggreg_final_brittany, file = file.path(dir$final, "RPG_Aggregated_Brittany.rds"))

# Wide Brittany df
saveRDS(result_df_aggreg_final_wide_R53, file = file.path(dir$final, "RPG_Aggregated_Brittany_wide.rds"))


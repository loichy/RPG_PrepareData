### Scrit qui prépare les données RPG

# Prepare evironment
# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, here, sf)

# List directories 
dir <- list()
dir$root <- here()
dir$data <- here(dir$root, "data")
dir$raw <- here(dir$data, "raw")
dir$derived <- here(dir$data, "derived")
dir$script <- here(dir$root, "script")
dir$output <- here(dir$root, "output")

# Create non existing directories
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))

# Load the data
load(here(dir$raw, "RPG_Corsica_2022.Rdata"))

#1. Fusion des données pour ajouter les labels de culture
library(dplyr)
rpg_corsica_map_no_geom <- rpg_corsica_map |> 
  select(-geometry)  # Supprime temporairement la colonne geometry
rpg_corsica_map_joined <- rpg_corsica_map_no_geom |>
  left_join(cultures, by = c("CODE_CULTU" = "CODE_CULTURE"))
rpg_corsica_map_final <- bind_cols(
  rpg_corsica_map |> select(geometry),  # Récupérer geometry
  rpg_corsica_map_joined  # Ajouter le dataframe joint
)

# Ouverture données 2012

rpg_corsica_2012 <- read_sf(here(dir$raw,"ILOTS_ANONYMES.shp"))
save(rpg_corsica_2012, file = here(dir$derived,"RPG_Corsica_2012.Rdata"))


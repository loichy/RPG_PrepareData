#===============================================================================
# Description: Aggregate RPG data at a French commune level
#===============================================================================

#===============================================================================
# 1). Prepare environment ------
#===============================================================================
# Prepare evironment
# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, here, sf, tmap)

# List directories 
dir <- list()
dir$root <- here()
# For remote uncomment following dir
# dir$root <- "/Users/lhenry/Data/RPG_PrepareData"
dir$data <- here(dir$root, "data")
dir$raw <- here(dir$data, "raw")
dir$sf <- here(dir$data, "shapefiles")
# dir$raw <- "C:/Users/loihenry/Dropbox/Recherche_Dauphine/DataArchive/RPG_data/Data/raw"
dir$derived <- here(dir$data, "derived")
# dir$derived <- "C:/Users/loihenry/Dropbox/Recherche_Dauphine/DataArchive/RPG_data/Data/aggregated_communes"
dir$script <- here(dir$root, "script")
dir$output <- here(dir$root, "output")

# Create non existing directories
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))

#===============================================================================
# 2) Load list of downloaded RPG files  ------
#===============================================================================
all_rpg_links <- readRDS(here(dir$raw, "rpg_files.rds"))


#===============================================================================
# 3) Aggregate all shp files by region and year at the commune level ------
#===============================================================================

# Function to find the ilot anonyme .shp file (after 2014 and before 2024)
find_pg_shp_file <- function(directory) {
  shp_files <- list.files(directory, pattern = "PARCELLES_GRAPHIQUES\\.shp$", recursive = TRUE, full.names = TRUE)
  if (length(shp_files) > 0) {
    return(shp_files[1])  # Return the first matching .shp file
  } else {
     return(NULL)
   }
}


# Function to find the ilot anonyme .shp file (before 2014)
find_ia_shp_file <- function(directory) {
  shp_files <- list.files(directory, pattern = "ILOTS_ANONYMES\\.shp$", recursive = TRUE, full.names = TRUE)
  if (length(shp_files) > 0) {
     return(shp_files[1])  # Return the first matching .shp file
   } else {
     return(NULL)
   }
}

# Function to find the parcel graphique .gpkg file (after 2024)
find_pg_gpkg_file <- function(directory) {
  gpkg_files <- list.files(directory, pattern = "RPG_Parcelles\\.gpkg$", recursive = TRUE, full.names = TRUE)
  if (length(gpkg_files) > 0) {
    return(gpkg_files[1])  # Return the first matching .gpkg file
  } else {
    return(NULL)
  }
}

# all_rpg_links <- all_rpg_links %>% 
#   filter(region_code == "R94") # To correct some mistakes

# process_data <- function(i) {
for (i in seq_len(nrow(all_rpg_links))) {
  # if (!require("pacman")) install.packages("pacman")
  # pacman::p_load(tidyverse, data.table, here, sf, tmap)
  # i <- 18
  # Save some useful names
  region_i <- all_rpg_links$region_name[i]
  region_code_i <- all_rpg_links$region_code[i]
  year_i <- all_rpg_links$year[i]
  # Create the destination file path
  destfile_i <- here(dir$raw, paste0("tempfile_", all_rpg_links$region_code[i], "_", all_rpg_links$year[i]))
  # Print the region code and year
  print(paste(all_rpg_links$region_code[i], " for year ", all_rpg_links$year[i]))
  # Check if the shapefile exists
  # Find the PARCELLES_GRAPHIQUES.shp file
  pg_shp_file_i <- find_pg_shp_file(destfile_i)
  # Find the ilot anonyme shp file (if any)
  ia_shp_file_i <- find_ia_shp_file(destfile_i)
  # Find the parcelles graphiques .gpkg file (if any)
  pg_gpkg_file_i <- find_pg_gpkg_file(destfile_i)
  # Shape file type: ilot anonyme (ia) or parcelle graphique (pg)
  rpg_type_i <- ifelse(
    !is.null(pg_shp_file_i), 
    "pg", 
    ifelse(
      !is.null(ia_shp_file_i), 
      "ia",
      ifelse(
        !is.null(pg_gpkg_file_i),
        "pg_gpkg",
        "noshp")
    )
  )
  
  # Prepare the shape file for spatial aggregation, depending on the type: ia or pg
  if (rpg_type_i != "noshp"){
    if (rpg_type_i == "pg"){
      # Print the shapefile type
      print(paste("Shapefile 'PARCELLES_GRAPHIQUES' exists:", pg_shp_file_i))
      
      # Read the shapefile
      rpg_map_i <- st_read(pg_shp_file_i, quiet = TRUE)
      
      # Correct invalid polygons if any
      if (!all(st_is_valid(rpg_map_i))) {
        message("Correct parcels with invalid geometry")
        rpg_map_i <- st_make_valid(rpg_map_i, quiet=T)
      }
      
      # Recompute some variable
      rpg_map_i <- rpg_map_i %>% 
        mutate(
          AREA_COMP = st_area(geometry), #To have it in m²
          YEAR = year_i,
          REGION = region_i,
          REGION_CODE = region_code_i,
          CODE_GROUP = gsub("^0", "", CODE_GROUP)
        )
      
      # Import shp file of communes boundaries:
      communes <- st_read(here(dir$sf, "communes-20220101.shp"), quiet = T)
      
      # Transpose CRS to have the same as RPG files CRS:
      communes_crs_i <- st_transform(communes, crs = st_crs(rpg_map_i))
      
      # Compute intersection
      rpg_map_intersection_i <- st_intersection(rpg_map_i, communes_crs_i)
      
      # Calculate areas
      rpg_map_intersection_i$intersect_area <- st_area(rpg_map_intersection_i)

      
      # Compute share of parcel area in the commune
      rpg_map_intersection_i <- rpg_map_intersection_i %>%
        mutate(percent_in_commune = as.numeric(intersect_area / AREA_COMP ),
               weight = ifelse(
                 # remove parcel of the commune if less than 10% of its area in commune, 
                 # keep all the parcel if more than 90% of its area in commune,
                 # otherwise weight the parcel in the commune aggregate by the share of its area in the commune
                 percent_in_commune < 0.1, 
                 0,
                 ifelse(
                   percent_in_commune >= 0.1 & percent_in_commune < 0.9,
                   percent_in_commune,
                   ifelse(
                     percent_in_commune >= 0.9,
                     1,
                     NA
                   )
                 )
               )
        )
      
      
      # Save this shape file only if corsica, to keep a detailed version of the aggregation process for one region
      if (all_rpg_links$region_code[i] == "R94"){
        # Save the shapefile
        st_write(rpg_map_intersection_i, here(dir$derived, paste0("RPG_Detailed_", region_code_i, "_", year_i, ".shp")), delete_dsn = TRUE)
        print(paste("Detailed shapefile saved for ", all_rpg_links$region_code[i], " for year ", all_rpg_links$year[i]))
      }

      # Compute useful variable at the geo_unit level
      rpg_map_intersection_i <- rpg_map_intersection_i %>%
        group_by(insee) %>% #First: construct a variable that for each parcel gives the surface of the agricultural area of the geo_unit in which it is located
        mutate(N_Parcels = sum(weight > 0), # Total number of parcels in the commune
               surf_agri_geo_unit_m2 = sum(AREA_COMP * weight, na.rm = T), # Surface of all agricultural parcels in the geo_unit
        )
      
      # For all region, then save the aggregate shape file
      rpg_result_i <- rpg_map_intersection_i %>% # Object that contain descriptive statistics
        group_by(insee, CODE_CULTU) %>% # Aggregate by CODE_CULTU for parcelles graphiques
        summarise(
          data_type = "pg",
          region_code = first(region_code_i),
          name = first(nom),
          year = first(YEAR),
          CODE_GROUP = first(CODE_GROUP),
          surf_tot_geo_unit_m2 = first(surf_ha)*10000,
          surf_agri_geo_unit_m2 = first(surf_agri_geo_unit_m2), # Surface of all agricultural parcels in the geo_unit
          surf_cult_m2 = sum(AREA_COMP * weight, na.rm = T),# Surface of parcels dedicated to each culture in each department
          surf_cult_perc = sum(AREA_COMP * weight, na.rm = T) / first(surf_agri_geo_unit_m2), # Percentage in the total agricultural area of the commune
          parcel_cult_n  = sum(weight>0), # Careful, forgotten na_rm = T
          N_Parcels = N_Parcels[1],
          parcel_cult_perc = parcel_cult_n / N_Parcels[1]) %>% 
        # Remove raws with 0 surface for the culture, to avoid having too many raws with 0 surface for each culture in the final dataset
        filter(as.numeric(surf_cult_m2) != 0)
      
      # Save it in prepared data folder
      saveRDS(st_drop_geometry(rpg_result_i), file = here(dir$derived, paste0("RPG_Aggregated_", region_code_i, "_", year_i,".rds")))
      
      # Return message that it is saved
      file_path <- here(dir$derived, paste0("RPG_Aggregated_", region_code_i, "_", year_i,".rds"))
      if (file.exists(file_path)) {
        message_y <- paste("File '", paste0("RPG_Aggregated_", region_code_i, "_", year_i), "' is correctly saved.", sep="")
        print(message_y)
      } else {
        message_n <- paste("File for region '", paste0(region_code_i, "' and for year '", year_i), "' cannot be correctly prepared.", sep="")
        print(message_n)
        
      }
      
      
    }
    if (rpg_type_i == "ia"){
      print(paste("Shapefile 'ILOTS_ANONYMES' exists:", ia_shp_file_i))
      # Read the shapefile
      rpg_map_i <- st_read(ia_shp_file_i, quiet = TRUE)
      
      # Correct invalid polygons, if any
      if (!all(st_is_valid(rpg_map_i))) {
        message("Correct parcels with invalid geometry")
        rpg_map_i <- st_make_valid(rpg_map_i, quiet = TRUE)
      }

      # Recompute some variable
      rpg_map_i <- rpg_map_i %>% 
        mutate(
          AREA_COMP = st_area(geometry), #To have it in m²
          YEAR = year_i,
          REGION = region_i,
          REGION_CODE = region_code_i,
          CODE_GROUP = gsub("^0", "", CODE_CULTU)
        )
      
      # Import shp file of communes boundaries:
      communes <- st_read(here(dir$sf, "communes-20220101.shp"), quiet = T)
      
      # Transpose CRS to have the same as RPG files CRS:
      communes_crs_i <- st_transform(communes, crs = st_crs(rpg_map_i))
      
      # Compute intersection
      rpg_map_intersection_i <- st_intersection(rpg_map_i, communes_crs_i)
      
      # Calculate areas
      rpg_map_intersection_i$intersect_area <- st_area(rpg_map_intersection_i)
      
      # Compute share of parcel area in the commune
      rpg_map_intersection_i <- rpg_map_intersection_i %>%
        mutate(percent_in_commune = as.numeric(intersect_area / AREA_COMP ),
               weight = ifelse(
                 # remove parcel of the commune if less than 10% of its area in commune, 
                 # keep all the parcel if more than 90% of its area in commune,
                 # otherwise weight the parcel in the commune aggregate by the share of its area in the commune
                 percent_in_commune < 0.1, 
                 0,
                 ifelse(
                   percent_in_commune >= 0.1 & percent_in_commune < 0.9,
                   percent_in_commune,
                   ifelse(
                     percent_in_commune >= 0.9,
                     1,
                     NA
                   )
                 )
               )
        )
      
      # Save this shape file only if corsica, to keep a detailed version of the aggregation process for one region
      if (all_rpg_links$region_code[i] == "R94"){
        # Save the shapefile
        st_write(rpg_map_intersection_i, here(dir$derived, paste0("RPG_Detailed_", region_code_i, "_", year_i, ".shp")), delete_dsn = TRUE)
        print(paste("Detailed shapefile saved for ", all_rpg_links$region_code[i], " for year ", all_rpg_links$year[i]))
      }
     
      # Compute useful variable at the geo_unit level
      rpg_map_intersection_i <- rpg_map_intersection_i %>%
        group_by(insee) %>% #First: construct a variable that for each parcel gives the surface of the agricultural area of the geo_unit in which it is located
        mutate(N_Parcels = sum(weight>0), # Total number of parcels in the commune
        surf_agri_geo_unit_m2 = sum(AREA_COMP * weight, na.rm = T), # Surface of all agricultural parcels in the geo_unit
        )
      
      # For all region, then save the aggregate shape file
      rpg_result_i <- rpg_map_intersection_i %>% # Object that contain descriptive statistics
        group_by(insee, CODE_GROUP) %>% # Aggregate by CODE_GROUP for ilots anonymes
        summarise(
          data_type = "pg",
          region_code = first(region_code_i),
          name = first(nom),
          year = first(YEAR),
          surf_tot_geo_unit_m2 = first(surf_ha)*10000,
          surf_agri_geo_unit_m2 = first(surf_agri_geo_unit_m2), # Surface of all agricultural parcels in the geo_unit
          surf_cult_m2 = sum(AREA_COMP * weight, na.rm = T),# Surface of parcels dedicated to each culture in each department
          surf_cult_perc = sum(AREA_COMP * weight, na.rm = T) / surf_agri_geo_unit_m2[1], # Percentage in the total agricultural area of the commune
          parcel_cult_n  = sum(weight>0),
          N_Parcels = N_Parcels[1],
          parcel_cult_perc = parcel_cult_n / N_Parcels[1]) %>% 
        # Remove raws with 0 surface for the culture, to avoid having too many raws with 0 surface for each culture in the final dataset
        filter(as.numeric(surf_cult_m2) != 0)
      
      # Save it in prepared data folder
      saveRDS(st_drop_geometry(rpg_result_i), file = here(dir$derived, paste0("RPG_Aggregated_", region_code_i, "_", year_i,".rds")))
              
      # Return message that it is saved
      file_path <- here(dir$derived, paste0("RPG_Aggregated_", region_code_i, "_", year_i,".rds"))
      if (file.exists(file_path)) {
        message_y <- paste("File '", paste0("RPG_Aggregated_", region_code_i, "_", year_i), "' is correctly saved.", sep="")
        print(message_y)
      } else {
        message_n <- paste("File for region '", paste0(region_code_i, "' and for year '", year_i), "' cannot be correctly prepared.", sep="")
        print(message_n)

      }
      
    }
    else if (rpg_type_i == "pg_gpkg"){
      print(paste("Shapefile 'RPG_PARCELLES.gpkg' exists:", pg_gpkg_file_i))
      # Read the shapefile
      rpg_map_i <- st_read(pg_gpkg_file_i, quiet = TRUE)
      
      # Correct invalid polygons if any
      if (!all(st_is_valid(rpg_map_i))) {
        message("Correct parcels with invalid geometry")
        rpg_map_i <- st_make_valid(rpg_map_i, quiet=T)
      }
      
      # Recompute some variable
      rpg_map_i <- rpg_map_i %>% 
        mutate(
          AREA_COMP = st_area(geom), #To have it in m²
          YEAR = year_i,
          REGION = region_i,
          REGION_CODE = region_code_i,
          CODE_GROUP = gsub("^0", "", code_group),
          CODE_CULTU = code_cultu
        )
      
      ## Aggregation at commune level 
      # Import shp file of communes boundaries:
      communes <- st_read(here(dir$sf, "communes-20220101.shp"), quiet = T)
      
      # Transpose CRS to have the same as RPG files CRS:
      communes_crs_i <- st_transform(communes, crs = st_crs(rpg_map_i))
      
      # Compute intersection
      rpg_map_intersection_i <- st_intersection(rpg_map_i, communes_crs_i)
      
      # Calculate areas
      rpg_map_intersection_i$intersect_area <- st_area(rpg_map_intersection_i)
      
      # Compute share of parcel area in the commune
      rpg_map_intersection_i <- rpg_map_intersection_i %>%
        mutate(percent_in_commune = as.numeric(intersect_area / AREA_COMP ),
               weight = ifelse(
                 # remove parcel of the commune if less than 10% of its area in commune, 
                 # keep all the parcel if more than 90% of its area in commune,
                 # otherwise weight the parcel in the commune aggregate by the share of its area in the commune
                 percent_in_commune < 0.1, 
                 0,
                 ifelse(
                   percent_in_commune >= 0.1 & percent_in_commune < 0.9,
                   percent_in_commune,
                   ifelse(
                     percent_in_commune >= 0.9,
                     1,
                     NA
                     )
                   )
                 )
               )
      
      
      # Save this shape file only if corsica, to keep a detailed version of the aggregation process for one region
      if (all_rpg_links$region_code[i] == "R94"){
        # Save the shapefile
        st_write(rpg_map_intersection_i, here(dir$derived, paste0("RPG_Detailed_", region_code_i, "_", year_i, ".shp")), delete_dsn = TRUE)
        print(paste("Detailed shapefile saved for ", all_rpg_links$region_code[i], " for year ", all_rpg_links$year[i]))
      }
      
      # Compute useful variable at the geo_unit level
      rpg_map_intersection_i <- rpg_map_intersection_i %>%
        group_by(insee) %>% #First: construct a variable that for each parcel gives the surface of the agricultural area of the geo_unit in which it is located
        mutate(N_Parcels = sum(weight > 0), # Total number of parcels in the commune (only counting a parcel in a commune if its area in it is greater than 10% ot the parcel area, thus using the weight variable)
               surf_agri_geo_unit_m2 = sum(AREA_COMP * weight, na.rm = T), # Surface of all agricultural parcels in the geo_unit
        )
      
      
      # For all region, then save the aggregate shape file
      rpg_result_i <- rpg_map_intersection_i %>% # Object that contain descriptive statistics
        group_by(insee, CODE_CULTU) %>% # Aggregate by culture
        summarise(
          data_type = "pg_gpkg",
          region_code = first(region_code_i),
          name = first(nom),
          year = first(YEAR),
          CODE_GROUP = first(CODE_GROUP),
          surf_tot_geo_unit_m2 = first(surf_ha)*10000,
          surf_agri_geo_unit_m2 = first(surf_agri_geo_unit_m2), # Surface of all agricultural parcels in the geo_unit
          surf_cult_m2 = sum(AREA_COMP * weight, na.rm = T),# Surface of all parcels dedicated to the culture in the commune
          surf_cult_perc = sum(AREA_COMP * weight, na.rm = T) / first(surf_agri_geo_unit_m2), # Percentage dedicated to the culture in the total agricultural area of the commune
          parcel_cult_n  = sum(weight>0), # Careful, forgotten na_rm = T
          N_Parcels = first(N_Parcels),
          parcel_cult_perc = parcel_cult_n / N_Parcels[1]) %>% 
        # Remove raws with 0 surface for the culture, to avoid having too many raws with 0 surface for each culture in the final dataset
        filter(as.numeric(surf_cult_m2) != 0)
      
      # Save it in prepared data folder
      saveRDS(st_drop_geometry(rpg_result_i), file = here(dir$derived, paste0("RPG_Aggregated_", region_code_i, "_", year_i,".rds")))
      
      # Return message that it is saved
      file_path <- here(dir$derived, paste0("RPG_Aggregated_", region_code_i, "_", year_i,".rds"))
      if (file.exists(file_path)) {
        message_y <- paste("File '", paste0("RPG_Aggregated_", region_code_i, "_", year_i), "' is correctly saved.", sep="")
        print(message_y)
      } else {
        message_n <- paste("File for region '", paste0(region_code_i, "' and for year '", year_i), "' cannot be correctly prepared.", sep="")
        print(message_n)
        
      }
      
    }
  } else{
    
    print(paste("No shapefile found for ", all_rpg_links$region_code[i], " for year ", all_rpg_links$year[i]))
    
  }

  
  # return(paste("Finished processing : ", region_code_i, year_i))0
  print(paste("Finished processing : ", region_code_i, year_i))
}


# library(furrr)
# 
# plan(multisession, workers = parallel::detectCores() - 1) # Or "multicore" on Linux
# 
# # Execute in parallel
# results <- future_map(seq_len(nrow(all_rpg_links)), process_data, .progress = TRUE)

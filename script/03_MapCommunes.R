#===============================================================================
# Description: Map RPG data at the commune level 
# author: 
#===============================================================================

#===============================================================================
# 1). Prepare environment ------
#===============================================================================
# Prepare environment
# Clean memory 
rm(list=ls())
gc()

# Load package
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, data.table, here, sf, tmap)

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

#===============================================================================
# 2) Load list of downloaded RPG files  ------
#===============================================================================
readRDS(here(dir$raw, "rpg_files.rds"))


#===============================================================================
# 3) Parametrize according to the region, year and zoom  ------
#===============================================================================
region_code <- "R94"    # Code région (exemple : Corse = R94)
year <- 2008            # Année
zoom_factor <- 0.1       # Proportion de zoom (0.1 = 10%)

#===============================================================================
# 4) Find and read the parcel shape file  ------
#===============================================================================
# Look for all parcelles graphiques files
rpg_files_all <- list.files(dir$derived, pattern = "RPG_Aggregated", recursive = TRUE, full.names = TRUE)

# Filter those for the parametrized region and year
rpg_files_filtered <- rpg_files_all[
  grepl(region_code, rpg_files_all) & grepl(as.character(year), rpg_files_all)
]

# Take the first one
if (length(rpg_files_filtered) == 0) stop("Aucun fichier shapefile trouvé pour cette région et cette année.")
rpg_file <- rpg_files_filtered[1]

# Read the file
rpg <- readRDS(rpg_file)

#===============================================================================
# 5) Read the commune shape file  ------
#===============================================================================

communes <- st_read(here(dir$raw, "communes-20220101.shp"), quiet = TRUE)

# Set same CRS 
communes <- st_transform(communes, st_crs(rpg))

#===============================================================================
# 6) Plot  ------
#===============================================================================

#  Calcul des limites pour zoomer
bbox <- st_bbox(rpg)
x_range <- bbox$xmax - bbox$xmin
y_range <- bbox$ymax - bbox$ymin
x_center <- (bbox$xmin + bbox$xmax)/2
y_center <- (bbox$ymin + bbox$ymax)/2

xlim_zoom <- c(x_center - x_range*zoom_factor/2, x_center + x_range*zoom_factor/2)
ylim_zoom <- c(y_center - y_range*zoom_factor/2, y_center + y_range*zoom_factor/2)

# Simplify geometry to get maps with smaller size
communes_simple <- st_simplify(communes, dTolerance = 100)
rpg_simple <- st_simplify(rpg, dTolerance = 100)

#  Map
map <- ggplot() +
  geom_sf(data = communes_simple, fill = NA, color = "black") +
  geom_sf(data = rpg_simple, aes(fill = CODE_GROUP), color = NA, alpha = 0.35) +
  geom_sf_text(data = communes_simple, aes(label = nom), size = 3) +
  coord_sf(xlim = xlim_zoom, ylim = ylim_zoom) +
  theme_minimal()+
  theme(legend.position="none")

# Sauvegarde
ggsave(filename = file.path(dir$output, paste0("map_", region_code, "_", year, ".pdf")),
       plot = map, width = 9, height = 16, device = cairo_pdf)

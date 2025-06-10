#===============================================================================
# Description: Tools to vizualise to compare parcels across years next to each others
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
pacman::p_load(tidyverse, data.table, here, sf, tmap, patchwork, rmapshaper, htmlwidgets, leaflet)

# List directories 
dir <- list()
dir$root <- here()
dir$data <- here(dir$root, "data")
dir$sf <- here(dir$data, "shapefiles")
dir$raw <- here(dir$data, "raw")
# dir$raw <- "C:/Users/loihenry/Dropbox/Recherche_Dauphine/DataArchive/RPG_data/Data/raw"
dir$derived <- here(dir$data, "derived")
dir$detailed <- "C:/Users/loihenry/Dropbox/Recherche_Dauphine/DataArchive/RPG_data/Data/detailed_communes"
dir$script <- here(dir$root, "script")
dir$output <- here(dir$root, "output")

# Create non existing directories
lapply(dir, function(i) dir.create(i, recursive = T, showWarnings = F))

#===============================================================================
# 2) Load list of downloaded RPG files  ------
#===============================================================================
list_rpg_files <- readRDS(here(dir$raw, "rpg_files.rds"))


#===============================================================================
# 3) Parametrize according to the region, year and zoom  ------
#===============================================================================
region_code <- "R94"    # Code région (exemple : Corse = R94)
year1 <- 2015           # Année1
year2 <- 2023            # Année2 
zoom_factor <- 0.2       # Proportion de zoom (0.1 = 10%)

#===============================================================================
# 4) Find and read the parcel shape file  ------
#===============================================================================
# Look for all parcelles graphiques files
shp_files_all <- list.files(dir$detailed, pattern = "RPG_Detailed*", recursive = TRUE, full.names = TRUE)

# Filter those for the parametrized region and year
shp_files_filtered_year1 <- shp_files_all[
  grepl(region_code, shp_files_all) & grepl(as.character(year1), shp_files_all)
]
shp_files_filtered_year2 <- shp_files_all[
  grepl(region_code, shp_files_all) & grepl(as.character(year2), shp_files_all)
]

# Take the first one
if (length(shp_files_filtered_year1) == 0) stop("Aucun fichier shapefile trouvé pour cette région et cette année.")
if (length(shp_files_filtered_year2) == 0) stop("Aucun fichier shapefile trouvé pour cette région et cette année.")
shp_file_year1 <- shp_files_filtered_year1[1]
shp_file_year2 <- shp_files_filtered_year2[1]

# Read the file
rpg_year1 <- read_sf(shp_file_year1)
rpg_year2 <- read_sf(shp_file_year2)

rpg_data <- rpg_year1 |> 
  bind_rows(rpg_year2)

# Add label
cultures <- read_delim(here(dir$raw,"REF_CULTURES_GROUPES_CULTURES_2020.csv"), delim = ";")
cultures <- cultures %>%
  mutate(CODE_GROUPE_CULTURE = as.character(CODE_GROUPE_CULTURE))

cultures_clean <- cultures %>%
  select(-CODE_CULTURE, -LIBELLE_CULTURE) %>% 
  group_by(CODE_GROUPE_CULTURE) %>%
  summarise(
    CODE_GR = first(CODE_GROUPE_CULTURE),
    LIBELLE_GROUPE_CULTURE = first(LIBELLE_GROUPE_CULTURE),
    .groups = "drop"
  ) %>% 
  select(-CODE_GROUPE_CULTURE)

rpg_data <- rpg_data |>
  left_join(cultures_clean, by = c("CODE_GR"))

#===============================================================================
# 5) Read the commune shape file  ------
#===============================================================================

communes <- st_read(here(dir$sf, "communes-corsica-20220101.shp"), quiet = TRUE)

# Set same CRS
communes <- st_transform(communes, st_crs(rpg_year1))

#===============================================================================
# 6) Static plot  ------
#===============================================================================

#  Calcul des limites pour zoomer
bbox1 <- st_bbox(rpg_data)
x_range <- bbox1$xmax - bbox1$xmin
y_range <- bbox1$ymax - bbox1$ymin
x_center <- (bbox1$xmin + bbox1$xmax)/2
y_center <- (bbox1$ymin + bbox1$ymax)/2

xlim_zoom <- c(x_center - x_range*zoom_factor/2, x_center + x_range*zoom_factor/2)
ylim_zoom <- c(y_center - y_range*zoom_factor/2, y_center + y_range*zoom_factor/2)

# bbox2 <- st_bbox(rpg_year1)
# x_range <- bbox2$xmax - bbox2$xmin
# y_range <- bbox2$ymax - bbox2$ymin
# x_center <- (bbox2$xmin + bbox2$xmax)/2
# y_center <- (bbox2$ymin + bbox2$ymax)/2

# xlim_zoom <- c(x_center - x_range*zoom_factor/2, x_center + x_range*zoom_factor/2)
# ylim_zoom <- c(y_center - y_range*zoom_factor/2, y_center + y_range*zoom_factor/2)

# Simplify geometry to get maps with smaller size
communes_simple <- st_simplify(communes, dTolerance = 100)
rpg_simple <- st_simplify(rpg_data, dTolerance = 100)
# rpg_simple2 <- st_simplify(rpg_year2, dTolerance = 100)

#  Map
map <- ggplot() +
  geom_sf(data = rpg_simple, aes(fill = CODE_CU), color = NA, alpha = 0.35) +
  facet_wrap(~YEAR) +
  geom_sf_text(data = communes_simple, aes(label = nom), size = 3) +
  geom_sf(data = communes_simple, fill = NA, color = "black") +
  coord_sf(xlim = xlim_zoom, ylim = ylim_zoom) +
  theme_minimal() +
  theme(legend.position="right")

#Sauvegarde
ggsave(filename = file.path(dir$output, paste0("map_", region_code, "_", year1, "_", year2, ".pdf")),
       plot = map, width = 18, height = 9, device = cairo_pdf)


#===============================================================================
# 7) Interactive plot  ------
#===============================================================================
#Map interactive 


#Charger le fichier
parcelles <- rpg_data  %>%
  st_make_valid(my_sf) %>% 
  st_transform(4326)   #conversion des coordonnées pour utilisation package leaflet

parcelles_simplify <- ms_simplify(parcelles, keep = 0.15)

unique(parcelles$YEAR)

#Création de 3 couches distinctes 
parcelles_vierge <- parcelles_simplify 
parcelles_1 <- parcelles_simplify %>% filter (YEAR == "2015")
parcelles_2 <- parcelles_simplify %>% filter (YEAR == "2023")

#Palette de n couleurs (n = nombre de cultures présentent dans CODE_CULTU)
palette_culture <- colorFactor(rainbow(length(unique(parcelles$CODE_GR))),
                                                  parcelles$CODE_GR)

#Création de la carte interactive                                
carte_interactive_Corse <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>% #ajout d'un fond de carte CartoDB
  
  # # Calque 1 : contours vierge
  # addPolygons(data = parcelles_vierge,
  #             weight = 1,
  #             color = NA,
  #             fill = FALSE,
  #             group = "Fond vierge") %>%
  
  # Calque 2 : 201 coloré par culture
  addPolygons(data = parcelles_1,
              fillColor = ~palette_culture(CODE_GR),
              color = "black", weight = 0.5,
              fillOpacity = 0.6,
              label = ~paste("Culture:", CODE_GR), #affichage label en survol avec le nom de la culture 
              group = "Cultures 2022") %>%
  
  # Calque 3 : 2023 coloré par culture
  addPolygons(data = parcelles_2,
              fillColor = ~palette_culture(CODE_GR),
              color = "black", weight = 0.5,
              fillOpacity = 0.6,
              label = ~paste("Culture:", CODE_GR),
              group = "Cultures 2023") %>%
  
  # Légende
  addLegend("bottomleft", pal = palette_culture,
            values = unique(parcelles$CODE_GR),
            title = "Cultures") %>%
  
  # Contrôle des calques (bascule entre les différents calques)
  addLayersControl(
    # baseGroups = c("Fond vierge"),
    overlayGroups = c("Cultures 2015", "Cultures 2023"),
    options = layersControlOptions(collapsed = FALSE)
  ) 

#Sauvegarder la carte interactive 
saveWidget(carte_interactive_Corse, file = file.path(dir$output, "carte_interactive_corse.html"), selfcontained = FALSE)

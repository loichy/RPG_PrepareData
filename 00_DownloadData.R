#===============================================================================
# Description: Extract all file names of RPG data on the URL adress
# author: 
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
pacman::p_load(tidyverse, data.table, here, sf, tmap, rvest, archive)

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
# 2). Extract all file names from website and get information on region name + year
#===============================================================================

# Idea: scrape all url links, and associate region name and year
# Specify the URL of the webpage
url <- "https://geoservices.ign.fr/rpg"

# Read the webpage
webpage <- read_html(url)

# Define the xpath of the block with all downloading links
block_xpath <- '//*[@id="block-ignpro-content"]/div/article/div[2]/div[1]/div[1]/div/div[2]/div[4]/div/div'

# Extract the specific block using the XPath
block <- webpage %>% 
  html_node(xpath = block_xpath)

# Extract the ul elements within the block
ul_elements <- block %>% 
  html_nodes("ul")

# Extract all the URLs from the ul elements
ul_urls <- ul_elements %>% 
  html_nodes("a") %>% 
  html_attr("href")



# Now, create a database of region code and name
region_code_dta <- data.frame(
  region_name = c("Auvergne-Rhône-Alpes",
                  "Bourgogne-Franche-Comté",
                  "Bretagne",
                  "Centre-Val de Loire",
                  "Corse",
                  "Grand Est",
                  "Hauts-de-France",
                  "Île-de-France",
                  "Normandie",
                  "Nouvelle-Aquitaine",
                  "Occitanie",
                  "Pays de la Loire",
                  "Provence-Alpes-Côte d’Azur"),
  region_code = c("R84",
                  "R27",
                  "R53",
                  "R24",
                  "R94",
                  "R44",
                  "R32",
                  "R11",
                  "R28",
                  "R75",
                  "R76",
                  "R52",
                  "R93")
)

# Create dataframe with region and year of all existing files
years <- as.character(c(2007:2023)) # Data exists from 2007 to 2023
rpg_dta_structure <- expand.grid(year = years, region_name = region_code_dta$region_name) %>% # I make all the cartesian combinations of years and region code
  left_join(region_code_dta, by = "region_name") %>% # I add the region name
  dplyr::select(region_name, region_code, year)

# To finally add a variable containing the URL link:
region_codes <- paste(unique(rpg_dta_structure$region_code), 
                      collapse = "|") # Vector to identify region code in the url link
all_years <- paste(unique(rpg_dta_structure$year), 
                   collapse = "|") # Vector to identify year of observation in the url link
url_df <- data.frame(
  url = ul_urls) %>% 
  mutate(region_code = str_extract(url, region_codes), # Associate to each url its region code
         year = str_extract(url, all_years) # Associate to each  url its year
  ) %>% 
  filter(!is.na(region_code))
table(url_df$year)

# Final data: merge to add the url links
all_rpg_links <- url_df %>% 
  left_join(rpg_dta_structure, by = c("region_code","year")) %>% 
  dplyr::select(region_name, region_code, year, url) %>% 
  arrange(year, region_name) %>% 
  mutate(url = gsub("[\r\n]", "", url))

# Avant la boucle (à l'extérieur): créer une data frame vide ou une liste qui est vide
tableau_final <- list()

# Save all-rpg_lings dataset in RDS format
saveRDS(all_rpg_links, here(dir$raw, "rpg_files.rds"), )

#===============================================================================
# 2). From URL links: download archive in proper folder and unarchive it
#===============================================================================
##### Create a loop that download all RPG data files
all_rpg_links <- all_rpg_links %>% 
  filter(region_code != "R94") # Except Corsica

# First funcction: download data
# Takes approximately five hours (DO NOT RUN!)
for(i in seq(all_rpg_links$url)){
  # i <- 13
  print(paste(all_rpg_links$region_code[i], " for year ", all_rpg_links$year[i]))
  # Create file where to download
  destfile_zip <- here(dir$raw, paste0("tempfile_zip_", all_rpg_links$region_code[i], "_", all_rpg_links$year[i]))
  dir.create(destfile_zip)
  # Download the file
  options(timeout=1000)
  download.file(all_rpg_links$url[i], here(destfile_zip,"temp.001"), mode = "wb", quiet = T )
  # Print a message indicating the download is complete
  print(paste("File downloaded to", destfile_zip))
  Sys.sleep(1)

  # Specify the destination file path

  destfile <- here(dir$raw, paste0("tempfile_", all_rpg_links$region_code[i], "_", all_rpg_links$year[i]))
  dir.create(destfile)

  # Unzip file
  # archive(here(destfile_zip, "temp.001"))
  archive_extract(archive = here(destfile_zip,"temp.001"), dir = here(destfile))

  # Function to find the .shp file
  find_pg_shp_file <- function(directory) {
    shp_files <- list.files(destfile, pattern = "PARCELLES_GRAPHIQUES\\.shp$", recursive = TRUE, full.names = TRUE)
    if (length(shp_files) > 0) {
      return(shp_files[1])  # Return the first matching .shp file
    } else {
      return(NULL)
    }
  }

  find_ia_shp_file <- function(directory) {
    shp_files <- list.files(destfile, pattern = "ILOTS_ANONYMES\\.shp$", recursive = TRUE, full.names = TRUE)
    if (length(shp_files) > 0) {
      return(shp_files[1])  # Return the first matching .shp file
    } else {
      return(NULL)
    }
  }

  # Find the PARCELLES_GRAPHIQUES.shp file
  pg_shp_file <- find_pg_shp_file(destfile)
  # Find the PARCELLES_GRAPHIQUES.shp file
  ia_shp_file <- find_ia_shp_file(destfile)


  if (!is.null(pg_shp_file)) {
    print(paste("Shapefile 'PARCELLES_GRAPHIQUES' exists:", pg_shp_file))

    # Additional processing can go here
  } else if (!is.null(ia_shp_file)){
    print(paste("Shapefile 'ILOTS_ANONYMES' is the only one:", ia_shp_file))

  } else {
    print("No shapefile found")
  }

  if (dir.exists(here(destfile_zip))) {
    #Delete file if it exists
    unlink(here(destfile_zip), recursive = T)
  }

  Sys.sleep(1)
}


# title: MapBiomas Soil
# subtitle: 15. Merge external data
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025
rm(list = ls())

# Set MapBiomas Soil Collection
collection <- "c3"

# Source helper functions and packages
source("src/00_helper_functions.r")

# SOILDATA #########################################################################################

# Read data from previous processing script
file_path <- "data/14_soildata.txt"
soildata <- data.table::fread(file_path, sep = "\t", na.strings = c("", "NA", "NaN"))
summary_soildata(soildata)
# Layers: 53562
# Events: 18676
# Georeferenced events: 16170
# Datasets: 261

# EXTERNAL DATA ####################################################################################

# Pseudo-samples: beach, dune, and sandy spot
folder_path <- "data/2025_10_23_pseudo_amostras_dunas_praias_areiais"
# List all SHP files in the folder
shp_files <- list.files(
  path = folder_path,
  pattern = "\\.shp$",
  full.names = TRUE, recursive = TRUE
)
# Read and merge all SHP files
pseudo_sand <- lapply(shp_files, sf::st_read)
pseudo_sand <- do.call(rbind, pseudo_sand)

if (FALSE) {
  mapview::mapview(pseudo_sand)
}

# Extract the coordinates into a data.table
pseudo_dt <- data.table::as.data.table(sf::st_coordinates(pseudo_data))

# Set columns
# dataset_id: ctbsand
# obsert



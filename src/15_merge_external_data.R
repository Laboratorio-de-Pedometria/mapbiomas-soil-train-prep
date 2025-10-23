# title: MapBiomas Soil
# subtitle: 15. Merge external data
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025
rm(list = ls())

# Set MapBiomas Soil Collection
collection <- "c3"

# Install and load required packages
if (!requireNamespace("data.table")) {
  install.packages("data.table")
}
if (!requireNamespace("sf")) {
  install.packages("sf")
}

# Source helper functions
source("src/00_helper_functions.r")

# SOILDATA

# Read data from previous processing script
file_path <- "data/12_soildata.txt"
soildata <- data.table::fread(file_path, sep = "\t", na.strings = c("", "NA", "NaN"))
summary_soildata(soildata)
# Layers: 51152
# Events: 16994
# Georeferenced events: 14372
# Datasets: 259


# EXTERNAL DATA ####################################################################################

# Pseudo-samples: dunes, beaches
folder_path <- "data/2025_10_23_pseudo_amostras_dunas_praias_areiais"
# List all SHP files in the folder
shp_files <- list.files(
  path = folder_path,
  pattern = "\\.shp$",
  full.names = TRUE, recursive = TRUE
)
# Read and merge all SHP files
pseudo_list <- lapply(shp_files, sf::st_read)
pseudo_data <- do.call(rbind, pseudo_list)

if (FALSE) {
  mapview::mapview(pseudo_data)
}

# Extract the coordinates into a data.table
pseudo_dt <- data.table::as.data.table(sf::st_coordinates(pseudo_data))

# Set columns
# dataset_id: ctbsand
# obsert



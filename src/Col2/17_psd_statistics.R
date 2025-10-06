# title: Compute statistics for PSD data
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2025 CC-BY
rm(list = ls())

####################################################################################################
# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("sf")) {
  install.packages("sf")
}
if (!require("geobr")) {
  install.packages("geobr")
}

# Source helper functions
source("src/00_helper_functions.r")

# Read SoilData data processed in the previous script
folder_path <- "~/Insync/MapBiomas Solo/Trainning samples/"
file_name <- "-clay-silt-sand-log-ratio.csv"
# List existing files in the folder_path and get the last one. Then read it.
existing_files <- list.files(path = folder_path, pattern = file_name)
last_file <- existing_files[length(existing_files)]
soildata <- data.table::fread(paste0(folder_path, last_file))
summary_soildata(soildata)

# Drop columns: depth, log_clay_sand, log_silt_sand
soildata <- soildata[, c("depth", "log_clay_sand", "log_silt_sand") := NULL]

# Clean column id: drop all content after the first dash
soildata[, id := sub("-.*", "", id)]

# Create spatial data
soildata_sf <- sf::st_as_sf(soildata, coords = c("coord_x", "coord_y"), crs = 4326)

# Read biomes and transform to WGS84
biomes <- geobr::read_biomes()[-7, ]
biomes <- sf::st_transform(biomes, crs = 4326)

# Overlay biomes with soil data
soildata_sf <- sf::st_join(soildata_sf, biomes)

# Transform back to data.table
soildata_sf <- data.table::as.data.table(soildata_sf)

# Cound the number of samples per 'id' per 'name_biome'
psd_ctb_data_per_biome <- soildata_sf[, .N, by = .(id, name_biome)]

# Save the statistics to a TXT file in the 'res/tab' folder
file_path <- "res/tab/psd_ctb_data_per_biome.txt"
data.table::fwrite(psd_ctb_data_per_biome, file_path)
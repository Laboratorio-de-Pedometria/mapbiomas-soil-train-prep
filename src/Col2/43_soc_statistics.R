# title: Compute statistics for SOC stock data
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

# Read data processed in the previous script
soildata <- data.table::fread("data/40_soildata_soc.txt")
summary_soildata(soildata)
# Layers: 12666
# Events: 12666
# Georeferenced events: 12666

# How many samples from the IFN?
# IFN_index == 1
ifn_samples <- soildata[IFN_index == 1, ]
# Drop the ifn_samples with 'id' that contains '-TREP' or '-XYREP'
ifn_samples <- ifn_samples[!grepl("-TREP|-XYREP", id), ]
# Check the number of IFN samples
nrow(ifn_samples)

# Drop columns: soc_stock_g_m2, year
soildata <- soildata[, c("soc_stock_g_m2", "year") := NULL]

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
soc_ctb_data_per_biome <- soildata_sf[, .N, by = .(id, name_biome)]

# Save the statistics to a TXT file in the 'res/tab' folder
file_path <- "res/tab/soc_ctb_data_per_biome.txt"
data.table::fwrite(soc_ctb_data_per_biome, file_path)

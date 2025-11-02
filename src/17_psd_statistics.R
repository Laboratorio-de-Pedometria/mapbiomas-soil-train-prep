# title: MapBiomas Soil
# subtitle: 17. PSD statistics
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2025 CC-BY
rm(list = ls())

# Set MapBiomas Soil Collection
collection <- "c3"

# Source helper functions and packages
source("src/00_helper_functions.r")

# Load PSD data ####################################################################################
# Read PSD data processed in the previous script
folder_path <- "res/tab/"
file_name <- "soildata_psd.csv"
# List existing files in the folder_path and get the last one. Then read it.
existing_files <- list.files(path = folder_path, pattern = file_name)
last_file <- existing_files[length(existing_files)]
soildata_psd <- data.table::fread(paste0(folder_path, last_file), na.strings = c("NA", "NaN", ""))
summary(soildata_psd)

# Clean data #######################################################################################

# Clean ID
# sand-pseudo -> sandpseudo
soildata_psd[, id := sub("-pseudo", "pseudo", id)]
# rock-pseudo -> rockpseudo
soildata_psd[, id := sub("-pseudo", "pseudo", id)]

# dataset_id <- id before the first dash
soildata_psd[, dataset_id := sub("-.*", "", id)]

# observacao_id <- id after the first dash
soildata_psd[, observacao_id := sub(".*-", "", id)]

# drop id
soildata_psd[, id := NULL]








# Create spatial data
soildata_psd_sf <- sf::st_as_sf(soildata_psd, coords = c("longitude", "latitude"), crs = 4326)





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
# title: MapBiomas Soil
# subtitle: 17. PSD statistics
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2025 CC-BY
rm(list = ls())

# Set MapBiomas Soil Collection
collection <- "c3"

# Source helper functions and packages
source("src/00_helper_functions.r")
if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
  install.packages("rnaturalearth", dependencies = TRUE)
}
if (!requireNamespace("prettymapr", quietly = TRUE)) {
  install.packages("prettymapr", dependencies = TRUE)
}

# Read the Brazilian country boundary
brazil <- geobr::read_country()
brazil <- sf::st_transform(brazil, crs = 4326)
print(brazil)

# Read the Brazilian biome boundaries
biomes <- sf::st_read("data/brazil_biomes.geojson")
print(biomes)

# Read the Brazilian state boundaries
states <- sf::st_read("data/brazil_states.geojson")
states <- sf::st_transform(states, crs = 4326)
print(states)

# Load PSD data ####################################################################################
# Read PSD data processed in the previous script
folder_path <- "res/tab/"
file_name <- "soildata_psd.csv"
# List existing files in the folder_path and get the last one. Then read it.
existing_files <- list.files(path = folder_path, pattern = file_name)
last_file <- existing_files[length(existing_files)]
soildata_psd <- data.table::fread(paste0(folder_path, last_file), na.strings = c("NA", "NaN", ""))

# Clean columns
# sand-pseudo -> sandpseudo
soildata_psd[, id := sub("-pseudo", "pseudo", id)]
# rock-pseudo -> rockpseudo
soildata_psd[, id := sub("-pseudo", "pseudo", id)]
# dataset_id <- id before the first dash
soildata_psd[, dataset_id := sub("-.*", "", id)]
# observacao_id <- id after the first dash
soildata_psd[, observacao_id := sub(".*-", "", id)]
# latitude -> coord_y
soildata_psd[, coord_y := latitude]
# longitude -> coord_x
soildata_psd[, coord_x := longitude]
summary_soildata(soildata_psd)
# Layers: 48900
# Events: 14948
# Georeferenced events: 14948
# Datasets: 196

# Figure 1. Frequency distribution of particle size components #####################################
file_path <- paste0("res/fig/", collection, "_psd_histogram.png")
png(file_path, width = 480 * 4, height = 480 * 2, res = 72 * 2)
par(mfrow = c(2, 4))
hist(
  soildata_psd$esqueleto,
  main = "Skeleton", xlab = "Skeleton (dag/kg), whole soil", col = "gray"
)
hist(
  soildata_psd$areia,
  main = "Sand", xlab = "Sand (dag/kg), fine earth", col = "gray"
)
hist(
  soildata_psd$silte,
  main = "Silt", xlab = "Silt (dag/kg), fine earth", col = "gray"
)
hist(
  soildata_psd$argila,
  main = "Clay", xlab = "Clay (dag/kg), fine earth", col = "gray"
)
hist(
  soildata_psd$log_esqueleto1p_argila1p,
  main = "log(Skeleton/Clay)", xlab = "log(Skeleton/Clay), whole soil", col = "gray"
)
hist(
  soildata_psd$log_areia1p_argila1p,
  main = "log(Sand/Clay)", xlab = "log(Sand/Clay), whole soil", col = "gray"
)
hist(
  soildata_psd$log_silte1p_argila1p,
  main = "log(Silt/Clay)", xlab = "log(Silt/Clay), whole soil", col = "gray"
)
hist(
  soildata_psd$profundidade,
  main = "Depth", xlab = "Depth (cm)", col = "gray"
)
dev.off()

# Figure 2. Spatial distribution of samples with particle size data ################################
# Select the first occurrence of each 'id' to avoid overplotting
soildata_psd_sf <- soildata_psd[!duplicated(soildata_psd$id), ]
# Create spatial data
soildata_psd_sf <- sf::st_as_sf(soildata_psd_sf, coords = c("coord_x", "coord_y"), crs = 4326)

# Read South America and transform to WGS84
southamerica <- rnaturalearth::ne_countries(continent = c("south america", "europe"),
  returnclass = "sf", scale = "medium")
southamerica <- southamerica[, "iso_a2"]
# Save figure in Portuguese and English
lang <- c("pt", "en")
main <- list(
  pt = "Amostras para modelagem da granulometria do solo inteiro",
  en = "Samples for modeling the whole-soil particle size distribution"
)
for (i in seq_along(lang)) {
  file_path <- paste0("res/fig/", collection, "_psd_spatial_distribution_", lang[i], ".png")
  png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
  par(mar = rep(1.9, 4))
  plot(brazil,
    reset = FALSE, col = "transparent",
    axes = TRUE, graticule = TRUE, lwd = 0.01,
    main = main[[i]]
  )
  plot(southamerica, reset = FALSE, col = "gray96", add = TRUE, lwd = 0.5)
  plot(biomes[-7, ]["name_biome"], reset = FALSE,
    main = "", axes = TRUE, col = "#eeece1", lwd = 0.5,
    border = "gray69",
    key.pos = NULL, graticule = TRUE, add = TRUE
  )
  plot(soildata_psd_sf["argila"], add = TRUE, cex = 0.5, col = "dodgerblue4")
  prettymapr::addscalebar(plotunit = "latlon", plotepsg = 4326, pos = "bottomright")
  dev.off()
}

# Figure for Dataverse thumbnail ##################################################################
# Supported image types are JPG, TIF, or PNG and should be no larger than 500 KB.
# The maximum display size for an image file as a dataset thumbnail is 48 pixels wide by 48 pixels
# high.
file_path <- paste0("res/fig/", collection, "_psd_spatial_distribution_brazil_thumbnail.png")
png(file_path, width = 480, height = 480, res = 72)
par(mar = rep(0, 4))
plot(brazil,
  reset = FALSE, col = "transparent",
  axes = TRUE, graticule = TRUE, lwd = 0.01,
  main = ""
)
plot(
  biomes[-7, ]["name_biome"],
  reset = FALSE, main = "", axes = FALSE, col = "#eeece1", lwd = 0.5, border = "gray69",
  key.pos = NULL
)
plot(soildata_psd_sf["argila"], add = TRUE, cex = 0.5, col = "dodgerblue4")
dev.off()

# Compute statistics for PSD data ##################################################################

# Overlay biomes with soil data
old_s2 <- sf::sf_use_s2()
sf::sf_use_s2(FALSE)
soildata_psd_sf <- sf::st_join(soildata_psd_sf, biomes[-7, ])
sf::sf_use_s2(old_s2)

# Overlay states with soil data
soildata_psd_sf <- sf::st_join(soildata_psd_sf, states)

# Back to data.table
soildata_psd <- data.table::as.data.table(soildata_psd_sf)

# Count the number of samples per 'name_biome'
psd_stats_biome <- soildata_psd[, .N, by = name_biome][order(-N)]
print(psd_stats_biome)
data.table::fwrite(psd_stats_biome,
  file = paste0("res/tab/", collection, "_psd_stats_by_biome.csv"),
  sep = ";", dec = ".", row.names = FALSE
)

# Count the number of samples per 'name_state'
psd_stats_state <- soildata_psd[, .N, by = name_state][order(-N)]
print(psd_stats_state)
data.table::fwrite(psd_stats_state,
  file = paste0("res/tab/", collection, "_psd_stats_by_state.csv"),
  sep = ";", dec = ".", row.names = FALSE
)

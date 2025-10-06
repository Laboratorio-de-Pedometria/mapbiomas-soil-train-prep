# title: SoilData - Soil Organic Carbon Stock
# subtitle: Export Particle Size Distribution
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

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
if (!require("rnaturalearth")) {
  install.packages("rnaturalearth", dependencies = TRUE)
}
if (!require("prettymapr")) {
  install.packages("prettymapr")
  library("prettymapr")
}

# Source helper functions
source("src/00_helper_functions.r")

# Read SoilData data processed in the previous script
soildata <- data.table::fread("data/14_soildata_soc.txt", sep = "\t")
summary_soildata(soildata)
# Layers: 29881
# Events: 15729
# Georeferenced events: 13381

# Create a data.table with the particle size distribution
psd_data <- soildata[
  !is.na(argila) & !is.na(silte) & !is.na(areia) & !is.na(coord_x) & !is.na(coord_y),
  .(id, coord_x, coord_y, profund_sup, profund_inf, argila, silte, areia)
]
psd_data[, depth := profund_sup + (profund_inf - profund_sup) / 2, by = .I]

# Drop rows with depth > 40 cm
psd_data <- psd_data[depth <= 40]
summary_soildata(psd_data)
# Layers: 19965
# Events: 11633
# Georeferenced events: 11633

# Compute additive log ratio transformation
psd_data[, log_clay_sand := log(argila / areia)]
psd_data[, log_silt_sand := log(silte / areia)]
summary(psd_data[, .(log_clay_sand, log_silt_sand)])

# Figure: Histograms
dev.off()
file_path <- "res/fig/psd_histogram.png"
png(file_path, width = 480 * 3, height = 480 * 2, res = 72 * 2)
par(mfrow = c(2, 3))
hist(psd_data$argila, main = "Clay", xlab = "Clay (%)", col = "gray")
hist(psd_data$silte, main = "Silt", xlab = "Silt (%)", col = "gray")
hist(psd_data$areia, main = "Sand", xlab = "Sand (%)", col = "gray")
hist(psd_data$log_clay_sand, main = "log(Clay/Sand)", xlab = "log(Clay/Sand)", col = "gray")
hist(psd_data$log_silt_sand, main = "log(Silt/Sand)", xlab = "log(Silt/Sand)", col = "gray")
hist(psd_data$depth, main = "Depth", xlab = "Depth (cm)", col = "gray")
dev.off()

# Figure: Spatial distribution
# Create spatial data
psd_data_sf <- sf::st_as_sf(psd_data, coords = c("coord_x", "coord_y"), crs = 4326)
# Read biomes and transform to WGS84
biomes <- geobr::read_biomes()[-7, ]
biomes <- sf::st_transform(biomes, crs = 4326)
# Read Brazil and transform to WGS84
brazil <- geobr::read_country()
brazil <- sf::st_transform(brazil, crs = 4326)
# Read South America and transform to WGS84
southamerica <- rnaturalearth::ne_countries(continent = c("south america", "europe"),
  returnclass = "sf", scale = "medium")
southamerica <- southamerica[, "iso_a2"]
# Save figure in Portuguese and English
lang <- c("pt", "en")
main <- list(
  pt = "Granulometria (argila, silte e areia)",
  en = "Particle size distribution (clay, silt and sand)"
)
dev.off()
for(i in seq_along(lang)) {
  file_path <- paste0("res/fig/psd-spatial-distribution-", lang[i], ".png")
  png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
  par(mar = rep(1.9, 4))
  plot(brazil,
    reset = FALSE, col = "transparent",
    axes = TRUE, graticule = TRUE, lwd = 0.01,
    main = ""
  )
  mtext(main[[i]], side = 3, line = 0.5, cex = 1.5, adj = 0)
  plot(southamerica, reset = FALSE, col = "gray96", add = TRUE, lwd = 0.5)
  plot(biomes["name_biome"], reset = FALSE,
    main = "", axes = TRUE, col = "#eeece1", lwd = 0.5,
    border = "gray69",
    key.pos = NULL, graticule = TRUE, add = TRUE
  )
  plot(psd_data_sf["log_clay_sand"], add = TRUE, cex = 0.5, col = "dodgerblue4")
  prettymapr::addscalebar(plotunit = "latlon", plotepsg = 4326, pos = "bottomright")
  dev.off()  
}

# Figure for Dataverse thumbnail
# Supported image types are JPG, TIF, or PNG and should be no larger than 500 KB.
# The maximum display size for an image file as a dataset thumbnail is 48 pixels wide by 48 pixels
# high.
dev.off()
file_path <- "res/fig/psd-spatial-distribution-brazil-thumbnail.png"
png(file_path, width = 480, height = 480, res = 72)
par(mar = rep(0, 4))
plot(brazil,
  reset = FALSE, col = "transparent",
  axes = TRUE, graticule = TRUE, lwd = 0.01,
  main = ""
)
plot(biomes["name_biome"], reset = FALSE, main = "", axes = FALSE, col = "#eeece1", lwd = 0.5,
  border = "gray69", key.pos = NULL
)
plot(psd_data_sf["log_clay_sand"], add = TRUE, cex = 0.5, col = "dodgerblue4")
dev.off()

# Drop unnecessary columns
psd_data[, c("argila", "silte", "areia", "profund_sup", "profund_inf") := NULL]

# Rename columns
data.table::setcolorder(
  psd_data,
  c("id", "coord_x", "coord_y", "depth", "log_clay_sand", "log_silt_sand")
)
summary_soildata(psd_data)
# Layers: 19965
# Events: 11633
# Georeferenced events: 11633
length(psd_data[, strsplit(id, "-")[[1]][1], by = .I][, unique(V1)])

# Plot using mapview
if (FALSE) {
  psd_data_sf <- sf::st_as_sf(psd_data, coords = c("coord_x", "coord_y"), crs = 4326)
  mapview::mapview(psd_data_sf, zcol = "log_clay_sand")
}

# Write data to disk
folder_path <- "~/Insync/MapBiomas Solo/Trainning samples/"
file_name <- "-clay-silt-sand-log-ratio.csv"
# List existing files in the folder_path and get the last one. Then read it.
existing_files <- list.files(path = folder_path, pattern = file_name)
last_file <- existing_files[length(existing_files)]
last_psd_data <- data.table::fread(paste0(folder_path, last_file))
# Check if last_psd_data == psd_data. If not, write psd_data to disk.
if (!identical(last_psd_data, psd_data)) {
  cat("Writing data to disk...\n")
  file_path <- paste0(folder_path, format(Sys.time(), "%Y-%m-%d"), file_name)
  file_path <- path.expand(file_path)
  data.table::fwrite(psd_data, file_path)
}

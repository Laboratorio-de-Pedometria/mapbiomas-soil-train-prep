# title: MapBiomas Soil
# subtitle: 16. Export Particle Size Distribution
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025
rm(list = ls())

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
# Read SoilData data processed in the previous script
file_path <- "data/15_soildata_soc.txt"
soildata <- data.table::fread(file_path, sep = "\t", na.strings = c("", "NA", "NaN"))
summary_soildata(soildata)
# Layers: 29881
# Events: 15729
# Georeferenced events: 13381
# Datasets: 250

# PARTICLE SIZE DISTRIBUTION
# Create a data.table with the particle size distribution
# The target variables are skeleton (esqueleto), argila (clay), silte (silt), and areia (sand). We
# also need the coordinates (coord_x, coord_y) and the depth interval (profund_sup, profund_inf).
psd_data <- soildata[
  !is.na(esqueleto) & !is.na(argila) & !is.na(silte) & !is.na(areia) &
  !is.na(coord_x) & !is.na(coord_y) &
  !is.na(profund_sup) & !is.na(profund_inf),
  .(id, dataset_id, coord_x, coord_y, profund_sup, profund_inf, esqueleto, argila, silte, areia)
]
summary_soildata(psd_data)
# Layers: 19719
# Events: 10819
# Georeferenced events: 10819
# Datasets: 177

# Update the proportions of the fine earth fractions (argila, silte, areia) to sum to 1000 g/kg
# minus the coarse fragments (esqueleto)
psd_data[, `:=`(
  argila = round(argila / (1000 - esqueleto) * 1000),
  silte = round(silte / (1000 - esqueleto) * 1000),
  areia = round(areia / (1000 - esqueleto) * 1000)
)]
psd_data[, .(argila, silte, areia, esqueleto)]

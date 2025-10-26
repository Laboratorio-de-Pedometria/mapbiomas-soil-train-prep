# title: MapBiomas Soil
# subtitle: 16. Export Particle Size Distribution
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025
rm(list = ls())

# Source helper functions and packages
source("src/00_helper_functions.r")

# SOILDATA
# Read SoilData data processed in the previous script
file_path <- "data/14_soildata.txt"
soildata <- data.table::fread(file_path, sep = "\t", na.strings = c("", "NA", "NaN"))
summary_soildata(soildata)
# Layers: 53562
# Events: 18676
# Georeferenced events: 16170
# Datasets: 261

# PARTICLE SIZE DISTRIBUTION
# Create a data.table with the particle size distribution
# The target variables are skeleton (esqueleto), argila (clay), silte (silt), and areia (sand). We
# also need the coordinates (coord_x, coord_y) and the depth interval (profund_sup, profund_inf).
soildata_psd <- soildata[
  !is.na(esqueleto) & !is.na(argila) & !is.na(silte) & !is.na(areia) &
    !is.na(coord_x) & !is.na(coord_y) &
    !is.na(profund_sup) & !is.na(profund_inf),
  .(id, coord_x, coord_y, profund_sup, profund_inf, esqueleto, argila, silte, areia)
]
summary_soildata(soildata_psd)
# Layers: 38833
# Events: 13775
# Georeferenced events: 13775

# Compute the depth as the midpoint of the depth interval and drop the depth interval columns.
soildata_psd[, profundidade := profund_sup + (profund_inf - profund_sup) / 2, by = .I]
soildata_psd[, `:=`(profund_sup = NULL, profund_inf = NULL)]

# Rename "coord_x" and "coord_y" to "longitude" and "latitude" respectively
data.table::setnames(soildata_psd, old = c("coord_x", "coord_y"), new = c("longitude", "latitude"))

# Reorder columns: id, longitude, latitude, profundidade, esqueleto, areia, silte, argila
col_order <- c("id", "longitude", "latitude", "profundidade", "esqueleto", "areia", "silte", "argila")
data.table::setcolorder(soildata_psd, col_order)

# Update the proportions of the fine earth fractions (argila, silte, areia)
# This the fractions were relative to the soil fine earth (diameter < 2mm). We update
# them to be relative to the whole soil, accounting for the presence of coarse fragments
# (skeleton, diameter > 2 mm). The proportion of skeleton already is relative to the
# whole soil. The sum of the four fractions should be 1000 g/kg.
soildata_psd[, `:=`(
  argila = round(argila * (1000 - esqueleto) / 1000),
  silte = round(silte * (1000 - esqueleto) / 1000),
  areia = round(areia * (1000 - esqueleto) / 1000)
)]
summary(soildata_psd[, .(esqueleto, argila, silte, areia)])

# Validation: Check if the sum of the four fractions is 1000 g/kg
soildata_psd[, total := argila + silte + areia + esqueleto]
print(soildata_psd[total != 1000, ])
soildata_psd[, total := NULL] # Remove the temporary "total" column
# We use "silte" to deal with rounding issues.
soildata_psd[, silte := 1000 - esqueleto - argila - areia]

# 

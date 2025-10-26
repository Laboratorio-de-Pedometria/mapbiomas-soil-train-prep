# title: MapBiomas Soil
# subtitle: 16. Export Particle Size Distribution
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025
rm(list = ls())

# Set MapBiomas Soil Collection
collection <- "c3"

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
soildata_psd <- soildata_psd[, ..col_order]

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

# Now we add one unit to all samples so that the final sum of the four fractions is 1004 g/kg,
# appending "1p" to their name.
soildata_psd[, `:=`(
  argila1p = argila + 1,
  silte1p = silte + 1,
  areia1p = areia + 1,
  esqueleto1p = esqueleto + 1
)]
summary(soildata_psd[, .(esqueleto1p, argila1p, silte1p, areia1p)])
soildata_psd[, total1p := argila1p + silte1p + areia1p + esqueleto1p]
print(soildata_psd[total1p != 1004, ])
soildata_psd[, total1p := NULL] # Remove the temporary "total1p" column
# remove the original fraction columns
soildata_psd[, `:=`(argila = NULL, silte = NULL, areia = NULL, esqueleto = NULL)]

# Compute additive log ratio transformation variables, using "argila1p" as denominator
soildata_psd[, log_silte1p_argila1p := log(silte1p / argila1p)]
soildata_psd[, log_areia1p_argila1p := log(areia1p / argila1p)]
soildata_psd[, log_esqueleto1p_argila1p := log(esqueleto1p / argila1p)]
summary(soildata_psd[, .(log_silte1p_argila1p, log_areia1p_argila1p, log_esqueleto1p_argila1p)])

# Export PSD data for spatial modelling ############################################################
ncol(soildata_psd) # Result: 8
nrow(soildata_psd) # Result: 38833
nrow(unique(soildata_psd[, "id"])) # Result: 13775
# Destination folder
folder_path <- "res/tab/"
file_name <- "soildata_psd.csv"

# List existing files in the folder_path and get the last one. Then read it.
existing_files <- list.files(path = folder_path, pattern = file_name)

write_out <- TRUE
if (length(existing_files) > 0) {
  last_file <- existing_files[length(existing_files)]
  last_soildata_psd <- data.table::fread(paste0(folder_path, last_file))
  # Check if last_soildata_psd == soildata_psd. If not, write soildata_psd to disk.
  # Use all.equal() as it is more robust to type differences after a read/write cycle.
  # isTRUE() is needed because all.equal() returns a character string describing
  # the difference if they are not equal, which would cause an error in an if() statement.
  if (isTRUE(all.equal(last_soildata_psd, soildata_psd))) {
    cat("No changes in PSD data. Not writing to disk.\n")
    write_out <- FALSE
  }
}
if (write_out) {
  cat("Writing PSD data to disk...\n")
  file_name <- paste0(collection, "_", format(Sys.time(), "%Y_%m_%d"), "_soildata_psd.csv")
  file_path <- paste0(folder_path, file_name)
  data.table::fwrite(soildata_psd, file_path)
}

# Load PSD data to Google Earth Engine #############################################################
# Using the API results in a more laborious process, as we need to chunk the data to avoid
# exceeding the 10 MB payload limit per request. Thus, we upload the data mannually using the
# GEE web interface.
# Location: projects/mapbiomas-workspace/SOLOS/AMOSTRAS/ORIGINAIS/collection3/
# Asset name: the same as the CSV file name without the .csv extension

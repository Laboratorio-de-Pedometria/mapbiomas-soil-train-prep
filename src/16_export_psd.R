# title: MapBiomas Soil
# subtitle: 16. Export Particle Size Distribution
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025
rm(list = ls())

# Set MapBiomas Soil Collection
collection <- "c3"

# Source helper functions and packages
source("src/00_helper_functions.r")

# LOAD SOILDATA ####################################################################################
# Read SoilData data processed in the previous script
file_path <- "data/15_soildata.txt"
soildata <- data.table::fread(file_path, sep = "\t", na.strings = c("", "NA", "NaN"))
summary_soildata(soildata)
# Layers: 72468
# Events: 19882
# Georeferenced events: 17366
# Datasets: 267

# Fine earth content (this should be done right after randomForest predictions)
soildata[is.na(terrafina), terrafina := 1000 - esqueleto]

# ROCK LAYERS EXPANSION ############################################################################
# Compute thickness of each soil layer
soildata[, espessura := profund_inf - profund_sup]
rockdata <- soildata[esqueleto == 1000, ]
print(rockdata[, .(id, camada_nome, profund_sup, profund_inf, espessura)])

# If a rock layer has thickness (espessura) greater than 10 cm, slice it into multiple layers of
# about 10 cm that fit into the the original layer thickness (espessura). For example, if a rock
# layer has thickness of 25 cm, slice it into three layers of thickness 8.33 cm each.
rockdata_expanded <- data.table::data.table()
for (i in 1:nrow(rockdata)) {
  layer_thickness <- rockdata[i, espessura]
  if (layer_thickness > 10) {
    num_slices <- ceiling(layer_thickness / 10)
    slice_thickness <- layer_thickness / num_slices
    for (j in 0:(num_slices - 1)) {
      new_layer <- data.table::copy(rockdata[i, ])
      new_layer[, profund_sup := profund_sup + j * slice_thickness]
      new_layer[, profund_inf := profund_sup + slice_thickness]
      new_layer[, espessura := slice_thickness]
      rockdata_expanded <- rbind(rockdata_expanded, new_layer)
    }
  } else {
    rockdata_expanded <- rbind(rockdata_expanded, rockdata[i, ])
  }
}
rockdata_expanded[, profund_sup := round(profund_sup, 1)]
rockdata_expanded[, profund_inf := round(profund_inf, 1)]
rockdata_expanded[, espessura := round(profund_inf - profund_sup, 1)]
# Update layer order (camada_id) by soil profile (id)
rockdata_expanded[, camada_id := seq_len(.N), by = id]
nrow(rockdata_expanded) # 7904 layers (includes all layers)
print(rockdata_expanded[, .(id, camada_nome, profund_sup, profund_inf, espessura)])
# Replace original rock layers with expanded rock layers.
soildata <- soildata[is.na(esqueleto) | esqueleto < 1000, ]
soildata <- rbind(soildata, rockdata_expanded)
summary_soildata(soildata)
# Layers: 74610
# Events: 19882
# Georeferenced events: 17366
# Datasets: 267

# PARTICLE SIZE DISTRIBUTION #######################################################################
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
# Layers: 57363
# Events: 14940
# Georeferenced events: 14940

# Compute the depth as the midpoint of the depth interval and drop the depth interval columns.
soildata_psd[, profundidade := (profund_inf + profund_sup) / 2, by = .I]
soildata_psd[, `:=`(profund_sup = NULL, profund_inf = NULL)]
# Check depth range
summary(soildata_psd$profundidade)
x11()
hist(soildata_psd$profundidade, breaks = 30, main = "Soil layer depth distribution", xlab = "Depth (cm)")

# Drop rows with depth (profundidade) > 100
soildata_psd <- soildata_psd[profundidade <= 100, ]
summary_soildata(soildata_psd)
# Layers: 43855
# Events: 14543
# Georeferenced events: 14543

# Rename "coord_x" and "coord_y" to "longitude" and "latitude" respectively
data.table::setnames(soildata_psd, old = c("coord_x", "coord_y"), new = c("longitude", "latitude"))

# Reorder columns: id, longitude, latitude, profundidade, esqueleto, areia, silte, argila. Then sort
# rows by id and profundidade.
col_order <- c("id", "longitude", "latitude", "profundidade", "esqueleto", "areia", "silte", "argila")
soildata_psd <- soildata_psd[, ..col_order][order(id, profundidade)]
if (interactive()) {
  View(soildata_psd)
}

# DATA TRANSFORMATION ##############################################################################
# Update the proportions of the fine earth fractions (argila, silte, areia)
# The fractions are relative to the soil fine earth (diameter < 2mm). We update them to be relative
# to the whole soil, accounting for the presence of coarse fragments (skeleton, diameter > 2 mm).
# The proportion of skeleton already is relative to the whole soil. We also add one unit to each
# fraction to avoid zero values, appending "1p" to their name. This will be useful for the
# additive log ratio transformation in the next step. The sum of the four fractions should be
# 1004 g/kg.
soildata_psd[, argila1p := round(argila * (1000 - esqueleto) / 1000) + 1]
soildata_psd[, areia1p := round(areia * (1000 - esqueleto) / 1000) + 1]
soildata_psd[, esqueleto1p := esqueleto + 1]
# We use "silte" to deal with rounding issues.
soildata_psd[, silte1p := 1004 - (argila1p + areia1p + esqueleto1p)]
summary(soildata_psd[, .(esqueleto1p, argila1p, silte1p, areia1p)])

# Compute additive log ratio transformation variables, using "argila1p" as denominator
soildata_psd[, log_silte1p_argila1p := log(silte1p / argila1p)]
soildata_psd[, log_areia1p_argila1p := log(areia1p / argila1p)]
soildata_psd[, log_esqueleto1p_argila1p := log(esqueleto1p / argila1p)]
summary(soildata_psd[, .(log_silte1p_argila1p, log_areia1p_argila1p, log_esqueleto1p_argila1p)])

# The back-transform to obtain each of the four individual fractions is as follows:
# argila = exp(0) / (1 + exp(log_silte1p_argila1p) + exp(log_areia1p_argila1p) + exp(log_esqueleto1p_argila1p))
# silte = exp(log_silte1p_argila1p) * argila
# areia = exp(log_areia1p_argila1p) * argila
# esqueleto = exp(log_esqueleto1p_argila1p) * argila
soildata_psd[, argila1p_test := exp(0) / (1 + exp(log_silte1p_argila1p) + exp(log_areia1p_argila1p) + exp(log_esqueleto1p_argila1p))]
soildata_psd[, silte1p_test := exp(log_silte1p_argila1p) * argila1p_test]
soildata_psd[, areia1p_test := exp(log_areia1p_argila1p) * argila1p_test]
soildata_psd[, esqueleto1p_test := exp(log_esqueleto1p_argila1p) * argila1p_test]
print(soildata_psd[, .(argila1p_test, silte1p_test, areia1p_test, esqueleto1p_test)])
# Check if the proportions sum to 1
soildata_psd[, total1p_test := argila1p_test + silte1p_test + areia1p_test + esqueleto1p_test]
print(soildata_psd[total1p_test != 1.0])
soildata_psd[, total1p_test := NULL] # Remove the temporary "total1p_test" column

# Then multiply each fraction by the total sum of fractions (1004 g/kg) to get the absolute
# values, subtract 1 from each fraction to revert the +1 adjustment, and round to the nearest
# integer:
# argila = round(argila * 1004 - 1)
# silte = round(silte * 1004 - 1)
# areia = round(areia * 1004 - 1)
# esqueleto = round(esqueleto * 1004 - 1)
soildata_psd[, `:=`(
  argila_test = round(argila1p_test * 1004 - 1),
  silte_test = round(silte1p_test * 1004 - 1),
  areia_test = round(areia1p_test * 1004 - 1),
  esqueleto_test = round(esqueleto1p_test * 1004 - 1)
)]
print(soildata_psd[, .(argila_test, silte_test, areia_test, esqueleto_test)])
# Check if the proportions sum to 1000 g/kg
soildata_psd[, total_test := argila_test + silte_test + areia_test + esqueleto_test]
print(soildata_psd[total_test != 1000, ])
soildata_psd[, total_test := NULL] # Remove the temporary "total_test" column

# Next, set any negative value to zero (should not happen)
# Check for negative values
print(soildata_psd[argila_test < 0 | silte_test < 0 | areia_test < 0 | esqueleto_test < 0, ])

# Finally, rescale clay, silt, and sand to sum 1000 g/kg: they should be equal to the original
# values!
soildata_psd[, `:=`(
  argila_test = round(argila_test / (1000 - esqueleto_test) * 1000),
  silte_test = round(silte_test / (1000 - esqueleto_test) * 1000),
  areia_test = round(areia_test / (1000 - esqueleto_test) * 1000)
)]
soildata_psd[, argila_diff := abs(argila - argila_test)]
# View(soildata_psd[argila_diff != 0, .(id, argila, argila_test, argila_diff)])
soildata_psd[, silte_diff := abs(silte - silte_test)]
# View(soildata_psd[silte_diff != 0, .(id, silte, silte_test, silte_diff)])
soildata_psd[, areia_diff := abs(areia - areia_test)]
# View(soildata_psd[areia_diff != 0, .(id, areia, areia_test, areia_diff)])
soildata_psd[, esqueleto_diff := abs(esqueleto - esqueleto_test)]
# View(soildata_psd[esqueleto_diff != 0, .(id, esqueleto, esqueleto_test, esqueleto_diff)])
soildata_psd[, argila_diff := NULL]
soildata_psd[, silte_diff := NULL]
soildata_psd[, areia_diff := NULL]
soildata_psd[, esqueleto_diff := NULL]

# The back-transform works fine. We can now remove the test columns.
soildata_psd[, `:=`(argila1p_test = NULL, silte1p_test = NULL, areia1p_test = NULL, esqueleto1p_test = NULL)]
soildata_psd[, `:=`(argila_test = NULL, silte_test = NULL, areia_test = NULL, esqueleto_test = NULL)]

# remove 1p
soildata_psd[, `:=`(argila1p = NULL, silte1p = NULL, areia1p = NULL, esqueleto1p = NULL)]

# Export PSD data for spatial modelling ############################################################
ncol(soildata_psd) # Result: 11 columns (variables)
nrow(soildata_psd) # Result: 43855 rows (layers)
nrow(unique(soildata_psd[, "id"])) # Result: 14543 unique soil profiles
length(unique(sub("-.*$", "", soildata_psd$id))) - 2 # Result: 193 unique datasets (excluding pseudo)

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

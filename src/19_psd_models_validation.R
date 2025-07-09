# title: Soil Particle Size Distribution - Validation Statistics
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2025
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
  library(data.table)
}

# Source helper functions
source("src/00_helper_functions.r")

# Read the data
dir_path <- path.expand("~/ownCloud/MapBiomas/res/tab/")

# log_clay_sand
# Read each file, drop rows with pred_log_clay_sand = NA, and bind them into a single data.table
# "log_clay_sand_000_010cm_cross_validation.txt"
# "log_clay_sand_010_020cm_cross_validation.txt"
# "log_clay_sand_020_030cm_cross_validation.txt"
log_clay_sand_000_010cm <- data.table::fread(
  paste0(dir_path, "log_clay_sand_000_010cm_cross_validation.txt"),
  na.strings = c("NA", "NaN")
)
log_clay_sand_010_020cm <- data.table::fread(
  paste0(dir_path, "log_clay_sand_010_020cm_cross_validation.txt"),
  na.strings = c("NA", "NaN")
)
log_clay_sand_020_030cm <- data.table::fread(
  paste0(dir_path, "log_clay_sand_020_030cm_cross_validation.txt"),
  na.strings = c("NA", "NaN")
)
log_clay_sand <- rbind(
  log_clay_sand_000_010cm[!is.na(pred_log_clay_sand)],
  log_clay_sand_010_020cm[!is.na(pred_log_clay_sand)],
  log_clay_sand_020_030cm[!is.na(pred_log_clay_sand)]
)
print(log_clay_sand)

# log_silt_sand
# Read each file, drop rows with pred_log_silt_sand = NA, and bind them into a single data.table
# "log_silt_sand_000_010cm_cross_validation.txt"
# "log_silt_sand_010_020cm_cross_validation.txt"
# "log_silt_sand_020_030cm_cross_validation.txt"
log_silt_sand_000_010cm <- data.table::fread(
  paste0(dir_path, "log_silt_sand_000_010cm_cross_validation.txt"),
  na.strings = c("NA", "NaN")
)
log_silt_sand_010_020cm <- data.table::fread(
  paste0(dir_path, "log_silt_sand_010_020cm_cross_validation.txt"),
  na.strings = c("NA", "NaN")
)
log_silt_sand_020_030cm <- data.table::fread(
  paste0(dir_path, "log_silt_sand_020_030cm_cross_validation.txt"),
  na.strings = c("NA", "NaN")
)
log_silt_sand <- rbind(
  log_silt_sand_000_010cm[!is.na(pred_log_silt_sand)],
  log_silt_sand_010_020cm[!is.na(pred_log_silt_sand)],
  log_silt_sand_020_030cm[!is.na(pred_log_silt_sand)]
)
print(log_silt_sand)

# cbind the two data.tables by 'id' and 'depth'
pred_psd <- merge(
  log_clay_sand,
  log_silt_sand,
  by = c("id", "depth"),
  suffixes = c("_clay_sand", "_silt_sand")
)
print(pred_psd)

# Back-transform the predictions
# Predictions are additive log-ratios (ALR) of clay, silt, and sand.
# We need to back-transform them to get the actual proportions.
# The formula for back-transforming ALR to proportions is:
#   clay = exp(pred_log_clay_sand) / (1 + exp(pred_log_clay_sand) + exp(pred_log_silt_sand))
#   silt = exp(pred_log_silt_sand) / (1 + exp(pred_log_clay_sand) + exp(pred_log_silt_sand))
#   sand = 1 / (1 + exp(pred_log_clay_sand) + exp(pred_log_silt_sand))
pred_psd[, denominator := 1 + exp(pred_log_clay_sand) + exp(pred_log_silt_sand)]
pred_psd[, pred_clay := round((exp(pred_log_clay_sand) / denominator) * 100)]
pred_psd[, pred_silt := round((exp(pred_log_silt_sand) / denominator) * 100)]
pred_psd[, pred_sand := round((1 / denominator) * 100)]
pred_psd[, denominator := NULL]
dim(pred_psd)
# 15963     7

# Read the original observations
soildata <- read_insync("psd_c02beta_000_010cm_v2.csv")
# Check the data
dim(soildata)
# Rows: 19944
# Columns: 79
# Drop all rows where depth > 30 cm
soildata <- soildata[depth <= 30]
dim(soildata)
# 15963    79

# Back-transform the original observations
# log_clay_sand and log_silt_sand are additive log-ratios (ALR) of clay, silt, and sand.
# We need to back-transform them to get the actual proportions.
soildata[, denominator := 1 + exp(log_clay_sand) + exp(log_silt_sand)]
soildata[, clay := round((exp(log_clay_sand) / denominator) * 100)]
soildata[, silt := round((exp(log_silt_sand) / denominator) * 100)]
soildata[, sand := round((1 / denominator) * 100)]
soildata[, denominator := NULL]
# Check the data
dim(soildata)

# Merge the two data.tables by 'id' and 'depth'
psd_model <- merge(soildata[, .(id, depth, clay, silt, sand, .geo)],
  pred_psd[, .(id, depth, pred_clay, pred_silt, pred_sand)],
  by = c("id", "depth")
)

# Process the geographic coordinates. They are stored in the '.geo' column in the following format:
# {""type"":""Point"",""coordinates"":[-53.794755636409,-29.651278496903196]}
# We need to extract the coordinates and create two new columns: 'lon' and 'lat'.
psd_model[, lon := as.numeric(sub(".*\\[([^,]+),.*", "\\1", .geo))]
psd_model[, lat := as.numeric(sub('.*\\[([^,]+),([0-9\\.-]+)\\].*', '\\2', .geo))]
# Remove the '.geo' column
psd_model[, .geo := NULL]

# Calculate average over groups, i.e. soil profiles based on the 'id' column
psd_model_avg <- psd_model[, .(
  clay = mean(clay, na.rm = TRUE),
  silt = mean(silt, na.rm = TRUE),
  sand = mean(sand, na.rm = TRUE),
  pred_clay = mean(pred_clay, na.rm = TRUE),
  pred_silt = mean(pred_silt, na.rm = TRUE),
  pred_sand = mean(pred_sand, na.rm = TRUE),
  lon = mean(lon, na.rm = TRUE),
  lat = mean(lat, na.rm = TRUE)
), by = id]

# Validation statistics - Overall ##################################################################
# Compute error statistics for the entire dataset using the custom function
# error_statistics(observed, predicted)
# observed = clay, silt, sand
# predicted = pred_clay, pred_silt, pred_sand
validation_stats_brazil <- rbind(
  clay = psd_model[, error_statistics(observed = clay, predicted = pred_clay)],
  silt = psd_model[, error_statistics(observed = silt, predicted = pred_silt)],
  sand = psd_model[, error_statistics(observed = sand, predicted = pred_sand)]
)
validation_stats_brazil <- round(validation_stats_brazil, 4)
# Print the validation statistics for Brazil
print("Validation statistics - Brazil - %")
print(validation_stats_brazil)

# Validation statistics - Groups ###################################################################
# Calculate error statistics for average values
validation_stats_brazil_avg <- rbind(
  clay = psd_model_avg[, error_statistics(observed = clay, predicted = pred_clay)],
  silt = psd_model_avg[, error_statistics(observed = silt, predicted = pred_silt)],
  sand = psd_model_avg[, error_statistics(observed = sand, predicted = pred_sand)]
)
validation_stats_brazil_avg <- round(validation_stats_brazil_avg, 4)
# Print the validation statistics for Brazil average
print("Validation statistics - Brazil average - %")
print(validation_stats_brazil_avg)

# library(geobr)
# maranhao <- geobr::read_state(code_state = "MA")
# maranhao <- st_transform(maranhao, crs = 4326)

# # transform psd_model to sf object
# library(sf)
# psd_model_sf <- st_as_sf(psd_model, coords = c("lon", "lat"), crs = 4326)
# # intersect with the map of Maranhão: I want to know which points are in Maranhão
# psd_model_ma <- st_intersection(psd_model_sf, maranhao)
# # Check the number of points in Maranhão
# nrow(psd_model_ma)
# psd_model_ma[, .N, by = id]

# # Get the ids of the points in Maranhão
# ids_ma <- unique(psd_model_ma$id)
# # Filter the original psd_model to keep only the points in Maranhão
# psd_model_ma <- psd_model[id %in% ids_ma]

# # Calculate error statistics for Maranhão
# validation_stats_ma <- rbind(
#   clay = psd_model_ma[, error_statistics(observed = clay, predicted = pred_clay)],
#   silt = psd_model_ma[, error_statistics(observed = silt, predicted = pred_silt)],
#   sand = psd_model_ma[, error_statistics(observed = sand, predicted = pred_sand)]
# )
# validation_stats_ma <- round(validation_stats_ma, 4)
# # Print the validation statistics for Maranhão
# print("Validation statistics - Maranhão - %")
# print(validation_stats_ma)

# # Calculate average over groups, i.e. soil profiles based on the 'id' column
# psd_model_ma_avg <- psd_model_ma[, .(
#   clay = mean(clay, na.rm = TRUE),
#   silt = mean(silt, na.rm = TRUE),
#   sand = mean(sand, na.rm = TRUE),
#   pred_clay = mean(pred_clay, na.rm = TRUE),
#   pred_silt = mean(pred_silt, na.rm = TRUE),
#   pred_sand = mean(pred_sand, na.rm = TRUE),
#   lon = mean(lon, na.rm = TRUE),
#   lat = mean(lat, na.rm = TRUE)
# ), by = id]

# # Calculate error statistics for average values in Maranhão
# validation_stats_ma_avg <- rbind(
#   clay = psd_model_ma_avg[, error_statistics(observed = clay, predicted = pred_clay)],
#   silt = psd_model_ma_avg[, error_statistics(observed = silt, predicted = pred_silt)],
#   sand = psd_model_ma_avg[, error_statistics(observed = sand, predicted = pred_sand)]
# )
# validation_stats_ma_avg <- round(validation_stats_ma_avg, 4)
# # Print the validation statistics for Maranhão average
# print("Validation statistics - Maranhão average - %")
# print(validation_stats_ma_avg)

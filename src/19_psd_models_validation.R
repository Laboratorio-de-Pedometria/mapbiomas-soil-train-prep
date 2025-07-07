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

# Merge the two data.tables by 'id' and 'depth'


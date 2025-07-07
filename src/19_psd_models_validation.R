# title: Soil Particle Size Distribution - Validation Statistics
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2025
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Read the data
dir_path <- path.expand("~/ownCloud/MapBiomas/res/tab/")

# log_clay_sand
# "log_clay_sand_000_010cm_cross_validation.txt"
# "log_clay_sand_010_020cm_cross_validation.txt"
# "log_clay_sand_020_030cm_cross_validation.txt"

# log_silt_sand
# "log_silt_sand_000_010cm_cross_validation.txt"
# "log_silt_sand_010_020cm_cross_validation.txt"
# "log_silt_sand_020_030cm_cross_validation.txt"

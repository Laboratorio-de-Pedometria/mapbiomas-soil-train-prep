# title: Modeling Soil Particle Size Distribution
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2025
rm(list = ls())

####################################################################################################
# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("ranger")) {
  install.packages("ranger")
}

# Source helper functions
source("src/00_helper_functions.r")

# Read data from disk
soildata_00_10cm <- data.table::fread("data/psd_c02beta_00_10cm_v2.csv")
summary_soildata(soildata_00_10cm)
# Layers: 29881
# Events: 15729
# Georeferenced events: 13381

# Set covariates
colnames(soildata_00_10cm)


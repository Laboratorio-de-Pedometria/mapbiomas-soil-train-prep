# title: MapBiomas Soil
# subtitle: 15. Merge external data
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025
rm(list = ls())

# Set MapBiomas Soil Collection
collection <- "c3"

# Source helper functions and packages
source("src/00_helper_functions.r")

if (!requireNamespace("BalancedSampling")) {
  install.packages("BalancedSampling")
  requireNamespace("BalancedSampling")
}

# SOILDATA #########################################################################################

# Read data from previous processing script
file_path <- "data/14_soildata.txt"
soildata <- data.table::fread(file_path, sep = "\t", na.strings = c("", "NA", "NaN"))
n_soildata <- length(unique(soildata[, id]))
summary_soildata(soildata)
# Layers: 53562
# Events: 18676
# Georeferenced events: 16170
# Datasets: 261

# EXTERNAL DATA ####################################################################################

# Pseudo-samples: beach, dune, and sandy spot
sand_folder <- "data/2025_10_23_pseudo_amostras_dunas_praias_areiais"
# List all SHP files in the folder
sand_files <- list.files(
  path = sand_folder,
  pattern = "\\.shp$",
  full.names = TRUE, recursive = TRUE
)
# Read and merge all SHP files
sand_samples <- lapply(sand_files, sf::st_read)
sand_samples <- do.call(rbind, sand_samples)

if (FALSE) {
  mapview::mapview(sand_samples)
}

# Select a random subset of the pseudo-samples. The number of samples selected is a fixed
# proportion of the number of georeferenced events in the soildata (n_soildata).
# Sampling is performed using a stratified sampling approach using the coordinates as strata.
# We will use a ballanced sampling approach to ensure that the samples are well distributed in space.
set.seed(1984)
sand_proportion <- 0.05
n_sand_samples <- round(sand_proportion * n_soildata)
prob <- rep(n_sand_samples / nrow(sand_samples), nrow(sand_samples))
sand_samples_idx <- BalancedSampling::lpm2(prob, sf::st_coordinates(sand_samples))
sand_samples_selected <- sand_samples[sand_samples_idx, ]
nrow(sand_samples_selected)
# 934

# Check spatial distribution of the selected samples
if (FALSE) {
  mapview::mapview(sand_samples) +
    mapview::mapview(sand_samples_selected, col.regions = "red")
}


sand_samples[, `:=`(
  dataset_id = "sand-pseudo-sample",
  id = paste0("sand-pseudo-sample-", .I),
  profund_sup = 0,
  profund_inf = 20,
  esqueleto = 0,
  argila = 0,
  silte = 0,
  areia = 1000
)]
summary_soildata(sand_samples)
# Layers: 2969
# Events: 2969
# Georeferenced events: 2969
# Datasets: 1
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
summary_soildata(soildata)
# Layers: 53562
# Events: 18676
# Georeferenced events: 16170
# Datasets: 261

# EXTERNAL DATA - BEACH, DUNE, AND SANDY SPOT PSEUDO-SAMPLES #######################################

# Read pseudo-samples of sandy soils from beaches, dunes, and sandy spots.
# These samples were created based on visual interpretation of satellite images
# (Google Earth/Maps) and represent locations with high likelihood of sandy soils.
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

# Select a random subset of the pseudo-samples. Sampling is performed using a stratified sampling
# approach using the coordinates as strata. We will use a ballanced sampling approach to ensure
# that the samples are well distributed in space.
set.seed(1984)
n_sand_samples <- 1000
prob <- rep(n_sand_samples / nrow(sand_samples), nrow(sand_samples))
sand_samples_idx <- BalancedSampling::lpm2(prob, sf::st_coordinates(sand_samples))
sand_samples_selected <- sand_samples[sand_samples_idx, ]
nrow(sand_samples_selected)
# 1000

# Check spatial distribution of the selected samples
if (FALSE) {
  mapview::mapview(sand_samples) +
    mapview::mapview(sand_samples_selected, col.regions = "red")
}

# Extract coordinates to a data.table
sand_samples_selected <- data.table::as.data.table(sf::st_coordinates(sand_samples_selected))

# Rename columns
# X and Y to coord_x and coord_y
data.table::setnames(sand_samples_selected, old = c("X", "Y"), new = c("coord_x", "coord_y"))

# Add columns to match the structure of "soildata"
sand_samples_selected[, `:=`(
  dataset_id = "sand-pseudo-sample",
  observacao_id = .I,
  id = paste0("sand-pseudo-sample-", .I),
  dataset_titulo = "Pseudo-sample from beach, dune, and sandy spot",
  organizacao_nome = "MapBiomas",
  dataset_licenca = "CC-BY 4.0",
  taxon_sibcs = "Neossolo Quartzarênico",
  coord_precisao = 30,
  coord_fonte = "Google Earth/Maps",
  coord_datum = 4326,
  pais_id = "BR",
  data_ano = 2023,
  amostra_quanti = 1,
  amostra_area = 10 * 10, # 5m x 5m area
  pedregosidade = "Ausente",
  rochosidade = "Ausente",
  camada_id = 1,
  camada_nome = "0-10",
  profund_sup = 0,
  profund_inf = 20,
  esqueleto = 0,
  terrafina = 1000,
  areia = 1000,
  silte = 0,
  argila = 0,
  ctc = NA_real_,
  ph_h2o = NA_real_,
  dsi = NA_real_
)]

# Replicate rows
sand_samples_selected <- sand_samples_selected[rep(1:.N, each = 5)]

# Update camada_id according to its order inside "id"
sand_samples_selected[, camada_id := 1:.N, by = id]

# Update depth intervals according to camada_id
sand_samples_selected[, profund_sup := profund_sup * camada_id]
sand_samples_selected[, profund_inf := profund_inf * camada_id]

# Update camada_nome according to profund_sup and profund_inf
sand_samples_selected[, camada_nome := paste0(profund_sup, "-", profund_inf)]

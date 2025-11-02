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

# Download Brazilian biome boundaries
# Check if the file already exists to avoid re-downloading
if (!file.exists("data/brazil_biomes.geojson")) {
  biome <- geobr::read_biomes(simplified = FALSE)
  biome <- sf::st_make_valid(biome)
  biome <- sf::st_transform(biome, crs = 4326)
  biome[biome$name_biome == "Sistema Costeiro", "code_biome"] <- 7
  # Save the data to a file for future use
  sf::st_write(biome, "data/brazil_biomes.geojson")
} else {
  biome <- sf::st_read("data/brazil_biomes.geojson")
}
print(biome)

# SOILDATA #########################################################################################

# Read data from previous processing script
file_path <- "data/14_soildata.txt"
soildata <- data.table::fread(file_path, sep = "\t", na.strings = c("", "NA", "NaN"))
summary_soildata(soildata)
# Layers: 54555
# Events: 18870
# Georeferenced events: 16360
# Datasets: 265

# EXTERNAL DATA - BEACH, DUNE, AND SANDY SPOT PSEUDO-SAMPLES #######################################

# Read pseudo-samples of sandy soils from beaches, dunes, and sandy spots.
# These samples were created based on visual interpretation of satellite images
# (Google Earth/Maps) and represent locations with high likelihood of sandy soils.
sand_folder <- "data/2025_10_23_pseudo_amostras_dunas_praias_areiais"
if (!dir.exists(sand_folder)) {
  stop(paste0("Folder not found: ", sand_folder))
} else {
  print(paste0("Reading pseudo-samples from folder: ", sand_folder))
  # List all SHP files in the folder
  sand_files <- list.files(
    path = sand_folder,
    pattern = "\\.shp$",
    full.names = TRUE, recursive = TRUE
  )
  # Read and merge all SHP files
  sand_samples_list <- lapply(sand_files, function(x) sf::st_geometry(sf::st_read(x, quiet = TRUE)))
  sand_samples <- do.call(c, sand_samples_list)
}
# Geometry set to Simple Features Collection
sand_samples <- sf::st_as_sf(sand_samples)
if (interactive()) {
  mapview::mapview(sand_samples)
}

# Intersect Brazilian biomes at the locations of the sandy soil pseudo-samples
old_s2 <- sf::sf_use_s2()
sf::sf_use_s2(FALSE)
sand_samples <- sf::st_intersection(sand_samples, biome)
sf::sf_use_s2(old_s2)

# Select a random subset of the pseudo-samples. Sampling is performed using a balanced sampling
# approach using 1) the coordinates and 2) the biome as strata. We will use a balanced sampling
# approach to ensure that the samples are well distributed in space and in biome.
set.seed(1984)
n_sand_samples <- 500
# Use length() for sfc objects (geometry sets)
prob_sand <- rep(n_sand_samples / nrow(sand_samples), nrow(sand_samples))
# Prepare balancing variables: coordinates + biome code
# lpm2 needs a matrix where each column is a balancing variable
coords <- sf::st_coordinates(sand_samples)
biome_numeric <- as.numeric(as.factor(sand_samples$code_biome))
# Combine into matrix: X, Y, and biome as numeric
balance_vars <- cbind(coords, biome = biome_numeric)
# Balanced sampling
sand_samples_idx <- BalancedSampling::lpm2(prob_sand, balance_vars)
sand_samples_selected <- sand_samples[sand_samples_idx, ]
nrow(sand_samples_selected)
# 500

# Check spatial distribution of the selected samples
if (interactive()) {
  mapview::mapview(sand_samples) +
    mapview::mapview(sand_samples_selected, col.regions = "red", cex = 4)
}
# Extract coordinates to a data.table
sand_samples_selected <- data.table::as.data.table(sf::st_coordinates(sand_samples_selected))
# Rename columns
# X and Y to coord_x and coord_y
data.table::setnames(sand_samples_selected, old = c("X", "Y"), new = c("coord_x", "coord_y"))
# Add columns to match the structure of "soildata"
sand_samples_selected[, `:=`(
  dataset_id = "sand-pseudo",
  observacao_id = .I,
  id = paste0("sand-pseudo-", .I),
  dataset_titulo = "Pseudo-sample from beach, dune, and sandy spot",
  organizacao_nome = "MapBiomas",
  dataset_licenca = "CC-BY 4.0",
  coord_precisao = 30,
  coord_fonte = "Google Earth/Maps",
  coord_datum = 4326,
  pais_id = "BR",
  data_ano = 2023,
  amostra_quanti = 1,
  amostra_area = 10 * 10, # 10m x 10m area
  camada_id = 1,
  camada_nome = "C",
  profund_sup = 0,
  profund_inf = 10,
  esqueleto = 0,
  terrafina = 1000,
  areia = 1000,
  silte = 0,
  argila = 0,
  carbono = 0,
  ctc = NA_real_,
  ph_h2o = NA_real_,
  dsi = NA_real_
)]

# Replicate rows
sand_samples_selected <- sand_samples_selected[rep(1:.N, each = 10)]
# Update camada_id according to its order inside "id"
sand_samples_selected[, camada_id := 1:.N, by = id]
sand_samples_selected[1:20, .(id, profund_sup, profund_inf, camada_id)]

# Update depth intervals according to camada_id
sand_samples_selected[, profund_inf := profund_inf * camada_id]
sand_samples_selected[, profund_sup := profund_inf - 10]

# Update "camada_nome" appending "camada_id" to "camada_nome"
sand_samples_selected[, camada_nome := paste0(camada_nome, camada_id)]
print(sand_samples_selected[1:20, .(id, profund_sup, profund_inf, camada_id, camada_nome)])

# EXTERNAL DATA - ROCKY OUTCROP PSEUDO-SAMPLES #####################################################
# Read pseudo-samples of rocky outcrops.
# These samples were created based on visual interpretation of satellite images
# (Google Earth/Maps) and represent locations with high likelihood of rocky outcrops.
rock_folder <- "data/2025_10_23_pseudo_amostras_aloramento_rochoso"
if (!dir.exists(rock_folder)) {
  stop(paste("Folder not found:", rock_folder))
} else {
  print(paste("Reading pseudo-samples from folder:", rock_folder))
  # List all SHP files in the folder_path
  rock_files <- list.files(
    path = rock_folder,
    pattern = "\\.shp$",
    full.names = TRUE, recursive = TRUE
  )
  # Read and merge all SHP files
  # Temporarily disable s2 processing to handle potential invalid geometries
  sf::sf_use_s2(FALSE)
  rock_samples_list <- lapply(rock_files, function(x) {
    geom <- sf::st_geometry(sf::st_read(x, quiet = TRUE))
    # Check if the geometry is a polygon/multipolygon and return the centroid if so
    if (all(sf::st_geometry_type(geom) %in% c("POLYGON", "MULTIPOLYGON"))) {
      # Repair invalid geometries before calculating the centroid
      geom <- sf::st_centroid(sf::st_make_valid(geom))
    }
    return(geom)
  })
  rock_samples <- do.call(c, rock_samples_list)
  # Re-enable s2 processing
  sf::sf_use_s2(TRUE)
}
rock_samples <- sf::st_as_sf(rock_samples)
if (interactive()) {
  mapview::mapview(rock_samples)
}
# 12421

# Intersect Brazilian biomes at the locations of the rocky soil pseudo-samples
old_s2 <- sf::sf_use_s2()
sf::sf_use_s2(FALSE)
rock_samples <- sf::st_intersection(rock_samples, biome)
sf::sf_use_s2(old_s2)

# Select a random subset of the pseudo-samples. Sampling is performed using a balanced sampling
# approach using 1) the coordinates and 2) the biome as strata. We will use a balanced sampling
# approach to ensure that the samples are well distributed in space and in biome.
set.seed(1984)
n_rock_samples <- 500
# Use length() for sfc objects (geometry sets)
prob_rock <- rep(n_rock_samples / nrow(rock_samples), nrow(rock_samples))
# Prepare balancing variables: coordinates + biome code
# lpm2 needs a matrix where each column is a balancing variable
coords <- sf::st_coordinates(rock_samples)
biome_numeric <- as.numeric(as.factor(rock_samples$code_biome))
# Balanced sampling
rock_samples_idx <- BalancedSampling::lpm2(prob_rock, cbind(coords, biome = biome_numeric))
rock_samples_selected <- rock_samples[rock_samples_idx, ]
length(rock_samples_selected)
# 500
rm(rock_samples_idx, prob_rock, biome_numeric, coords)

# Check spatial distribution of the selected samples
if (interactive()) {
  mapview::mapview(rock_samples) +
    mapview::mapview(rock_samples_selected, col.regions = "red", cex = 4)
}

# Extract coordinates to a data.table
rock_samples_selected <- data.table::as.data.table(sf::st_coordinates(rock_samples_selected))

# Rename columns
# X and Y to coord_x and coord_y
data.table::setnames(rock_samples_selected, old = c("X", "Y"), new = c("coord_x", "coord_y"))

# Add columns to match the structure of "soildata"
rock_samples_selected[, `:=`(
  dataset_id = "rock-pseudo",
  observacao_id = .I,
  id = paste0("rock-pseudo-", .I),
  dataset_titulo = "Pseudo-sample from rocky outcrop",
  organizacao_nome = "MapBiomas",
  dataset_licenca = "CC-BY 4.0",
  coord_precisao = 30,
  coord_fonte = "Google Earth/Maps",
  coord_datum = 4326,
  pais_id = "BR",
  data_ano = 2023,
  amostra_quanti = 1,
  amostra_area = 10 * 10, # 10m x 10m area
  camada_nome = "R",
  camada_id = 1,
  profund_sup = 0,
  profund_inf = 10,
  esqueleto = 1000,
  terrafina = 0,
  areia = 0,
  silte = 0,
  argila = 0,
  carbono = 0,
  ctc = NA_real_,
  ph_h2o = NA_real_,
  dsi = NA_real_
)]

# Replicate rows
rock_samples_selected <- rock_samples_selected[rep(1:.N, each = 10)]

# Update camada_id according to its order inside "id"
rock_samples_selected[, camada_id := 1:.N, by = id]
# Update depth intervals according to camada_id
rock_samples_selected[, profund_inf := profund_inf * camada_id]
rock_samples_selected[, profund_sup := profund_inf - 10]
# Update "camada_nome" appending "camada_id" to "camada_nome"
rock_samples_selected[, camada_nome := paste0(camada_nome, camada_id)]

# MERGE EXTERNAL DATA INTO SOILDATA ################################################################
# Merge the pseudo-samples into the main soildata data.table
soildata <- data.table::rbindlist(
  list(soildata, sand_samples_selected, rock_samples_selected),
  use.names = TRUE, fill = TRUE
)
summary_soildata(soildata)
# Layers: 64555
# Events: 19870
# Georeferenced events: 17360
# Datasets: 267
# Save the merged soildata to a file
file_path <- "data/15_soildata.txt"
data.table::fwrite(soildata, file_path, sep = "\t")

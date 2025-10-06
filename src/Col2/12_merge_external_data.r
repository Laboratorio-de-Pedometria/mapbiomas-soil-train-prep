####################################################################################################
# ATTENTION: THIS SCRIPT WAS MOVED TO SOILDATA-INTEGRATION ON 2024-05-25
# https://github.com/Laboratorio-de-Pedometria/SoilData-integration
####################################################################################################
# title: SoilData - Soil Organic Carbon Stock
# subtitle: Merge external data
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("sf")) {
  install.packages("sf")
}
if (!require("geobr")) {
  install.packages("geobr")
}

# Source helper functions
source("src/00_helper_functions.r")

brazil <- geobr::read_country()

# Rename columns following previous standards
rename <- c(
  "dados_id_febr",                "dataset_id",
  "evento_id_febr",               "id",
  "camada_id_febr",               "camada_id",
  "coord_longitude",              "coord_x",
  "coord_latitude",               "coord_y",
  "coord_estado_sigla",           "estado_id",
  "ph_h2o_25_eletrodo",           "ph",
  "ph_h2o",                       "ph",
  "ctc_soma_calc",                "ctc",
  "carbono_forno_1min950_cgdct",  "carbono",
  "argila_sodio_pipeta",          "argila",
  "densidade_solo_cilindro",      "dsi",
  "sibcs_20xx",                   "taxon_sibcs"
)
rename <- matrix(rename, ncol = 2, byrow = TRUE)

# Load external data sets
# Events
files_event <- list.files(
  path = path.expand("~/ownCloud/febr-repo/processamento"),
  pattern = "-evento.txt$",
  full.names = TRUE, recursive = TRUE
)
length(files_event) # 7 data sets
print(files_event)
data_event <- list()
for (i in seq_along(files_event)) {
  data_event[[i]] <- data.table::fread(files_event[i], dec = ",")
  id <- rev(strsplit(files_event[i], split = "/")[[1]])[1]
  id <- strsplit(id, "-")[[1]][1]
  data_event[[i]][, dados_id_febr := id]
  data.table::setnames(data_event[[i]], old = rename[, 1], new = rename[, 2], skip_absent = TRUE)
}
data_event <- data.table::rbindlist(data_event, fill = TRUE)
nrow(data_event) # 1662 events

# Standardize coordinate reference system
target_crs <- 4326
data_event[, coord_datum_epsg := as.integer(gsub("EPSG:", "", coord_datum_epsg))]
sf_data_event <- split(data_event, data_event[, coord_datum_epsg])
idx_transform <- which(names(sf_data_event) != target_crs)
for (i in seq_along(sf_data_event)) {
  if (i %in% idx_transform) {
    crs <- as.integer(names(sf_data_event[i]))
    sf_data_event[[i]] <- sf::st_as_sf(
      sf_data_event[[i]],
      coords = c("coord_x", "coord_y"), crs = crs
    )
    sf_data_event[[i]] <- sf::st_transform(sf_data_event[[i]], crs = target_crs)
  } else {
    sf_data_event[[i]] <- sf::st_as_sf(sf_data_event[[i]],
      coords = c("coord_x", "coord_y"),
      crs = target_crs
    )
  }
}
data_event <- do.call(rbind, sf_data_event)
data_event <- cbind(sf::st_coordinates(data_event), as.data.frame(data_event))
data_event <- data.table::as.data.table(data_event)
data_event[coord_datum_epsg != target_crs & !is.na(coord_datum_epsg), coord_datum_epsg := target_crs]
data.table::setnames(data_event, old = c("X", "Y"), new = c("coord_x", "coord_y"))
data_event[, geometry := NULL]
summary_soildata(data_event)
# Layers: 1662
# Events: 1662
# Georeferenced events: 1662

# Clean sampling date (just to make sure)
data_event[data_coleta_ano < 1950, data_coleta_ano := NA_integer_]
data_event[data_coleta_ano > as.integer(format(Sys.time(), "%Y")), data_coleta_ano := NA_integer_]
summary_soildata(data_event)
# Layers: 1662
# Events: 1662
# Georeferenced events: 1662

# Layers
files_layer <- list.files(
  path = path.expand("~/ownCloud/febr-repo/processamento"),
  pattern = "-camada.txt$",
  full.names = TRUE, recursive = TRUE
)
length(files_layer) # 7 data sets
print(files_layer)
data_layer <- list()
for (i in seq_along(files_layer)) {
  data_layer[[i]] <- data.table::fread(files_layer[i], dec = ",")
  id <- rev(strsplit(files_layer[i], split = "/")[[1]])[1]
  id <- strsplit(id, "-")[[1]][1]
  data_layer[[i]][, dados_id_febr := id]
  data.table::setnames(data_layer[[i]], old = rename[, 1], new = rename[, 2], skip_absent = TRUE)
}
data_layer <- data.table::rbindlist(data_layer, fill = TRUE)
data_layer[, camada_nome := camada_id]
nrow(data_layer) # 2134 layers

# Merge data from events and layers
soildata_01 <- merge(data_event, data_layer, by = c("dataset_id", "id"))
colnames(soildata_01)
if (!"terrafina" %in% colnames(soildata_01)) {
  soildata_01[, terrafina := NA_real_]
}
# if (!"camada_nome" %in% colnames(soildata_01)) {
#   soildata_01[, camada_nome := NA_character_]
# }
summary_soildata(soildata_01)
# Layers: 1941
# Events: 1051
# Georeferenced events: 1051

# Read SoilData data processed in the previous scripts
soildata_02 <- data.table::fread("data/11_soildata_soc.txt", sep = "\t")
if (!"coord_datum_epsg" %in% colnames(soildata_02)) {
  soildata_02[, coord_datum_epsg := 4326]
}
if (FALSE) {
  x11()
  plot(brazil, reset = FALSE, main = "")
  points(soildata_02[, coord_x], soildata_02[, coord_y], cex = 0.5, pch = 20)
  points(soildata_01[, coord_x], soildata_01[, coord_y], cex = 0.5, pch = 20, col = "red")
}

# Merge SoilData data with external data
soildata_01[, observacao_id := id]
soildata_01[, id := paste0(dataset_id, "-", id)]
soildata <- rbind(soildata_01, soildata_02, fill = TRUE)
# The following code is deleterious: it removed key columns from the second data set
# idx <- match(colnames(soildata_02), colnames(soildata_01))
# idx01 <- na.exclude(idx)
# idx02 <- which(!is.na(idx))
# soildata <- rbind(soildata_02[, ..idx02], soildata_01[, ..idx01])
summary_soildata(soildata)
# Layers: 52260
# Events: 15175
# Georeferenced events: 12045

# Write data to disk
data.table::fwrite(soildata, "data/12_soildata_soc.txt", sep = "\t")

####################################################################################################
# ATTENTION: THIS SCRIPT WAS MOVED TO SOILDATA-INTEGRATION ON 2024-05-03
# https://github.com/Laboratorio-de-Pedometria/SoilData-integration
####################################################################################################
# title: SoilData - Soil Organic Carbon Stock
# subtitle: Merge curated data
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Source helper functions
source("src/00_helper_functions.r")

# Read curated data
read_cols <- c(
  "dataset_id", "observacao_id",
  "data_ano",
  "coord_x", "coord_y", "coord_precisao", "coord_fonte", "coord_datum",
  "pais_id", "estado_id", "municipio_id",
  "amostra_area",
  "taxon_sibcs",
  "camada_nome", "camada_id", "amostra_id",
  "profund_sup", "profund_inf",
  "carbono", "ctc", "ph", "argila", "silte", "areia", "terrafina", "dsi"
)
curated_path <- list.files(
  path = path.expand("~/ownCloud/SoilData"),
  pattern = "^ctb[0-9]{4}\\.csv$",
  full.names = TRUE, recursive = TRUE
)
length(curated_path) # 28 datasets
print(curated_path)

# Read all files and store them in a list
curated_list <- lapply(curated_path, function(x) {
  data.table::fread(x)
})

# rbind all datasets keeping only the matching columns
curated_data <- data.table::rbindlist(curated_list, fill = TRUE)
curated_data <- curated_data[, ..read_cols]
curated_data[, id := paste0(dataset_id, "-", observacao_id)]
summary_soildata(curated_data)
# Layers: 9818
# Events: 3654
# Georeferenced events: 3357

# Read SoilData data processed in the previous script
soildata <- data.table::fread("data/12_soildata_soc.txt", sep = "\t")
summary_soildata(soildata)
# Layers: 52260
# Events: 15175
# Georeferenced events: 12045

# Identify datasets missing the carbon content
# na_carbon <- soildata[is.na(carbono), .N, by = dataset_id]
# na_carbon[N > 50]
# ctb <- "ctb0039"
# soildata[dataset_id == ctb, .(dataset_id, observacao_id, camada_nome, carbono)]

# Merge curated data with SoilData
data.table::setnames(soildata, old = "data_coleta_ano", new = "data_ano")
data.table::setnames(soildata, old = "coord_datum_epsg", new = "coord_datum")
curated_ctb <- curated_data[, unique(dataset_id)]
soildata <- soildata[!dataset_id %in% curated_ctb]
summary_soildata(soildata)
# Layers: 51044
# Events: 14761
# Georeferenced events: 11633
soildata <- rbind(soildata, curated_data, fill = TRUE)
summary_soildata(soildata)
# Layers: 60862
# Events: 18415
# Georeferenced events: 14990

# Check spatial distribution
if (FALSE) {
  x11()
  soildata_sf <- soildata[!is.na(coord_x) & !is.na(coord_y)]
  soildata_sf <- sf::st_as_sf(soildata_sf, coords = c("coord_x", "coord_y"), crs = 4326)
  plot(soildata_sf["estado_id"], cex = 0.5)
}

####################################################################################################
# Export cleaned data
# Number of datasets
length(soildata[, unique(dataset_id)])
# 262 datasets
data.table::fwrite(soildata, "data/13_soildata_soc.txt", sep = "\t")

# title: MapBiomas Soil
# subtitle: Merge curated data
# author: Alessandro Samuel-Rosa
# data: 2025 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Source helper functions
source("src/00_helper_functions.r")

# Curated data #####################################################################################
# Path to data curation repository
curated_path <- "~/projects/SoilData/SoilData-ctb"

# List all curated data files
curated_files <- list.files(
  path = path.expand(curated_path),
  pattern = "^ctb[0-9]{4}\\.csv$",
  full.names = TRUE, recursive = TRUE
)
length(curated_files) # 28 datasets
print(curated_files)

# Set of columns to read from curated data
read_cols <- c(
  "dataset_id", "observacao_id",
  "data_ano", "ano_fonte",
  "coord_x", "coord_y", "coord_precisao", "coord_fonte", "coord_datum",
  "pais_id", "estado_id", "municipio_id",
  "amostra_area",
  "taxon_sibcs",
  "camada_nome", "camada_id", "amostra_id",
  "profund_sup", "profund_inf",
  "carbono", "ctc", "ph", "argila", "silte", "areia", "terrafina", "dsi"
)

# If length(curated_files) is larger than 0, read all files and store them in a list
if (length(curated_files) > 0) {
  curated_list <- lapply(curated_files, data.table::fread)
  # rbind all datasets keeping only the matching columns
  curated_data <- data.table::rbindlist(curated_list, fill = TRUE)
  curated_data <- curated_data[, ..read_cols]
  curated_data[, id := paste0(dataset_id, "-", observacao_id)]
} else {
  stop("No curated files found")
}
summary_soildata(curated_data)
# Layers:
# Events:
# Georeferenced events:

# Read the latest Brazilian Soil Dataset ###########################################################
# Check if "data/00_brazilian_soil_dataset_2024.txt" exists. If not, read the Brazilian
# Soil Dataset 2024 using the 'dataverse' package. Next, write it to
# 'data/00_brazilian_soil_dataset_2024.txt'. If the file already exists, read it using the
# 'data.table' package.
file_path <- "data/00_brazilian_soil_dataset_2024.txt"
doi <- "10.60502/SoilData/BCAV2B"
if (!file.exists(file_path)) {
  br_soil2024 <- dataverse::get_dataframe_by_name("brazilian-soil-dataset-2024.txt",
    server = "https://soildata.mapbiomas.org/dataverse/soildata",
    dataset = doi, .f = data.table::fread
  )
  data.table::fwrite(br_soil2024, file_path, dec = ".", sep = ";")
} else {
  br_soil2024 <- data.table::fread(file_path, dec = ".", sep = ";")
}
summary_soildata(br_soil2024)
# Layers: 57077
# Events: 16824
# Georeferenced events: 14334
# Datasets: 255

# # Merge curated data with SoilData
# data.table::setnames(soildata, old = "data_coleta_ano", new = "data_ano")
# data.table::setnames(soildata, old = "coord_datum_epsg", new = "coord_datum")
# curated_ctb <- curated_data[, unique(dataset_id)]
# soildata <- soildata[!dataset_id %in% curated_ctb]
# summary_soildata(soildata)
# # Layers: 51044
# # Events: 14761
# # Georeferenced events: 11633
# soildata <- rbind(soildata, curated_data, fill = TRUE)
# summary_soildata(soildata)
# # Layers: 60862
# # Events: 18415
# # Georeferenced events: 14990

# Check spatial distribution
if (FALSE) {
  x11()
  soildata_sf <- br_soil2024[!is.na(coord_x) & !is.na(coord_y)]
  soildata_sf <- sf::st_as_sf(soildata_sf, coords = c("coord_x", "coord_y"), crs = 4326)
  plot(soildata_sf["estado_id"], cex = 0.5)
}

br_soil2024[, id := paste0(dataset_id, "-", observacao_id)]

# Export cleaned data ##############################################################################
summary_soildata(br_soil2024)
data.table::fwrite(br_soil2024, "data/10_soildata.txt", sep = "\t")

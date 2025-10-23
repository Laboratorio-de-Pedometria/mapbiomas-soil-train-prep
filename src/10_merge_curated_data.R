# title: MapBiomas Soil
# subtitle: Merge curated data
# author: Alessandro Samuel-Rosa
# data: 2025 CC-BY
rm(list = ls())

# Install and load required packages
if (!requireNamespace("data.table")) {
  install.packages("data.table")
}

# Auxiliary data and functions #####################################################################
# Source helper functions
source("src/00_helper_functions.r")

# Download Brazilian state boundaries
# Check if the file already exists to avoid re-downloading
if (!file.exists("data/brazil_states.geojson")) {
  brazil <- geobr::read_state(simplified = FALSE)
  # Save the data to a file for future use
  sf::st_write(brazil, "data/brazil_states.geojson")
} else {
  brazil <- sf::st_read("data/brazil_states.geojson")
}

# Curated data #####################################################################################
# Path to data curation repository
curated_path <- "~/projects/SoilData/SoilData-ctb"

# List all curated data files
curated_files <- list.files(
  path = path.expand(curated_path),
  pattern = "^ctb[0-9]{4}\\.csv$",
  full.names = TRUE, recursive = TRUE
)
length(curated_files) # 35 datasets
print(curated_files)

# If length(curated_files) is larger than 0, read all files and store them in a list
if (length(curated_files) > 0) {
  curated_list <- lapply(curated_files, data.table::fread)
  curated_data <- data.table::rbindlist(curated_list, fill = TRUE)
  curated_data[, id := paste0(dataset_id, "-", observacao_id)]
} else {
  warning("No curated files found")
}
summary_soildata(curated_data)
# Layers: 10926
# Events: 3995
# Georeferenced events: 3557
# Datasets: 35

# Error handling: Check for projected coordinates, printing the dataset_id of the rows with
# projected coordinates
proj_coords <- curated_data[!is.na(coord_x) & !is.na(coord_y) &
  (coord_x < -180 | coord_x > 180 | coord_y < -90 | coord_y > 90), ]
proj_coords <- unique(proj_coords$dataset_id)
if (length(proj_coords) > 0) {
  warning(
    "Projected coordinates found in the following dataset_id(s):\n",
    paste(unique(proj_coords), collapse = ", ")
  )
}
# Remove rows with projected coordinates from curated_data
curated_data <- curated_data[!dataset_id %in% proj_coords]
summary_soildata(curated_data)
# Layers: 10926
# Events: 3995
# Georeferenced events: 3557
# Datasets: 35

# Error handling: Check for points falling outside Brazil, printing the dataset_id of the rows
# with such points
curated_data_sf <- sf::st_as_sf(
  curated_data[!is.na(coord_x) & !is.na(coord_y)],
  coords = c("coord_x", "coord_y"), crs = 4674
)
curated_data_sf <- sf::st_join(curated_data_sf, brazil["abbrev_state"], left = TRUE)
outside_brazil <- curated_data_sf[is.na(curated_data_sf$abbrev_state), ]
outside_brazil <- unique(outside_brazil$dataset_id)
if (length(outside_brazil) > 0) {
  warning(
    "Points falling outside Brazil found in the following dataset_id(s):\n",
    paste(unique(outside_brazil), collapse = ", ")
  )
} else {
  message("No points falling outside Brazil found.")
}
# Remove rows with points falling outside Brazil from curated_data
curated_data <- curated_data[!dataset_id %in% outside_brazil]
summary_soildata(curated_data)
# Layers: 10926
# Events: 3995
# Georeferenced events: 3557
# Datasets: 35

# Brazilian Soil Dataset 2024 ######################################################################
# Check if "data/00_brazilian_soil_dataset_2024.txt" exists. If not, read the Brazilian Soil Dataset
# 2024 using the 'dataverse' package and write it to 'data/00_brazilian_soil_dataset_2024.txt'.
# If the file already exists, retrieve its creation date and compare it with the creation date of
# the dataset in Dataverse. If the local file is older than the dataset in Dataverse, re-download it.
# Otherwise, read the local file using the 'data.table' package.
file_path <- "data/00_brazilian_soil_dataset_2024.txt"
doi <- "10.60502/SoilData/BCAV2B"
download_data <- TRUE
if (file.exists(file_path)) {
  # Get the creation date of the local file
  file_info <- file.info(file_path)
  local_file_date <- as.Date(file_info$mtime)
  message("Local file creation date: ", local_file_date)

  # Get the creation date of the dataset in Dataverse
  dataset <- dataverse::get_dataset(
    dataset = doi,
    server = "https://soildata.mapbiomas.org/dataverse/soildata"
  )
  remote_file_date <- as.Date(dataset$files$creationDate)
  message("Remote file creation date: ", remote_file_date)

  # Download only if the remote file is newer
  if (local_file_date >= remote_file_date) {
    download_data <- FALSE
    message("Local file is up-to-date. No need to download.")
  }
}
if (download_data) {
  br_soil2024 <- dataverse::get_dataframe_by_name("brazilian-soil-dataset-2024.txt",
    server = "https://soildata.mapbiomas.org/dataverse/soildata",
    dataset = doi, .f = data.table::fread
  )
  data.table::fwrite(br_soil2024, file_path, dec = ".", sep = ";")
  message("File downloaded and saved to ", file_path)
} else {
  br_soil2024 <- data.table::fread(file_path, dec = ".", sep = ";")
}
br_soil2024[, id := paste0(dataset_id, "-", observacao_id)]
summary_soildata(br_soil2024)
# Layers: 57077
# Events: 16824
# Georeferenced events: 14334
# Datasets: 255

# Merge ############################################################################################
# Merge curated data with SoilData
curated_ctb <- curated_data[, unique(dataset_id)]
br_soil2024 <- br_soil2024[!dataset_id %in% curated_ctb]
summary_soildata(br_soil2024)
# Layers: 47792
# Events: 13566
# Georeferenced events: 11117
# Datasets: 226
# Merge datasets, keeping all columns of both datasets
soildata <- data.table::rbindlist(list(br_soil2024, curated_data), fill = TRUE)
summary_soildata(soildata)
# Layers: 58718
# Events: 17561
# Georeferenced events: 14674
# Datasets: 261

# Check spatial distribution
if (FALSE) {
  x11()
  soildata_sf <- soildata[!is.na(coord_x) & !is.na(coord_y)]
  soildata_sf <- sf::st_as_sf(soildata_sf, coords = c("coord_x", "coord_y"), crs = 4326)
  plot(soildata_sf["estado_id"], cex = 0.5, key.pos = 1, key.length = 1)
}

# Export data ######################################################################################
summary_soildata(soildata)
# Layers: 58719
# Events: 17561
# Georeferenced events: 14674
# Datasets: 261
data.table::fwrite(soildata, "data/10_soildata.txt", sep = "\t")

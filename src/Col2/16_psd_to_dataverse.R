# title: Publish processed data to SoilData (Dataverse)
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2024 CC-BY
rm(list = ls())

####################################################################################################
# Particle size distribution (0-30 cm), percentage
json_file <- "data/soil-particle-size-distribution-metadata.json"

# Source helper functions
source("src/00_helper_functions.r")

# Read data processed in the previous script
folder_path <- "~/Insync/MapBiomas Solo/Trainning samples/"
file_name <- "-clay-silt-sand-log-ratio.csv"
# List existing files in the folder_path and get the last one. Then read it.
existing_files <- list.files(path = folder_path, pattern = file_name)
last_file <- existing_files[length(existing_files)]
soildata <- data.table::fread(paste0(folder_path, last_file))
summary_soildata(soildata)
# Layers: 19965
# Events: 11633
# Georeferenced events: 11633

# Rename columns
# id: point_id
# coord_x: longitude
# coord_y: latitude
data.table::setnames(soildata,
 c("id", "coord_x", "coord_y"), c("point_id", "longitude", "latitude"))
# Save data to a temporary CSV
file_name <- "tmp/soil-particle-size-distribution-0-30-cm-percentage.csv"
data.table::fwrite(soildata, file_name)

# Submit data to Dataverse
# Check if DATAVERSE_KEY is set
api_key <- Sys.getenv("DATAVERSE_KEY")
if (api_key == "") {
 stop("Please set the environmental variable DATAVERSE_KEY")
} else {
 message("API key found")
}
# Set variables
server_url <- "https://soildata.mapbiomas.org"
parent <- "soildata"
# Create command to submit data
cmd <- paste0(
 "curl -H 'X-Dataverse-key:", api_key,
 "' -X POST '", server_url, "/api/dataverses/", parent,
 "/datasets' --upload-file '", json_file, "' -H 'Content-type:application/json'"
)
if (FALSE) {
  # ATTENTION: This command will publish the dataset
  message("Publishing new dataset")
  system(cmd)
}

# Add file to dataset
persistent_id <- "doi:10.60502/SoilData/P6R332"
# Create command to add file to dataset
cmd <- paste0(
 "curl -H 'X-Dataverse-key:", api_key,
 "' -X POST '", server_url, "/api/datasets/:persistentId/add?persistentId=",
 persistent_id, "' -F 'file=@", file_name, "' -F 'jsonData={\"description\":\"Particle size distribution (0-30 cm), percentage\"}'"
)
if (FALSE) {
  # ATTENTION: This command will add the file to the dataset
  message("Adding file to dataset")
  system(cmd)
}

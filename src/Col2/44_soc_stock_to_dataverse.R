# title: Publish processed data to SoilData (Dataverse)
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2024 CC-BY
rm(list = ls())

####################################################################################################
# Soil organic carbon stock (0-30 cm), grams per square meter
# data/soil-organic-carbon-stock-metadata.json

# Source helper functions
source("src/00_helper_functions.r")

# Read data processed in the previous script
soildata <- data.table::fread("data/40_soildata_soc.txt")
summary_soildata(soildata)
# Layers: 12666
# Events: 12666
# Georeferenced events: 12666

# Remove replicates
# id := paste0(id, "-REP", 1:4)
soildata <- soildata[!grepl("-XYREP[1-4]", id)]
summary_soildata(soildata)
# Layers: 12574
# Events: 12574
# Georeferenced events: 12574

# Check date range (ignore values equal to 1948)
soildata[year == 1948, year := NA_integer_]
soildata[, range(year, na.rm = TRUE)]

# Rename columns
# id: point_id
# coord_x: longitude
# coord_y: latitude
data.table::setnames(soildata,
 c("id", "coord_x", "coord_y"), c("point_id", "longitude", "latitude"))
# Save data to a temporary CSV
file_name <- "tmp/soil-organic-carbon-stock-0-30-cm-grams-per-square-meter.csv"
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
json_file <- "data/soil-organic-carbon-stock-metadata.json"
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
persistent_id <- "doi:10.60502/SoilData/SXCSDK"
# Create command to add file to dataset
cmd <- paste0(
 "curl -H 'X-Dataverse-key:", api_key,
 "' -X POST '", server_url, "/api/datasets/:persistentId/add?persistentId=",
 persistent_id, "' -F 'file=@", file_name, "' -F 'jsonData={\"description\":\"Soil organic carbon stock (0-30 cm), grams per square meter\"}'"
)
if (FALSE) {
  # ATTENTION: This command will add the file to the dataset
  message("Adding file to dataset")
  system(cmd)
}

# title: Compute SoilData statistics
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2025 CC-BY
# summary: This script accesses the SoilData repository (Dataverse) and retrieves the DOI and title
# of all datasets that were used to generate the PSD and SOC data. The list is used to prepare the
# soil data report.
rm(list = ls())

####################################################################################################
# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("dataverse")) {
  install.packages("dataverse")
}

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

# Get SoilData datasets with CTB
soil_datasets <- dataverse::dataverse_contents(dataverse = parent, server = server_url, key = api_key)
# Loop-over datasets and get citation metadata
soildata_ctb <- data.frame(ctb = NA_character_, title = NA_character_, url = NA_character_)
for (i in seq_along(soil_datasets)) {
  doi <- soil_datasets[[i]]$persistentUrl
  cat(doi)
  # Attempt to retrieve the dataset metadata with error handling
  metadata <- tryCatch(
    {
      dataverse::dataset_metadata(soil_datasets[[i]], server = server_url, key = api_key)
    },
    error = function(e) {
      cat("Error retrieving dataset metadata for ID:", i, "\n")
      cat("Error message:", e$message, "\n")
      return(NULL) # Return NULL in case of error
    }
  )
  # Check if metadata was successfully retrieved
  if (is.null(metadata)) {
    cat(": Skipping dataset ID:", i, "due to error.\n")
  } else if ("otherId" %in% metadata$fields$typeName) {
    otherId_idx <- which(metadata$fields$typeName == "otherId")
    title_idx <- which(metadata$fields$typeName == "title")
    if (metadata$fields$value[otherId_idx][[1]]$otherIdAgency$value %in% c("febr", "FEBR")) {
      ctb <- metadata$fields$value[otherId_idx][[1]]$otherIdValue$value
      soildata_ctb[i, ] <- c(ctb, metadata$fields$value[title_idx][[1]], doi)
      cat(":", ctb, "\n")
    }
    } else {
      cat(": Skipping dataset ID:", i, "due to missing 'otherId' field.\n")
    }
  }
soildata_ctb <- data.table(soildata_ctb)
# Drop rows with missing data
soildata_ctb <- soildata_ctb[!is.na(ctb)]

# Read PSD statistics computed in a previous script
file_path <- "res/tab/psd_ctb_data_per_biome.txt"
ctb_psd <- data.table::fread(file_path)

# Read SOC statistics computed in a previous script
file_path <- "res/tab/soc_ctb_data_per_biome.txt"
ctb_soc <- data.table::fread(file_path)

# Merge
ctb <- merge(ctb_psd[, "id"], ctb_soc[, "id"], all = TRUE)

# Keep only unique 'id' and drop remaning columns
ctb <- unique(ctb, by = "id")

# Merge SoilData datasets with CTB, keeping only rows with matching 'id'
mapbiomas_ctb <- merge(x = soildata_ctb, y = ctb, by.x = "ctb", by.y = "id", all.y = TRUE)

# Write to file
file_path <- "res/tab/soildata_ctb_mapbiomas.txt"
write.table(mapbiomas_ctb, file_path, sep = "\t", quote = FALSE, row.names = FALSE)


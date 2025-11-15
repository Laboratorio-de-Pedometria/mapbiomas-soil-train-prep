# title: MapBiomas Soil
# subtitle: 21. Estimate Soil Bulk Density
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2025
rm(list = ls())

# Set MapBiomas Soil Collection
collection <- "c3"

# Source helper functions and packages
source("src/00_helper_functions.r")

# LOAD SOILDATA ####################################################################################
# Read SoilData data processed in the previous script. We go back to the file generated in step 14.
file_path <- "data/14_soildata.txt"
soildata <- data.table::fread(file_path, sep = "\t", na.strings = c("", "NA", "NaN"))
summary_soildata(soildata)
# Layers: 62468
# Events: 18882
# Georeferenced events: 16366
# Datasets: 265

# DESIGN MATRIX FOR BULK DENSITY ESTIMATION ########################################################
# Check data type
print(soildata)

# Convert categorical variables to character
soildata[, geomorphon := as.character(geomorphon)]

# Target variable: soil bulk density (dsi)
# Identify soil layers missing soil bulk density data
is_na_dsi <- is.na(soildata[["dsi"]])
nrow(soildata[is.na(dsi), ]) # Result: 53095 layers
nrow(unique(soildata[is.na(dsi), "id"])) # Result: 15281 events

# Plot distribution of the target variable before inputation
file_path <- paste0("res/fig/", collection, "_bulk_density_histogram_before_imputation.png")
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
par(mar = c(5, 4, 2, 2) + 0.1)
hist(soildata[, dsi],
  xlab = "Soil Bulk Density (g/cm³)",
  ylab = paste0("Absolute frequency (n = ", length(na.exclude(soildata[, dsi])), ")"),
  main = "", col = "gray", border = "gray",
  breaks = seq(0, 3, by = 0.1)
)
grid(nx = FALSE, ny = NULL, col = "gray")
rug(soildata[!is_na_dsi, dsi])
dev.off()


# title: SoilData - Soil Organic Carbon Stock
# subtitle: Prepare soil covariates
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

# Source helper functions
source("src/00_helper_functions.r")

# Read data produced in the previous processing script
soildata <- data.table::fread("data/14_soildata_soc.txt", sep = "\t")
summary_soildata(soildata)
# Layers: 29881
# Events: 15729
# Georeferenced events: 13381

# Correct layer depth and name (MOVED TO SOILDATA INTEGRATION)
# soildata[
#   dataset_id == "ctb0829" & observacao_id == "P92" & camada_nome == "A",
#   profund_inf := 8
# ]
soildata[
  dataset_id == "ctb0636" & observacao_id == "Perfil-03" & profund_sup == 0,
  camada_nome := ifelse(camada_nome == "Ao", "A1", camada_nome)
]

# Prepare predictors

# Project geographic coordinates
# Start by converting the geographic coordinates (latitude and longitude) to UTM coordinates
# (Universal Transverse Mercator) using the WGS 84 datum (EPSG:4326). The UTM coordinates are
# projected to the SIRGAS 2000 datum (EPSG:31983) to minimize distortion in the study area.
# The UTM coordinates are projected to the SIRGAS 2000 datum (EPSG:31983) to minimize distortion
# in the study area.
# Start by extracting the geographic coordinates from the soil data. Then create a sf object with
# the geographic coordinates and the WGS 84 datum (EPSG:4326). Finally, project the geographic
# coordinates to the SIRGAS 2000 datum (EPSG:31983). The projected coordinates are added back to the
# soil data.
soildata_sf <- sf::st_as_sf(
  soildata[!is.na(coord_x) & !is.na(coord_y)],
  coords = c("coord_x", "coord_y"), crs = 4326
)
if (FALSE) {
  x11()
  plot(soildata_sf["estado_id"])
}
soildata_sf <- sf::st_transform(soildata_sf, crs = 31983)
soildata[!is.na(coord_x) & !is.na(coord_y), coord_x_utm := sf::st_coordinates(soildata_sf)[, 1]]
soildata[!is.na(coord_x) & !is.na(coord_y), coord_y_utm := sf::st_coordinates(soildata_sf)[, 2]]
summary(soildata[, .(coord_x_utm, coord_y_utm)])
rm(soildata_sf)

# SiBCS
# ORDER and SUBORDER (multivariate)
soildata[, taxon_sibcs := toupper(taxon_sibcs)]
soildata[, taxon_sibcs := gsub("Á", "A", taxon_sibcs)]
soildata[, taxon_sibcs := gsub("Ê", "E", taxon_sibcs)]
soildata[, taxon_sibcs := gsub("Í", "I", taxon_sibcs)]
soildata[, taxon_sibcs := gsub("Ú", "U", taxon_sibcs)]
soildata[, taxon_sibcs := gsub("Ó", "O", taxon_sibcs)]
soildata[taxon_sibcs == "", taxon_sibcs := "NA NA NA NA"]
sibcs <- strsplit(soildata[["taxon_sibcs"]], " ")
sibcs <- lapply(sibcs, function(x) {
  len <- length(x)
  if (len > 4) {
    x <- x[1:4]
  } else if (len < 4) {
    x <- c(x, rep("NA", 4 - len))
  }
  return(x)
})
sibcs <- data.table::as.data.table(do.call(rbind, sibcs))
soildata[, ORDER := sibcs[, 1]]
soildata[, SUBORDER := sibcs[, 2]]
soildata[ORDER == "NA", ORDER := NA_character_]
soildata[SUBORDER == "NA", SUBORDER := NA_character_]
# If categories in ORDER and SUBORDER have less than 15 observations, replace its values with NA
soildata[, ORDER := ifelse(.N < 15, NA_character_, ORDER), by = ORDER]
soildata[, SUBORDER := ifelse(.N < 15, NA_character_, SUBORDER), by = SUBORDER]
soildata[ORDER == "PVA", ORDER := "ARGISSOLO"] # Correct a typo
soildata[ORDER == "PVA", SUBORDER := NA_character_] # Correct a typo
soildata[ORDER == "PODZOLICO", ORDER := "ARGISSOLO"] # Correct a typo
soildata[ORDER == "PODZOLICO", SUBORDER := NA_character_] # Correct a typo
soildata[ORDER == "CA", ORDER := "CAMBISSOLO"] # Correct a typo
soildata[ORDER == "CA", SUBORDER := NA_character_] # Correct a typo
soildata[SUBORDER == "A", SUBORDER := NA_character_] # Correct a typo
soildata[SUBORDER == "QUARTZARENICORTICO", SUBORDER := "QUARTZARENICO"] # Correct a typo
soildata[SUBORDER == "TB", SUBORDER := NA_character_] # Correct a typo
soildata[, .N, by = ORDER]
soildata[, .N, by = SUBORDER]

# STONESOL
# Soil classes known for having a skeleton (bivariate)
soildata[, STONESOL := NA_character_]
soildata[ORDER != "UNKNOWN" | SUBORDER != "UNKNOWN", STONESOL := "FALSE"]
soildata[ORDER == "NEOSSOLO", STONESOL := "TRUE"]
soildata[ORDER == "PLINTOSSOLO", STONESOL := "TRUE"]
soildata[SUBORDER == "FLUVICO", STONESOL := "TRUE"]
soildata[SUBORDER == "LITOLICO", STONESOL := "TRUE"]
soildata[SUBORDER == "REGOLITICO", STONESOL := "TRUE"]
soildata[SUBORDER == "QUARTZARENICO", STONESOL := "FALSE"]
soildata[SUBORDER == "HAPLICO", STONESOL := "FALSE"]
soildata[, .N, by = STONESOL]

# STONY
# Soil layers known for having concretions, nodules, rock fragments, rock-like pedogenic layers, and
# human artifacts (bivariate)
soildata[, STONY := NA_character_]
soildata[camada_nome != "UNKNOWN", STONY := "FALSE"]
soildata[grepl("c", camada_nome, ignore.case = FALSE), STONY := "TRUE"]
soildata[grepl("F", camada_nome, ignore.case = FALSE), STONY := "TRUE"]
# REMOVE
# soildata[grepl("^R$", camada_nome, ignore.case = FALSE, perl = TRUE), esqueleto := 1000]
soildata[grepl("R", camada_nome, ignore.case = TRUE), STONY := "TRUE"]
soildata[grepl("u", camada_nome, ignore.case = FALSE), STONY := "TRUE"]
soildata[, .N, by = STONY]

# ORGANIC
# Organic layers (bivariate)
soildata[, ORGANIC := NA_character_]
soildata[carbono < 80, ORGANIC := "FALSE"]
soildata[carbono >= 80, ORGANIC := "TRUE"]
soildata[grepl("o", camada_nome, ignore.case = TRUE), ORGANIC := "TRUE"]
soildata[grepl("H", camada_nome, ignore.case = FALSE), ORGANIC := "TRUE"]
soildata[, .N, by = ORGANIC]

# AHRZN
# A horizon (bivariate)
soildata[, AHRZN := NA_character_]
soildata[camada_nome != "???", AHRZN := "FALSE"]
soildata[grepl("A", camada_nome, ignore.case = FALSE), AHRZN := "TRUE"]
unique(soildata[AHRZN == "TRUE", camada_nome])
unique(soildata[AHRZN == "FALSE", camada_nome])
soildata[, .N, by = AHRZN]

# BHRZN
# B horizon (bivariate)
soildata[, BHRZN := NA_character_]
soildata[camada_nome != "???", BHRZN := "FALSE"]
soildata[grepl("B", camada_nome, ignore.case = FALSE), BHRZN := "TRUE"]
unique(soildata[BHRZN == "TRUE", camada_nome])
unique(soildata[BHRZN == "FALSE", camada_nome])
soildata[, .N , by = BHRZN]

# Dense horizon
soildata[grepl("t", camada_nome), BHRZN_DENSE := TRUE]
soildata[grepl("v", camada_nome), BHRZN_DENSE := TRUE]
soildata[grepl("pl", camada_nome), BHRZN_DENSE := TRUE]
soildata[grepl("n", camada_nome), BHRZN_DENSE := TRUE]
soildata[is.na(BHRZN_DENSE), BHRZN_DENSE := FALSE]
soildata[camada_nome == "???", BHRZN_DENSE := NA]
soildata[, .N, by = BHRZN_DENSE]

# EHRZN
# E horizon (bivariate)
soildata[, EHRZN := NA_character_]
soildata[camada_nome != "???", EHRZN := "FALSE"]
soildata[grepl("E", camada_nome, ignore.case = FALSE), EHRZN := "TRUE"]
unique(soildata[EHRZN == "TRUE", camada_nome])
unique(soildata[EHRZN == "FALSE", camada_nome])
soildata[, .N, by = EHRZN]

# Bulk density of upper and lower layer
# First, sort the data by soil event (id) and soil layer (camada_id).
# For each soil layer (camada_id) in a soil event (id), identify the bulk density (dsi) of the
# immediately upper and lower layers. If the layer is the first or last in a given soil event (id),
# the bulk density of the upper or lower layer is set to NA, respectively.
soildata <- soildata[order(id, camada_id)]
soildata[, dsi_upper := shift(dsi, type = "lag"), by = id]
soildata[, dsi_lower := shift(dsi, type = "lead"), by = id]
summary(soildata[, dsi_upper])
summary(soildata[, dsi_lower])

# Fine earth of the upper and lower layer
soildata[, fine_upper := shift(terrafina, type = "lag"), by = id]
soildata[, fine_lower := shift(terrafina, type = "lead"), by = id]
summary(soildata[, fine_upper])
summary(soildata[, fine_lower])

# DENSIC
# Dense horizon (bivariate)
soildata[grepl("tg", camada_nome), DENSIC := TRUE]
soildata[grepl("v", camada_nome), DENSIC := TRUE]
soildata[grepl("n", camada_nome), DENSIC := TRUE]
soildata[is.na(DENSIC), DENSIC := FALSE]
soildata[camada_nome == "???", DENSIC := NA]
soildata[, .N, by = DENSIC]

# cec/clay ratio
# Cation exchange capacity (ctc) to clay ratio
soildata[ctc > 0 & argila > 0, cec_clay_ratio := ctc / argila]
summary(soildata[, cec_clay_ratio])

# silt/clay ratio
# Silt to clay ratio
soildata[silte > 0 & argila > 0, silt_clay_ratio := silte / argila]
summary(soildata[, silt_clay_ratio])

# Write data to disk
summary_soildata(soildata)
# Layers: 29881
# Events: 15729
# Georeferenced events: 13381
data.table::fwrite(soildata, "data/20_soildata_soc.txt", sep = "\t")

# title: MapBiomas Soil
# subtitle: 12. Prepare soil covariates
# author: Alessandro Samuel-Rosa
# data: 2025 CC-BY
rm(list = ls())

# Install and load required packages
if (!requireNamespace("data.table")) {
  install.packages("data.table")
}

# Source helper functions
source("src/00_helper_functions.r")

# Read data produced in the previous processing script
soildata <- data.table::fread("data/11_soildata.txt", sep = "\t", na.strings = c("", "NA", "NaN"))
summary_soildata(soildata)
# Layers: 51152
# Events: 16994
# Georeferenced events: 14372
# Datasets: 259

# SPATIAL COORDINATES
# The spatial coordinates will be used as predictors in the modeling process to account for spatial
# variability in soil properties. We expect that nearby locations will have more similar soil
# properties than locations that are far apart. We will use projected coordinates (UTM) using the
# coordinate reference system EPSG:31983 (SIRGAS 2000 / UTM zone 23S) as they are more suitable for
# distance calculations and spatial analysis. As the values of the projected coordinates are large,
# we will center and scale them to have mean 0 and standard deviation 1.
# Project geographic coordinates
soildata_sf <- sf::st_as_sf(
  soildata[!is.na(coord_x) & !is.na(coord_y)],
  coords = c("coord_x", "coord_y"), crs = 4326
)
soildata_sf <- sf::st_transform(soildata_sf, crs = 31983)
soildata[!is.na(coord_x) & !is.na(coord_y), coord_x_utm := sf::st_coordinates(soildata_sf)[, 1]]
soildata[!is.na(coord_x) & !is.na(coord_y), coord_y_utm := sf::st_coordinates(soildata_sf)[, 2]]
# Center and scale coordinates, multiplying by 1000 to have values in the range of soil properties
soildata[, coord_x_utm := scale(coord_x_utm, center = TRUE, scale = TRUE) * 1000]
soildata[, coord_y_utm := scale(coord_y_utm, center = TRUE, scale = TRUE) * 1000]
summary(soildata[, .(coord_x_utm, coord_y_utm)])
rm(soildata_sf)

# BRAZILIAN SOIL CLASSIFICATION
# ORDER and SUBORDER
soildata[, taxon_sibcs := toupper(taxon_sibcs)]
soildata[, taxon_sibcs := gsub("Á", "A", taxon_sibcs)]
soildata[, taxon_sibcs := gsub("Ê", "E", taxon_sibcs)]
soildata[, taxon_sibcs := gsub("É", "E", taxon_sibcs)]
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
soildata[ORDER == "PVA", ORDER := "ARGISSOLO"] # Correct a typo
soildata[ORDER == "PVA", SUBORDER := NA_character_] # Correct a typo
soildata[ORDER == "CA", ORDER := "CAMBISSOLO"] # Correct a typo
soildata[ORDER == "CA", SUBORDER := NA_character_] # Correct a typo
soildata[SUBORDER == "A", SUBORDER := NA_character_] # Correct a typo
soildata[SUBORDER == "QUARTZARENICORTICO", SUBORDER := "QUARTZARENICO"] # Correct a typo
# AREIA
soildata[ORDER == "AREIA", SUBORDER := "QUARTZARENICO"]
soildata[ORDER == "AREIA", ORDER := "NEOSSOLO"]
# AREIAS
soildata[ORDER == "AREIAS", SUBORDER := "QUARTZARENICO"]
soildata[ORDER == "AREIAS", ORDER := "NEOSSOLO"]
# BRUNIZEN
soildata[ORDER == "BRUNIZEN", ORDER := "BRUNIZEM"]
# CAMBISOL
soildata[ORDER == "CAMBISOL", ORDER := "CAMBISSOLO"]
# CHENOSSOLO
soildata[ORDER == "CHENOSSOLO", ORDER := "CHERNOSSOLO"]
# CHERNOSSOLOS
soildata[ORDER == "CHERNOSSOLOS", ORDER := "CHERNOSSOLO"]
# CONCRECIONARIO
soildata[ORDER == "CONCRECIONARIO", SUBORDER := "PETRICO"]
soildata[ORDER == "CONCRECIONARIO", ORDER := "PLINTOSSOLO"]
# CONCRECIONERIO
soildata[ORDER == "CONCRECIONERIO", SUBORDER := "PETRICO"]
soildata[ORDER == "CONCRECIONERIO", ORDER := "PLINTOSSOLO"]
# GLEI
soildata[ORDER == "GLEI", ORDER := "GLEISSOLO"]
# GLEIOSSOLO
soildata[ORDER == "GLEIOSSOLO", ORDER := "GLEISSOLO"]
# GLEY
soildata[ORDER == "GLEY", ORDER := "GLEISSOLO"]
# LATERITA
soildata[ORDER == "LATERITA", ORDER := "PLINTOSSOLO"]
# LATERIRA
soildata[ORDER == "LATERIRA", ORDER := "PLINTOSSOLO"]
# LATERICO
soildata[ORDER == "LATERICO", ORDER := "PLINTOSSOLO"]
# LATERITICO
soildata[ORDER == "LATERITICO", ORDER := "PLINTOSSOLO"]
# LATERIA
soildata[ORDER == "LATERIA", ORDER := "PLINTOSSOLO"]
# LATOSOL
soildata[ORDER == "LATOSOL", ORDER := "LATOSSOLO"]
# LATOSSOL
soildata[ORDER == "LATOSSOL", ORDER := "LATOSSOLO"]
# LATOSOLO
soildata[ORDER == "LATOSOLO", ORDER := "LATOSSOLO"]
# LATOSSOLOS
soildata[ORDER == "LATOSSOLOS", ORDER := "LATOSSOLO"]
# LBRA
soildata[ORDER == "LBRA", ORDER := "LATOSSOLO"]
# LEA
soildata[ORDER == "LEA", ORDER := "LATOSSOLO"]
# LVA
soildata[ORDER == "LVA", ORDER := "LATOSSOLO"]
# LITOSOL
soildata[ORDER == "LITOSOL", SUBORDER := "LITOLICO"]
soildata[ORDER == "LITOSOL", ORDER := "NEOSSOLO"]
# LITOLICO
soildata[SUBORDER == "LITOLICO", SUBORDER := "LITOLICO"]
soildata[SUBORDER == "LITOLICO", ORDER := "NEOSSOLO"]
# LRD
soildata[ORDER == "LRD", ORDER := "LATOSSOLO"]
# NEO0SSOLO
soildata[ORDER == "NEO0SSOLO", ORDER := "NEOSSOLO"]
# NEOSSOLOS
soildata[ORDER == "NEOSSOLOS", ORDER := "NEOSSOLO"]
# NEOSSOLOQUARTZARENICO
soildata[ORDER == "NEOSSOLOQUARTZARENICO", SUBORDER := "QUARTZARENICO"]
soildata[ORDER == "NEOSSOLOQUARTZARENICO", ORDER := "NEOSSOLO"]
# NITOSSOLOS
soildata[ORDER == "NITOSSOLOS", ORDER := "NITOSSOLO"]
# PLANOSOL
soildata[ORDER == "PLANOSOL", ORDER := "PLANOSSOLO"]
# PLANOSSOL
soildata[ORDER == "PLANOSSOL", ORDER := "PLANOSSOLO"]
# PLANOSSONLO
soildata[ORDER == "PLANOSSONLO", ORDER := "PLANOSSOLO"]
# PLITOSSOLO
soildata[ORDER == "PLITOSSOLO", ORDER := "PLINTOSSOLO"]
# PLINNTOSSOLO
soildata[ORDER == "PLINNTOSSOLO", ORDER := "PLINTOSSOLO"]
# PODOZOLICO
soildata[ORDER == "PODOZOLICO", SUBORDER := NA_character_]
soildata[ORDER == "PODOZOLICO", ORDER := "ARGISSOLO"]
# PODZOLICO
soildata[ORDER == "PODZOLICO", SUBORDER := NA_character_]
soildata[ORDER == "PODZOLICO", ORDER := "ARGISSOLO"]
# PODZOLICOS
soildata[ORDER == "PODZOLICOS", SUBORDER := NA_character_]
soildata[ORDER == "PODZOLICOS", ORDER := "ARGISSOLO"]
# POZOLICO
soildata[ORDER == "POZOLICO", SUBORDER := NA_character_]
soildata[ORDER == "POZOLICO", ORDER := "ARGISSOLO"]
# PODZOUCO
soildata[ORDER == "PODZOUCO", SUBORDER := NA_character_]
soildata[ORDER == "PODZOUCO", ORDER := "ARGISSOLO"]
# PVE
soildata[ORDER == "PVE", ORDER := "ARGISSOLO"]
# PODZOL
soildata[ORDER == "PODZOL", ORDER := "ESPODOSSOLO"]
# REGOSOL
soildata[ORDER == "REGOSOL", SUBORDER := "REGOLITICO"]
soildata[ORDER == "REGOSOL", ORDER := "NEOSSOLO"]
# REGOSSOLO
soildata[ORDER == "REGOSSOLO", SUBORDER := "REGOLITICO"]
soildata[ORDER == "REGOSSOLO", ORDER := "NEOSSOLO"]
# SOLO
soildata[ORDER == "SOLO", SUBORDER := NA_character_]
soildata[ORDER == "SOLO", ORDER := NA_character_]
# SOLOS
soildata[ORDER == "SOLOS", SUBORDER := NA_character_]
soildata[ORDER == "SOLOS", ORDER := NA_character_]
# VERTISOL
soildata[ORDER == "VERTISOL", ORDER := "VERTISSOLO"]
# VERTISOLO
soildata[ORDER == "VERTISOLO", ORDER := "VERTISSOLO"]
# COM
soildata[SUBORDER == "COM", SUBORDER := NA_character_]
# NÃO
soildata[SUBORDER == "NÃO", SUBORDER := NA_character_]
# POUCO
soildata[SUBORDER == "POUCO", SUBORDER := NA_character_]
# QUARTZOSA
soildata[SUBORDER == "QUARTZOSA", SUBORDER := "QUARTZARENICO"]
# VARIAÇÃO
soildata[SUBORDER == "VARIAÇÃO", SUBORDER := NA_character_]
# VERMELHO-AMARELADO
soildata[SUBORDER == "VERMELHO-AMARELADO", SUBORDER := "VERMELHO-AMARELO"]
# VERMELHO-AMARELDISTROFICO
soildata[SUBORDER == "VERMELHO-AMARELDISTROFICO", SUBORDER := "VERMELHO-AMARELO"]
# VERMELHO-ESCURO
soildata[SUBORDER == "VERMELHO-ESCURO", SUBORDER := "VERMELHO"]
# If categories in ORDER and SUBORDER have less than 15 observations, replace its values with NA
soildata[, ORDER := ifelse(.N < 15, NA_character_, ORDER), by = ORDER]
soildata[, SUBORDER := ifelse(.N < 15, NA_character_, SUBORDER), by = SUBORDER]
soildata[, .N, by = ORDER][order(ORDER)]
soildata[, .N, by = SUBORDER][order(SUBORDER)]

# Write data to disk ###############################################################################
summary_soildata(soildata)
# Layers: 51152
# Events: 16994
# Georeferenced events: 14372
# Datasets: 259
data.table::fwrite(soildata, "data/12_soildata.txt", sep = "\t")

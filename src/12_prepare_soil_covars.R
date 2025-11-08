# title: MapBiomas Soil
# subtitle: 12. Prepare soil covariates
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2025
rm(list = ls())

# Set MapBiomas Soil Collection
collection <- "c3"

# Source helper functions
source("src/00_helper_functions.r")

# Read data produced in the previous processing script
soildata <- data.table::fread("data/11_soildata.txt", sep = "\t", na.strings = c("", "NA", "NaN"))
summary_soildata(soildata)
# Layers: 66581
# Events: 18887
# Georeferenced events: 16370
# Datasets: 265

# Dataset-wise covariates ##########################################################################

# COARSE DATASET
# Dataset-wise presence of coarse fragments
# If any layer in a dataset has esqueleto > 0, the dataset is considered to have coarse fragments
soildata[, DATASET_COARSE := any(esqueleto > 0, na.rm = TRUE), by = dataset_id]
# If all layers in a dataset have esqueleto == 0, the dataset is considered to have no coarse
# fragments
soildata[, DATASET_COARSE := ifelse(
  all(esqueleto == 0, na.rm = TRUE), FALSE, DATASET_COARSE
), by = dataset_id]
# If all layers in a dataset have esqueleto == NA, the dataset is considered to have no data on
# coarse fragments
soildata[, DATASET_COARSE := ifelse(
  all(is.na(esqueleto)), NA, DATASET_COARSE
), by = dataset_id]
soildata[, .N, by = DATASET_COARSE]

# Event-wise covariates ###########################################################################

# Stoniness
soildata[, .N, by = pedregosidade]
# - -> NA_character_
soildata[grepl("^-$", pedregosidade), pedregosidade := NA_character_]
# Não pedregoso -> "Ausente"
soildata[grepl("Não pedregos", pedregosidade, ignore.case = TRUE), pedregosidade := "Ausente"]
# ausente -> "Ausente"
soildata[grepl("ausente", pedregosidade), pedregosidade := "Ausente"]
# Sem Pedregosidade -> "Ausente"
soildata[grepl("Sem Pedregosidade", pedregosidade), pedregosidade := "Ausente"]
# Pedregoso -> "Pedregosa"
soildata[grepl("Pedregos", pedregosidade, ignore.case = TRUE), pedregosidade := "Pedregosa"]
# Moderadamente pedregosa -> "Moderada"
soildata[grepl("Moderadamente pedregos", pedregosidade, ignore.case = TRUE), pedregosidade := "Moderada"]
# Extremamente pedregoso -> "Extrema"
soildata[grepl("Extremamente pedregos", pedregosidade, ignore.case = TRUE), pedregosidade := "Extrema"]
# Extremamente -> "Extrema"
soildata[grepl("Extremamente", pedregosidade, ignore.case = TRUE), pedregosidade := "Extrema"]
# Ligeiramente pedregoso -> "Ligeira"
soildata[grepl("Ligeiramente pedregos", pedregosidade, ignore.case = TRUE), pedregosidade := "Ligeira"]
# Ligeiramente -> "Ligeira"
soildata[grepl("Ligeiramente", pedregosidade, ignore.case = TRUE), pedregosidade := "Ligeira"]
# Forte -> "Muita"
soildata[grepl("Forte", pedregosidade, ignore.case = TRUE), pedregosidade := "Muita"]
# Muito -> "Muita"
soildata[grepl("Muito", pedregosidade, ignore.case = TRUE), pedregosidade := "Muita"]
# Pouca -> "Ligeira"
soildata[grepl("Pouca", pedregosidade, ignore.case = TRUE), pedregosidade := "Ligeira"]
soildata[, .N, by = pedregosidade]
# Stoniness levels: Ausente, Extrema, Ligeira, Moderada, Muita, Pedregosa
# New column 'stoniness' will copy only the relevant levels from 'pedregosidade'
soildata[, stoniness := ifelse(
  pedregosidade %in% c("Ausente", "Extrema", "Ligeira", "Moderada", "Muita", "Pedregosa"),
  pedregosidade, "unknown"
)]
soildata[, .N, by = stoniness]
# has_stoniness
# NA = "unknown"
# Ausente = FALSE
# All other levels = TRUE
soildata[, has_stoniness := ifelse(
  is.na(pedregosidade) | pedregosidade == "unknown", "unknown",
  ifelse(pedregosidade == "Ausente", FALSE, TRUE)
)]
soildata[, .N, by = has_stoniness]

# Rockiness
soildata[, .N, by = rochosidade]
# - -> NA_character_
soildata[grepl("^-$", rochosidade), rochosidade := NA_character_]
# Sem Rochosidade -> "Ausente"
soildata[grepl("Sem Rochosidade", rochosidade), rochosidade := "Ausente"]
# Não rochosa -> "Ausente"
soildata[grepl("Não rochos", rochosidade, ignore.case = TRUE), rochosidade := "Ausente"]
# Moderadamente rochosa -> "Moderada"
soildata[grepl("Moderadamente rochos", rochosidade, ignore.case = TRUE), rochosidade := "Moderada"]
# Ligeiramente rochosa -> "Ligeira"
soildata[grepl("Ligeiramente rochos", rochosidade, ignore.case = TRUE), rochosidade := "Ligeira"]
# Não rochoso -> "Ausente"
soildata[grepl("Não rochoso", rochosidade, ignore.case = TRUE), rochosidade := "Ausente"]
# Extremamente rochosa -> "Extrema"
soildata[grepl("Extremamente rochos", rochosidade, ignore.case = TRUE), rochosidade := "Extrema"]
# Extremamente -> "Extrema"
soildata[grepl("Extremamente", rochosidade, ignore.case = TRUE), rochosidade := "Extrema"]
# Muito rochosa -> "Muita"
soildata[grepl("Muito rochos", rochosidade, ignore.case = TRUE), rochosidade := "Muita"]
# Muito -> "Muita"
soildata[grepl("Muito", rochosidade, ignore.case = TRUE), rochosidade := "Muita"]
# ausente -> "Ausente"
soildata[grepl("ausente", rochosidade), rochosidade := "Ausente"]
# Rochoso -> "Rochosa"
soildata[grepl("Rochos", rochosidade, ignore.case = TRUE), rochosidade := "Rochosa"]
# Pouco -> "Ligeira"
soildata[grepl("Pouco", rochosidade, ignore.case = TRUE), rochosidade := "Ligeira"]
soildata[, .N, by = rochosidade]
# Rockiness levels: Ausente, Extrema, Ligeira, Moderada, Muita, Rochosa
# New column 'rockiness' will copy only the relevant levels from 'rochosidade'
soildata[, rockiness := ifelse(
  rochosidade %in% c("Ausente", "Extrema", "Ligeira", "Moderada", "Muita", "Rochosa"),
  rochosidade, "unknown"
)]
soildata[, .N, by = rockiness]
# has_rockiness
# NA = "unknown"
# Ausente = FALSE
# All other levels = TRUE
soildata[, has_rockiness := ifelse(
  is.na(rochosidade) | rochosidade == "unknown", "unknown",
  ifelse(rochosidade == "Ausente", FALSE, TRUE)
)]
soildata[, .N, by = has_rockiness]

# State (estado_id)
soildata[, .N, by = estado_id]
# Rio de Janeiro -> RJ
soildata[grepl("Rio de Janeiro", estado_id), estado_id := "RJ"]
# "" -> NA_character_
soildata[estado_id == "", estado_id := NA_character_]
soildata[, .N, by = estado_id]

# COARSE EVENT
# Event-wise presence of coarse fragments
# If any layer in a soil event has esqueleto > 0, the soil event is considered to have coarse
# fragments
soildata[, EVENT_COARSE := any(esqueleto > 0, na.rm = TRUE), by = id]
# If all layers in a soil event have esqueleto == 0, the soil event is considered to have no coarse
# fragments
soildata[, EVENT_COARSE := ifelse(
  all(esqueleto == 0, na.rm = TRUE), FALSE, EVENT_COARSE
), by = id]
# If all layers in a soil event have esqueleto == NA, the soil event is considered to have no data
# on coarse fragments
soildata[, EVENT_COARSE := ifelse(
  all(is.na(esqueleto)), NA, EVENT_COARSE
), by = id]
soildata[, .N, by = EVENT_COARSE]

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
soildata[ORDER == "PVA", ORDER := "ARGISSOLO"]
soildata[ORDER == "PVA", SUBORDER := NA_character_] # Correct a typo
soildata[ORDER == "CA", ORDER := "CAMBISSOLO"] # Correct a typo
soildata[ORDER == "CA", SUBORDER := NA_character_] # Correct a typo
soildata[SUBORDER == "A", SUBORDER := NA_character_] # Correct a typo
soildata[SUBORDER == "QUARTZARENICORTICO", SUBORDER := "QUARTZARENICO"]
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
# CAMBISSOLICO
soildata[ORDER == "CAMBISSOLICO", ORDER := "CAMBISSOLO"]
# CHERNOSSOLOS
soildata[ORDER == "CHERNOSSOLOS", ORDER := "CHERNOSSOLO"]
# CONCRECIONARIO
soildata[ORDER == "CONCRECIONARIO", SUBORDER := "PETRICO"]
soildata[ORDER == "CONCRECIONARIO", ORDER := "PLINTOSSOLO"]
# GLEI
soildata[ORDER == "GLEI", ORDER := "GLEISSOLO"]
# GLEY
soildata[ORDER == "GLEY", ORDER := "GLEISSOLO"]
# LATERITA
soildata[ORDER == "LATERITA", ORDER := "PLINTOSSOLO"]
# LATERITICO
soildata[ORDER == "LATERITICO", ORDER := "PLINTOSSOLO"]
# LATOSOL
soildata[ORDER == "LATOSOL", ORDER := "LATOSSOLO"]
# LATOSSOL
soildata[ORDER == "LATOSSOL", ORDER := "LATOSSOLO"]
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
# NEOSSOLOS
soildata[ORDER == "NEOSSOLOS", ORDER := "NEOSSOLO"]
# NITOSSOLOS
soildata[ORDER == "NITOSSOLOS", ORDER := "NITOSSOLO"]
# PLANOSOL
soildata[ORDER == "PLANOSOL", ORDER := "PLANOSSOLO"]
# PLANOSSOL
soildata[ORDER == "PLANOSSOL", ORDER := "PLANOSSOLO"]
# PODZOLICO
soildata[ORDER == "PODZOLICO", SUBORDER := NA_character_]
soildata[ORDER == "PODZOLICO", ORDER := "ARGISSOLO"]
# PODZOLICOS
soildata[ORDER == "PODZOLICOS", SUBORDER := NA_character_]
soildata[ORDER == "PODZOLICOS", ORDER := "ARGISSOLO"]
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
# AFLORAMENTO
soildata[ORDER == "AFLORAMENTO", ORDER := NA_character_]
# If categories in ORDER and SUBORDER have less than 30 observations, replace its values with NA
soildata[, ORDER := ifelse(.N < 30, NA_character_, ORDER), by = ORDER]
soildata[, SUBORDER := ifelse(.N < 30, NA_character_, SUBORDER), by = SUBORDER]
soildata[, .N, by = ORDER][order(ORDER)]
soildata[, .N, by = SUBORDER][order(SUBORDER)]

# STONESOL
# Soil classes known for having a skeleton (bivariate)
soildata[, STONESOL := NA_character_]
soildata[ORDER != "UNKNOWN" | SUBORDER != "UNKNOWN", STONESOL := "FALSE"]
soildata[ORDER == "NEOSSOLO", STONESOL := "TRUE"]
soildata[ORDER == "PLINTOSSOLO", STONESOL := "TRUE"]
soildata[SUBORDER == "FLUVICO", STONESOL := "TRUE"]
soildata[SUBORDER == "CONCRECIONARIO", STONESOL := "TRUE"]
soildata[SUBORDER == "LITOLICO", STONESOL := "TRUE"]
soildata[SUBORDER == "PETRICO", STONESOL := "TRUE"]
soildata[SUBORDER == "REGOLITICO", STONESOL := "TRUE"]
soildata[SUBORDER == "QUARTZARENICO", STONESOL := "FALSE"]
soildata[SUBORDER == "HAPLICO", STONESOL := "FALSE"]
soildata[, .N, by = STONESOL]

# LAYER-WISE COVARIATES ###########################################################################

soildata[, unique(camada_nome)]

# STONY
# Soil layers known for having concretions, nodules, rock fragments, rock-like pedogenic layers, and
# human artifacts (bivariate)
soildata[, STONY := NA_character_]
soildata[camada_nome != "UNKNOWN", STONY := "FALSE"]
soildata[grepl("c", camada_nome, ignore.case = FALSE), STONY := "TRUE"]
soildata[grepl("F|f", camada_nome, ignore.case = FALSE), STONY := "TRUE"]
soildata[grepl("r", camada_nome, ignore.case = FALSE), STONY := "TRUE"]
soildata[grepl("u", camada_nome, ignore.case = FALSE), STONY := "TRUE"]
# If there are no letters in camada_nome, keep as "UNKNOWN"
soildata[!grepl("[a-zA-Z]", camada_nome), STONY := "UNKNOWN"]
# If there is "camada", "cam.", "CAM", "Secçã", "AREIA", "Sombrico", "SUPERF", "C.SUPE."
# in camada_nome, keep as "UNKNOWN"
soildata[
  grepl("camada|cam.|CAM|Secçã|AREIA|Sombrico|SUPERF|C.SUPE.", camada_nome, ignore.case = TRUE),
  STONY := "UNKNOWN"
]
if (FALSE) {
  View(soildata[, .N, by = .(STONY, camada_nome)][order(camada_nome)])
}

# ORGANIC
# Organic layers (bivariate)
soildata[, ORGANIC := NA_character_]
# If there is a B or E letter in camada_nome, set ORGANIC to FALSE
soildata[grepl("B|E", camada_nome, ignore.case = FALSE), ORGANIC := "FALSE"]
soildata[carbono < 80, ORGANIC := "FALSE"]
soildata[carbono >= 80, ORGANIC := "TRUE"]
soildata[grepl("o", camada_nome, ignore.case = TRUE), ORGANIC := "TRUE"]
soildata[grepl("H", camada_nome, ignore.case = FALSE), ORGANIC := "TRUE"]
soildata[, .N, by = ORGANIC]
if (FALSE) {
  View(soildata[, .N, by = .(ORGANIC, camada_nome)][order(camada_nome)])
}

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
soildata[, .N, by = BHRZN]

# DENSIC
# Dense horizon (bivariate)
soildata[grepl("tg", camada_nome), DENSIC := TRUE]
soildata[grepl("v", camada_nome), DENSIC := TRUE]
soildata[grepl("n", camada_nome), DENSIC := TRUE]
soildata[is.na(DENSIC), DENSIC := FALSE]
soildata[camada_nome == "???", DENSIC := NA]
soildata[, .N, by = DENSIC]

# EHRZN
# E horizon (bivariate)
soildata[, EHRZN := NA_character_]
soildata[camada_nome != "???", EHRZN := "FALSE"]
soildata[grepl("E", camada_nome, ignore.case = FALSE), EHRZN := "TRUE"]
unique(soildata[EHRZN == "TRUE", camada_nome])
unique(soildata[EHRZN == "FALSE", camada_nome])
soildata[, .N, by = EHRZN]

# CHRZN
# C horizon (bivariate)
soildata[, CHRZN := NA_character_]
soildata[camada_nome != "???", CHRZN := "FALSE"]
soildata[grepl("C", camada_nome, ignore.case = FALSE), CHRZN := "TRUE"]
unique(soildata[CHRZN == "TRUE", camada_nome])
unique(soildata[CHRZN == "FALSE", camada_nome])
soildata[, .N, by = CHRZN]

# GLEY
# Gleyed horizon (bivariate)
soildata[, GLEY := NA_character_]
# If has letters in camada_nome, set GLEY to FALSE
soildata[grepl("[a-zA-Z]", camada_nome), GLEY := "FALSE"]
# If has g, set GLEY to TRUE
soildata[grepl("G|g", camada_nome, ignore.case = FALSE), GLEY := "TRUE"]
unique(soildata[GLEY == "TRUE", camada_nome])
unique(soildata[GLEY == "FALSE", camada_nome])
soildata[, .N, by = GLEY]

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

# Coarse fragments of the upper and lower layer
soildata[, coarse_upper := shift(esqueleto, type = "lag"), by = id]
soildata[, coarse_lower := shift(esqueleto, type = "lead"), by = id]
summary(soildata[, coarse_upper])
summary(soildata[, coarse_lower])

# Clay content of the upper and lower layer
soildata[, argila_upper := shift(argila, type = "lag"), by = id]
soildata[, argila_lower := shift(argila, type = "lead"), by = id]

# Sand content of the upper and lower layer
soildata[, areia_upper := shift(areia, type = "lag"), by = id]
soildata[, areia_lower := shift(areia, type = "lead"), by = id]
summary(soildata[, areia_upper])
summary(soildata[, areia_lower])

# cec/clay ratio
# Cation exchange capacity (ctc) to clay ratio
soildata[ctc > 0 & argila > 0, cec_clay_ratio := ctc / argila]
summary(soildata[, cec_clay_ratio])

# silt/clay ratio
# Silt to clay ratio
soildata[silte > 0 & argila > 0, silt_clay_ratio := silte / argila]
summary(soildata[, silt_clay_ratio])

# Write data to disk ###############################################################################
# Number of unique soil events with complete spatial and temporal coordinates
nrow(unique(
  soildata[!is.na(coord_x) & !is.na(coord_y), .(dataset_id, observacao_id, coord_x, coord_y, data_ano)]
))
# 16566
summary_soildata(soildata)
# Layers: 54105
# Events: 18870
# Georeferenced events: 16360
# Datasets: 265
data.table::fwrite(soildata, "data/12_soildata.txt", sep = "\t")

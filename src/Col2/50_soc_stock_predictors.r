# title: SoilData - Soil Organic Carbon Stock
# subtitle: Prepare SOC stock predictors
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Source helper functions
source("src/00_helper_functions.r")

# Read data processed in the previous script
soildata <- data.table::fread("data/40_soildata_soc.txt", sep = "\t")
summary_soildata(soildata)
# Layers: 16743
# Events: 10378
# Georeferenced events: 8311

# Carbon stocks per event
# Aggregation of carbon stocks in the top layer (0 to 30 cm) of each event
# Computation of covariates
soildata_soc_stock <- soildata[
  ,
  .(
    # Target variables
    soc_stock_kgm2 = sum(soc_stock_kgm2, na.rm = TRUE), # time
    endpoint = mean(endpoint), # event (1 = event occurred, 0 = event did not occur)
    # Profile wise predictor variables
    dataset_id = unique(dataset_id),
    year = as.integer(round(mean(data_coleta_ano, na.rm = TRUE))),
    coord_x = mean(coord_x_utm, na.rm = TRUE),
    coord_y = mean(coord_y_utm, na.rm = TRUE),
    br_state = unique(estado_id),
    max_depth = max(profund_inf),
    order = unique(ORDER),
    suborder = unique(SUBORDER),
    stonesol = unique(STONESOL),
    lulc = unique(lulc),
    bdod_0_5cm = mean(bdod_0_5cm, na.rm = TRUE),
    bdod_5_15cm = mean(bdod_5_15cm, na.rm = TRUE),
    bdod_15_30cm = mean(bdod_15_30cm, na.rm = TRUE),
    cfvo_0_5cm = mean(cfvo_0_5cm, na.rm = TRUE),
    cfvo_5_15cm = mean(cfvo_5_15cm, na.rm = TRUE),
    cfvo_15_30cm = mean(cfvo_15_30cm, na.rm = TRUE),
    clay_0_5cm = mean(clay_0_5cm, na.rm = TRUE),
    clay_5_15cm = mean(clay_5_15cm, na.rm = TRUE),
    clay_15_30cm = mean(clay_15_30cm, na.rm = TRUE),
    sand_0_5cm = mean(sand_0_5cm, na.rm = TRUE),
    sand_5_15cm = mean(sand_5_15cm, na.rm = TRUE),
    sand_15_30cm = mean(sand_15_30cm, na.rm = TRUE),
    soc_0_5cm = mean(soc_0_5cm, na.rm = TRUE),
    soc_5_15cm = mean(soc_5_15cm, na.rm = TRUE),
    soc_15_30cm = mean(soc_15_30cm, na.rm = TRUE),
    # Layer wise predictor variables
    # Value of the topmost layer (camada_id == 1) of the profile
    fines_1 = terrafina[camada_id == 1],
    clay_1 = argila[camada_id == 1],
    silt_1 = silte[camada_id == 1],
    sand_1 = areia[camada_id == 1],
    carbon_1 = carbono[camada_id == 1],
    cec_1 = ctc[camada_id == 1],
    ph_1 = ph[camada_id == 1],
    dsi_1 = dsi[camada_id == 1],
    depth_top_1 = profund_sup[camada_id == 1],
    depth_mid_1 = (profund_sup[camada_id == 1] + profund_inf[camada_id == 1]) / 2,
    depth_bottom_1 = profund_inf[camada_id == 1],
    stony_1 = STONY[camada_id == 1],
    organic_1 = ORGANIC[camada_id == 1],
    ahrzn_1 = AHRZN[camada_id == 1],
    bhrzn_1 = BHRZN[camada_id == 1],
    ehrzn_1 = EHRZN[camada_id == 1],
    cec_clay_1 = cec_clay_ratio[camada_id == 1],
    silt_clay_1 = silt_clay_ratio[camada_id == 1],
    # Value of the second layer (camada_id == 2) of the profile
    fines_2 = terrafina[camada_id == 2],
    clay_2 = argila[camada_id == 2],
    silt_2 = silte[camada_id == 2],
    sand_2 = areia[camada_id == 2],
    carbon_2 = carbono[camada_id == 2],
    cec_2 = ctc[camada_id == 2],
    ph_2 = ph[camada_id == 2],
    dsi_2 = dsi[camada_id == 2],
    depth_top_2 = profund_sup[camada_id == 2],
    depth_mid_2 = (profund_sup[camada_id == 2] + profund_inf[camada_id == 2]) / 2,
    depth_bottom_2 = profund_inf[camada_id == 2],
    stony_2 = STONY[camada_id == 2],
    organic_2 = ORGANIC[camada_id == 2],
    ahrzn_2 = AHRZN[camada_id == 2],
    bhrzn_2 = BHRZN[camada_id == 2],
    ehrzn_2 = EHRZN[camada_id == 2],
    cec_clay_2 = cec_clay_ratio[camada_id == 2],
    silt_clay_2 = silt_clay_ratio[camada_id == 2],
    # Value of the third layer (camada_id == 3) of the profile
    fines_3 = terrafina[camada_id == 3],
    clay_3 = argila[camada_id == 3],
    silt_3 = silte[camada_id == 3],
    sand_3 = areia[camada_id == 3],
    carbon_3 = carbono[camada_id == 3],
    cec_3 = ctc[camada_id == 3],
    ph_3 = ph[camada_id == 3],
    dsi_3 = dsi[camada_id == 3],
    depth_top_3 = profund_sup[camada_id == 3],
    depth_mid_3 = (profund_sup[camada_id == 3] + profund_inf[camada_id == 3]) / 2,
    depth_bottom_3 = profund_inf[camada_id == 3],
    stony_3 = STONY[camada_id == 3],
    organic_3 = ORGANIC[camada_id == 3],
    ahrzn_3 = AHRZN[camada_id == 3],
    bhrzn_3 = BHRZN[camada_id == 3],
    ehrzn_3 = EHRZN[camada_id == 3],
    cec_clay_3 = cec_clay_ratio[camada_id == 3],
    silt_clay_3 = silt_clay_ratio[camada_id == 3]
  ),
  by = id
]
# Estimate the maximum SOC stock in the top layer (0 to 30 cm) of each event assuming a constant
# SOC density
soildata_soc_stock[, round(max_stock := soc_stock_kgm2 / max_depth * 30, 1)]
print(soildata_soc_stock)

# Set end point
# If a profile has a max_depth of 30 cm, then set end_point to 1
nrow(soildata_soc_stock[endpoint == 1, ]) # Result: 176 events
soildata_soc_stock[max_depth == 30, endpoint := 1]
nrow(soildata_soc_stock[endpoint == 1, ]) # Result: 6198 events
nrow(soildata_soc_stock[is.na(endpoint), ]) # Result: 0 events
soildata_soc_stock[is.na(endpoint), endpoint := 0]

# Write data to disk
summary_soildata(soildata_soc_stock)
# Layers: 10378
# Events: 10378
# Georeferenced events: 8311
data.table::fwrite(soildata_soc_stock, "data/41_soildata_soc.txt", sep = "\t")

# PREVIOUS /////////////////////////////////////////////////////////////////////////////////////////
# # Agregar estoque de carbono na camada superficial (0 até 30 cm) de cada evento
# colnames(febr_data)
# febr_data <- febr_data[
#   !is.na(carbono_estoque_kgm2) &
#     espessura > 0 &
#     !is.na(coord_x) &
#     !is.na(coord_y) &
#     !is.na(data_coleta_ano),
#   .(
#     cos_estoque_gm2 = as.integer(round(sum(carbono_estoque_kgm2, na.rm = TRUE) * 1000)),
#     data_coleta_ano = as.integer(round(mean(data_coleta_ano, na.rm = TRUE))),
#     coord_x = mean(coord_x, na.rm = TRUE),
#     coord_y = mean(coord_y, na.rm = TRUE),
#     espessura = sum(espessura),
#     FOREST = unique(FOREST),
#     NONFOREST = unique(NONFOREST),
#     PASTURE = unique(PASTURE),
#     AGRICULTURE = unique(AGRICULTURE),
#     FORESTRY = unique(FORESTRY),
#     NONVEGETATION = unique(NONVEGETATION)
#   ),
#   by = id
# ]
# nrow(febr_data) # Result: 8904 events/layers
# 
# if (FALSE) {
#   x11()
#   hist(febr_data[, cos_estoque_gm2] / 1000)
#   rug(febr_data[, cos_estoque_gm2] / 1000)
# }

# Filter out duplicated events
# Dataset ctb0829 used events from the RADAMBRASIL project. Those events are erroneously assigned
# the sampling year 2000. We will correct this by removing those events.
test_columns <- c("coord_x", "coord_y")
duplicates <- duplicated(febr_data[, ..test_columns])
# Identify duplicates with id starting with "ctb0829"
duplicates <- duplicates & grepl("ctb0829", febr_data$id)
sum(duplicates) # 34 duplicated events
febr_data <- febr_data[!duplicates, ]
nrow(febr_data) # 8870 events/layers

# Other datasets used events from the RADAMBRASIL project. Those events are erroneously assigned
# the sampling year. We will correct this by removing those events.
# ctb0702
febr_data <- febr_data[id != "ctb0702-P-86", ]
febr_data <- febr_data[id != "ctb0702-P-97", ]
# ctb0683
febr_data <- febr_data[id != "ctb0683-232", ]
nrow(febr_data) # 8867 events/layers

# Other events with equal coordinates will be jittered (add small noise)
test_columns <- c("coord_x", "coord_y")
duplicates <- duplicated(febr_data[, ..test_columns])
sum(duplicates) # 25 duplicated events
febr_data[duplicates, coord_x := coord_x + runif(.N, -0.0001, 0.0001)]
febr_data[duplicates, coord_y := coord_y + runif(.N, -0.0001, 0.0001)]
nrow(febr_data) # 8867 events

# Write data to disk
data.table::fwrite(febr_data, "mapbiomas-soc/data/06-febr-data.txt", sep = "\t", dec = ",")
data.table::fwrite(
  febr_data[, c("id", "cos_estoque_gm2", "data_coleta_ano", "coord_x", "coord_y")],
  paste0("mapbiomas-soc/res/tab/", format(Sys.time(), "%Y-%m-%d"), "-pontos-estoque-cos.csv")
)

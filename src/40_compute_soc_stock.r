# title: SoilData - Soil Organic Carbon Stock
# subtitle: Compute soil organic carbon stocks
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
if (!require("geobr")) {
  install.packages("geobr")
}
if (!require("qmap")) {
  install.packages("qmap")
  library(qmap)
}

# Source helper functions
source("src/00_helper_functions.r")

# Read data processed in the previous script
soildata <- data.table::fread("data/31_soildata_soc.txt", sep = "\t")
summary_soildata(soildata)
# Layers: 29881
# Events: 15729
# Georeferenced events: 13381

# Remove data from Technosols and Anthrosols
# Solo construído no aterro encerrado da Caturrita, Santa Maria (RS)
soildata <- soildata[dataset_id != "ctb0036", ]

# Projeto Parque Frei Veloso - Levantamento Detalhado dos Solos do Campus da Ilha do Fundão UFRJ
soildata <- soildata[dataset_id != "ctb0599", ]

# Projeto Caldeirão: caracterização e gênese das Terras Pretas de Índio na Amazônia
soildata <- soildata[dataset_id != "ctb0018", ]

summary_soildata(soildata)
# Layers: 29563
# Events: 15542
# Georeferenced events: 13209

# Filter out soil layers missing data on soil organic carbon
is_na_carbono <- is.na(soildata[["carbono"]])
sum(is_na_carbono)
# 1785 layers
soildata <- soildata[!is_na_carbono, ]
summary_soildata(soildata)
# Layers: 27778
# Events: 14761
# Georeferenced events: 12615

# Topsoil --> BECAUSE WE REMOVED LAYERS WITH MISSING DATA ON SOIL ORGANIC CARBON
# For each event (id), check if there is a layer with profund_sup == 0. Filter out events without a
# topsoil layer. This procedure was performed before: it is repeated here because layers missing
# data on soil organic carbon were removed (there may be other reasons too).
soildata[, topsoil := any(profund_sup == 0), by = id]
nrow(unique(soildata[topsoil == FALSE, "id"]))
# 79 events
soildata <- soildata[topsoil == TRUE, ]
soildata[, topsoil := NULL]
summary_soildata(soildata)
# Layers: 27664
# Events: 14682
# Georeferenced events: 12575

# Empty layers --> ALSO BECAUSE WE REMOVED LAYERS WITH MISSING DATA ON SOIL ORGANIC CARBON
# Is there any soil profile (id) missing an intermediate layer? A missing intermediate layer occurs
# when profund_inf of layer i is different from profund_sup of layer i + 1. Many of these profiles
# are complementary/extra observations that sampled only the topsoil and the subsoil, e.g. 0-20 and
# 60-80 cm. They are interval censored. For further analysis, we drop the lowermost layers.
soildata[, empty_layer := shift(profund_inf, type = "lag") != profund_sup, by = id]
nrow(unique(soildata[empty_layer == TRUE, "id"]))
# 1382 events
soildata <- soildata[empty_layer == FALSE | is.na(empty_layer), ]
soildata <- soildata[, empty_layer := NULL]
summary_soildata(soildata)
# Layers: 26282
# Events: 14682
# Georeferenced events: 12575

# Layer limits
# Resetting the limits of each layer according to the target depth range (0 and 30 cm).
# Source: https://github.com/ncss-tech/aqp/blob/master/R/depthWeights.R
# The thickness of each layer is recalculated.
target_layer <- c(0, 30)
soildata[profund_sup < target_layer[1], profund_sup := target_layer[1]]
soildata[profund_inf < target_layer[1], profund_inf := target_layer[1]]
soildata[profund_sup > target_layer[2], profund_sup := target_layer[2]]
soildata[profund_inf > target_layer[2], profund_inf := target_layer[2]]
# Recompute layer thickness
soildata[, espessura := profund_inf - profund_sup]
nrow(soildata[espessura <= 0, ])
# 2892 layers with thickness <= 0 --> remove them
soildata <- soildata[espessura > 0, ]
summary_soildata(soildata)
# Layers: 23390
# Events: 14682
# Georeferenced events: 12575
length(soildata[, unique(dataset_id)])
# 236 datasets

# Volume of coarse fragments
# The volume of coarse fragments is calculated as the volume of the skeleton divided by the density
# of the skeleton (2.65 g/cm^3) and converted to m^3.
soildata[, fragmentos := esqueleto / 2.65 / 1000]
# soildata[dataset_id == "ctb0054", fragmentos := 0]

# Compute soil organic carbon stock (kg/m^2) in each layer
# Source: T. Hengl et al., “SoilGrids1km–global soil information based on automated mapping,” PLoS
# ONE, vol. 9, no. 8, p. e105992, 2014, doi: 10.1371/journal.pone.0105992.
# The first step is to transform the data from fine earth content (g/kg) to volume (cm^3/cm^3). This
# is done assuming that the density of coarse fragments is 2.65 g/cm^3.
# g/kg / g/cm^3
# 1) carbono / 1000: kg/kg
# 2) espessura / 100: m
# 3) dsi * 1000: kg/m^3
# 4) fragmentos: 1
soildata[, soc_stock_kgm2 := (carbono / 1000) * (espessura / 100) * (dsi * 1000) * (1 - fragmentos)]
if (FALSE) {
  x11()
  hist(soildata[, soc_stock_kgm2])
  rug(soildata[, soc_stock_kgm2])
}

# Layers per event
# Count the number of layers per event (id)
soildata[, n := .N, by = "id"]
table(soildata[, n])
#     1     2     3     4     5 
#  7762 10496  4677   440    15 
soildata[, n := NULL]

# Drop rows missing data on coord_x, coord_y, and data_ano and with a thickness of 0 cm
summary_soildata(soildata[
  !is.na(soc_stock_kgm2) & espessura > 0 & !is.na(coord_x) & !is.na(coord_y) & !is.na(data_ano)
])
# Layers: 19028
# Events: 12575
# Georeferenced events: 12575

# Aggregate soil organic carbon stock in the topsoil (0 to 30 cm) of each event
soc_data <- soildata[
  !is.na(soc_stock_kgm2) &
    espessura > 0 &
    !is.na(coord_x) &
    !is.na(coord_y) &
    !is.na(data_ano),
  .(
    soc_stock_g_m2 = round(sum(soc_stock_kgm2, na.rm = TRUE) * 1000),
    year = as.integer(round(mean(data_ano, na.rm = TRUE))),
    coord_x = mean(coord_x, na.rm = TRUE),
    coord_y = mean(coord_y, na.rm = TRUE)
  ),
  by = id
]
summary(soc_data[, soc_stock_g_m2])
summary_soildata(soc_data)
# Layers: 12575
# Events: 12575
# Georeferenced events: 12575

# Process sampling year
# Create an indicator variable for the sampling year (YEAR_index) that is the difference between the
# sampling year and the closest year of the series (1985 - 2023). We start with YEAR_index = 0 for
# all events.
soc_data[, YEAR_index := 0L]
year_range <- c(min = 1985, max = 2023)
# Check if there are any events with sampling year larger than the final year of the series
print(soc_data[year == year_range["max"], id])
soc_data[year > year_range["max"], YEAR_index := year - year_range["max"]]
soc_data[, .N, by = YEAR_index]
# Then set the sampling year to the final year of the series.
soc_data[year > year_range["max"], year := year_range["max"]]
# Check if there are any events with sampling year smaller than the initial year of the series
print(soc_data[year < year_range["min"], id])
soc_data[year < year_range["min"], YEAR_index := year - year_range["min"]]
soc_data[, .N, by = YEAR_index][order(-YEAR_index)]
# Then set the sampling year to the initial year of the series.
soc_data[year < year_range["min"], year := year_range["min"]]
soc_data[, .N, by = year]

# Intersect soil organic carbon samples with Brazilian biomes
# Download the data for the Brazilian biomes
biome <- geobr::read_biomes()
biome <- sf::st_transform(biome, 4326)
biome <- biome[biome$name_biome != "Sistema Costeiro", "name_biome"]
# Create a spatial object with the soil organic carbon data
soc_data_sf <- sf::st_as_sf(soc_data, coords = c("coord_x", "coord_y"), crs = 4326)
# Intersect the soil organic carbon data with the Brazilian biomes
soc_data_sf <- sf::st_join(soc_data_sf, biome)
soc_data[, name_biome := soc_data_sf$name_biome]
rm(soc_data_sf)
# Check if there are any events without a biome
soc_data[, .N, by = name_biome][order(-N)]
# Drop events without a biome
soc_data <- soc_data[!is.na(name_biome), ]
summary_soildata(soc_data)
# Layers: 12574
# Events: 12574
# Georeferenced events: 12574

# National Forest Inventory (IFN)
# Identify the events that are part of the IFN:
# - their id starts with ctb0053, ctb0055, ctb0056, ctb0057, ctb0058, ctb0059, ctb0060, or ctb0061
soc_data[, IFN_index := grepl("^ctb0053|^ctb005[5-9]|^ctb006[0-1]", id)]
soc_data[, IFN_index := as.integer(IFN_index)]
soc_data[, .N, by = IFN_index][order(-N)]
# Compare the SOC stock between the IFN and non-IFN samples
# For the IFN samples, the median SOC stock is higher than for the non-IFN samples in all biomes.
# Differences are statistically significant in the Amazon, Atlantic Forest, and Caatinga. The
# difference is not statistically significant in the Cerrado, but the coifndence interval is very
# wide for the IFN samples. There are no IFN samples in the Pampa and Pantanal biomes.
dev.off()
file_path <- "res/fig/40-soc_stock_boxplot_ifn_biome.png"
png(file_path, width = 480 * 3, height = 480 * 2, res = 72 * 2)
boxplot(
  soc_stock_g_m2 ~ IFN_index + name_biome, soc_data,
  col = c("lightgreen", "lightyellow"),
  ylab = "Soil organic carbon stock (g/m^2)", cex.axis = 0.8, ylim = c(0, 20000)
)
legend("topright",
  legend = c("Non-IFN", "IFN"),
  fill = c("lightgreen", "lightyellow"), cex = 0.8
)
dev.off()
# The SOC stock of IFN samples are skewed to the right. The skew is stronger for large SOC stocks.
# We employ quantile mapping to correct the skewness of the IFN samples.

# Quantile mapping
# The quantile mapping is performed for the IFN samples in each biome. The quantiles of the SOC
# stock of the IFN samples are matched to the quantiles of the SOC stock of the non-IFN samples.
amazon_qmap <- qmap::fitQmap(
  obs = soc_data[name_biome == "Amazônia" & soc_data$IFN_index == 0, "soc_stock_g_m2"],
  mod = soc_data[name_biome == "Amazônia" & soc_data$IFN_index == 1, "soc_stock_g_m2"],
  method = "PTF", wet.day = FALSE, transfun = "power"
)
amazon_qmap <- qmap::doQmap(
  soc_data[name_biome == "Amazônia" & IFN_index == 1, soc_stock_g_m2], amazon_qmap
)
range(amazon_qmap)
#  1151.49 34289.10
# x11()
# par(mfrow = c(1, 3))
# ylim <- extendrange(soc_data[name_biome == "Amazônia", "soc_stock_g_m2"])
# boxplot(
#   soc_data[name_biome == "Amazônia" & ifn == FALSE, "soc_stock_g_m2"],
#   ylim = ylim, sub = "Non-IFN", notch = TRUE
# )
# abline(h = median(amazon_qmap), col = "red", lty = "dashed")
# boxplot(
#   soc_data[name_biome == "Amazônia" & ifn == TRUE, "soc_stock_g_m2"],
#   ylim = ylim, sub = "IFN", notch = TRUE
# )
# abline(h = median(amazon_qmap), col = "red", lty = "dashed")
# mtext("Amazon", side = 3, line = 1, cex = 1.5)
# boxplot(amazon_qmap, ylim = ylim, sub = "IFN (corrected)", notch = TRUE)
# abline(h = median(amazon_qmap), col = "red", lty = "dashed")

# Caatinga
caatinga_qmap <- qmap::fitQmap(
  obs = soc_data[name_biome == "Caatinga" & soc_data$IFN_index == 0, "soc_stock_g_m2"],
  mod = soc_data[name_biome == "Caatinga" & soc_data$IFN_index == 1, "soc_stock_g_m2"],
  method = "PTF", wet.day = FALSE, transfun = "scale"
)
caatinga_qmap <- qmap::doQmap(
  soc_data[name_biome == "Caatinga" & IFN_index == 1, soc_stock_g_m2], caatinga_qmap
)
range(caatinga_qmap)
#  270.5178 17898.3521
# x11()
# par(mfrow = c(1, 3))
# ylim <- extendrange(soc_data[name_biome == "Caatinga", "soc_stock_g_m2"])
# boxplot(
#   soc_data[name_biome == "Caatinga" & ifn == FALSE, "soc_stock_g_m2"],
#   ylim = ylim, sub = "Non-IFN", notch = TRUE
# )
# abline(h = median(caatinga_qmap), col = "red", lty = "dashed")
# boxplot(
#   soc_data[name_biome == "Caatinga" & ifn == TRUE, "soc_stock_g_m2"],
#   ylim = ylim, sub = "IFN", notch = TRUE
# )
# abline(h = median(caatinga_qmap), col = "red", lty = "dashed")
# mtext("Caatinga", side = 3, line = 1, cex = 1.5)
# boxplot(caatinga_qmap, ylim = ylim, sub = "IFN (corrected)", notch = TRUE)
# abline(h = median(caatinga_qmap), col = "red", lty = "dashed")

# Cerrado
cerrado_qmap <- qmap::fitQmap(
  obs = soc_data[name_biome == "Cerrado" & soc_data$IFN_index == 0, "soc_stock_g_m2"],
  mod = soc_data[name_biome == "Cerrado" & soc_data$IFN_index == 1, "soc_stock_g_m2"],
  method = "PTF", wet.day = FALSE, transfun = "power"
)
cerrado_qmap <- qmap::doQmap(
  soc_data[name_biome == "Cerrado" & IFN_index == 1, soc_stock_g_m2], cerrado_qmap
)
range(cerrado_qmap)
#  1011.272 17301.586
# x11()
# par(mfrow = c(1, 3))
# ylim <- extendrange(soc_data[name_biome == "Cerrado", "soc_stock_g_m2"])
# boxplot(
#   soc_data[name_biome == "Cerrado" & ifn == FALSE, "soc_stock_g_m2"],
#   ylim = ylim, sub = "Non-IFN", notch = TRUE
# )
# abline(h = median(cerrado_qmap), col = "red", lty = "dashed")
# boxplot(
#   soc_data[name_biome == "Cerrado" & ifn == TRUE, "soc_stock_g_m2"],
#   ylim = ylim, sub = "IFN", notch = TRUE
# )
# abline(h = median(cerrado_qmap), col = "red", lty = "dashed")
# mtext("Cerrado", side = 3, line = 1, cex = 1.5)
# boxplot(cerrado_qmap, ylim = ylim, sub = "IFN (corrected)", notch = TRUE)
# abline(h = median(cerrado_qmap), col = "red", lty = "dashed")

# Mata Atlântica
atlantica_qmap <- qmap::fitQmap(
  obs = soc_data[name_biome == "Mata Atlântica" & soc_data$IFN_index == 0, "soc_stock_g_m2"],
  mod = soc_data[name_biome == "Mata Atlântica" & soc_data$IFN_index == 1, "soc_stock_g_m2"],
  method = "PTF", wet.day = FALSE, transfun = "power"
)
atlantica_qmap <- qmap::doQmap(
  soc_data[name_biome == "Mata Atlântica" & IFN_index == 1, soc_stock_g_m2], atlantica_qmap
)
range(atlantica_qmap)
#   52.35181 53287.66803
# x11()
# par(mfrow = c(1, 3))
# ylim <- extendrange(soc_data[name_biome == "Mata Atlântica", "soc_stock_g_m2"])
# boxplot(
#   soc_data[name_biome == "Mata Atlântica" & ifn == FALSE, "soc_stock_g_m2"],
#   ylim = ylim, sub = "Non-IFN", notch = TRUE
# )
# abline(h = median(atlantica_qmap), col = "red", lty = "dashed")
# boxplot(
#   soc_data[name_biome == "Mata Atlântica" & ifn == TRUE, "soc_stock_g_m2"],
#   ylim = ylim, sub = "IFN", notch = TRUE
# )
# abline(h = median(atlantica_qmap), col = "red", lty = "dashed")
# mtext("Mata Atlântica", side = 3, line = 1, cex = 1.5)
# boxplot(atlantica_qmap, ylim = ylim, sub = "IFN (corrected)", notch = TRUE)
# abline(h = median(atlantica_qmap), col = "red", lty = "dashed")

# Map bias-corrected SOC stock
soc_data[IFN_index == 1 & name_biome == "Amazônia", soc_stock_g_m2 := amazon_qmap]
soc_data[IFN_index == 1 & name_biome == "Caatinga", soc_stock_g_m2 := caatinga_qmap]
soc_data[IFN_index == 1 & name_biome == "Cerrado", soc_stock_g_m2 := cerrado_qmap]
soc_data[IFN_index == 1 & name_biome == "Mata Atlântica", soc_stock_g_m2 := atlantica_qmap]

# Save figure with the boxplot of SOC stock by biome and IFN status
dev.off()
file_path <- "res/fig/40-soc_stock_boxplot_ifn_biome_adjusted.png"
png(file_path, width = 480 * 3, height = 480 * 2, res = 72 * 2)
boxplot(
  soc_stock_g_m2 ~ IFN_index + name_biome, soc_data,
  col = c("lightgreen", "lightyellow"),
  ylab = "Soil organic carbon stock (g/m^2)", cex.axis = 0.8, ylim = c(0, 20000)
)
legend("topright",
  legend = c("Non-IFN", "IFN"),
  fill = c("lightgreen", "lightyellow"), cex = 0.8
)
dev.off()

# Replicate instances with high SOC stock, creating new nearby instances
# This was originaly implemented before the quantile mapping. So, the instances with high SOC stock
# are not necessarily the same now.

# MATA ATLÂNTICA
# 68000 g/m^2
# x11()
# hist(soc_data[name_biome == "Mata Atlântica", soc_stock_g_m2])
# rug(soc_data[name_biome == "Mata Atlântica", soc_stock_g_m2])
soc_data[name_biome == "Mata Atlântica" & soc_stock_g_m2 > 68000, ]

# ctb0832-226
# -21.23333333, -41.31666667 (ORIGINAL)
# -21.232867, -41.316707
# -21.233262, -41.316175
# -21.233265, -41.315750
# -21.233301, -41.314682
tmp <- soc_data[id == "ctb0832-226"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-21.232867, -21.233262, -21.233265, -21.233301)]
tmp[, coord_x := c(-41.316707, -41.316175, -41.315750, -41.314682)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0691-15
# -20.270007, -40.277173 (ORIGINAL)
# -20.270420, -40.278063
# -20.262183, -40.285028
# -20.270310, -40.281488
# -20.272343, -40.280716
tmp <- soc_data[id == "ctb0691-15"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-20.270420, -20.262183, -20.270310, -20.272343)]
tmp[, coord_x := c(-40.278063, -40.285028, -40.281488, -40.280716)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0662-P89
# -19.5616, -39.8885 (ORIGINAL)
# -19.561325, -39.889315
# -19.561086, -39.889300
# -19.563407, -39.890190
# -19.562224, -39.890426
tmp <- soc_data[id == "ctb0662-P89"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-19.561325, -19.561086, -19.563407, -19.562224)]
tmp[, coord_x := c(-39.889315, -39.889300, -39.890190, -39.890426)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0574-GB-46
# -23.0079224, -43.5042010 (ORIGINAL)
# -23.011275, -43.498102
# -23.005505, -43.497909
# -23.009870, -43.496542
# -23.009707, -43.508743
tmp <- soc_data[id == "ctb0574-GB-46"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-23.011275, -23.005505, -23.009870, -23.009707)]
tmp[, coord_x := c(-43.498102, -43.497909, -43.496542, -43.508743)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# CERRADO
# 60000 g/m^2
# hist(soc_data[name_biome == "Cerrado", soc_stock_g_m2])
# rug(soc_data[name_biome == "Cerrado", soc_stock_g_m2])
soc_data[name_biome == "Cerrado" & soc_stock_g_m2 > 60000, ]

# ctb0617-Perfil-49
# -19.5042035, -47.7903787 (ORIGINAL)
# -19.503229, -47.789611
# -19.503452, -47.791285
# -19.502612, -47.791306
# -19.501429, -47.789654
tmp <- soc_data[id == "ctb0617-Perfil-49"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-19.503229, -19.503452, -19.502612, -19.501429)]
tmp[, coord_x := c(-47.789611, -47.791285, -47.791306, -47.789654)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0617-Perfil-45
# -19.5055229, -47.7914277 (ORIGINAL)
# -19.505525, -47.789965
# -19.505555, -47.790802
# -19.505282, -47.792368
# -19.504827, -47.793184
tmp <- soc_data[id == "ctb0617-Perfil-45"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-19.505525, -19.505555, -19.505282, -19.504827)]
tmp[, coord_x := c(-47.789965, -47.790802, -47.792368, -47.793184)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0777-41
# -13.070637, -46.0070019 (ORIGINAL)
# -13.066117, -46.007771
# -13.073140, -46.004166
# -13.078574, -45.999789
# -13.083507, -45.983137
tmp <- soc_data[id == "ctb0777-41"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-13.066117, -13.073140, -13.078574, -13.083507)]
tmp[, coord_x := c(-46.007771, -46.004166, -45.999789, -45.983137)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0600-TS-8
# -16.6849201, -48.7208003 (ORIGINAL)
# -16.686965, -48.721114
# -16.688906, -48.720489
# -16.686869, -48.721344
# -16.680299, -48.718884
tmp <- soc_data[id == "ctb0600-TS-8"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-16.686965, -16.688906, -16.686869, -16.680299)]
tmp[, coord_x := c(-48.721114, -48.720489, -48.721344, -48.718884)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# CAATINGA
# 23000 g/m^2
# hist(soc_data[name_biome == "Caatinga", soc_stock_g_m2])
# rug(soc_data[name_biome == "Caatinga", soc_stock_g_m2])
soc_data[name_biome == "Caatinga" & soc_stock_g_m2 > 23000, ]

# ctb0058-RN_33
# -5.400389, -35.46005 (ORIGINAL)
# -5.399865, -35.460195
# -5.400656, -35.459691
# -5.400291, -35.458628
# -5.400056, -35.461008
tmp <- soc_data[id == "ctb0058-RN_33"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-5.399865, -5.400656, -5.400291, -5.400056)]
tmp[, coord_x := c(-35.460195, -35.459691, -35.458628, -35.461008)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0059-CE_366
# -7.379668, -39.42 (ORIGINAL)
# -7.380247, -39.419912
# -7.378354, -39.420030
# -7.379505, -39.421270
# -7.379642, -39.418810
tmp <- soc_data[id == "ctb0059-CE_366"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-7.380247, -7.378354, -7.379505, -7.379642)]
tmp[, coord_x := c(-39.419912, -39.420030, -39.421270, -39.418810)]
set.seed(1984)

# ctb0059-CE_54
# -3.599735, -38.88008 (ORIGINAL)
# -3.599965, -38.880073
# -3.599719, -38.879780
# -3.599287, -38.880093
# -3.599680, -38.880456
tmp <- soc_data[id == "ctb0059-CE_54"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-3.599965, -3.599719, -3.599287, -3.599680)]
tmp[, coord_x := c(-38.880073, -38.879780, -38.880093, -38.880456)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0694-49
# -5.3244224, -35.4646402 (ORIGINAL)
# -5.324633, -35.464634
# -5.324412, -35.464353
# -5.324412, -35.464888
# -5.324027, -35.464661
tmp <- soc_data[id == "ctb0694-49"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-5.324633, -5.324412, -5.324412, -5.324027)]
tmp[, coord_x := c(-35.464634, -35.464353, -35.464888, -35.464661)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# PANTANAL
# 10000 g/m^2
# hist(soc_data[name_biome == "Pantanal", soc_stock_g_m2])
# rug(soc_data[name_biome == "Pantanal", soc_stock_g_m2])
soc_data[name_biome == "Pantanal" & soc_stock_g_m2 > 10000, ]

# ctb0054-P11
# -16.65224, -56.38391 (ORIGINAL)
# -16.652519, -56.383898
# -16.652200, -56.383541
# -16.652239, -56.384243
# -16.651841, -56.383962
tmp <- soc_data[id == "ctb0054-P11"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-16.652519, -16.652200, -16.652239, -16.651841)]
tmp[, coord_x := c(-56.383898, -56.383541, -56.384243, -56.383962)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0054-P28
# -16.84245, -56.40736 (ORIGINAL)
# -16.842622, -56.407352
# -16.842436, -56.407151
# -16.842451, -56.407623
# -16.842145, -56.407390
tmp <- soc_data[id == "ctb0054-P28"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-16.842622, -16.842436, -16.842451, -16.842145)]
tmp[, coord_x := c(-56.407352, -56.407151, -56.407623, -56.407390)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0054-P29
# -16.80298, -56.29631 (ORIGINAL)
# -16.803310, -56.296312
# -16.802949, -56.295842
# -16.802703, -56.296850
# -16.802307, -56.296287
tmp <- soc_data[id == "ctb0054-P29"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-16.803310, -16.802949, -16.802703, -16.802307)]
tmp[, coord_x := c(-56.296312, -56.295842, -56.296850, -56.296287)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0763-169
# -19.052547, -57.6605278 (ORIGINAL)
# -19.052604, -57.660814
# -19.052771, -57.661283
# -19.052279, -57.660069
# -19.052044, -57.659567
tmp <- soc_data[id == "ctb0763-169"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-19.052604, -19.052771, -19.052279, -19.052044)]
tmp[, coord_x := c(-57.660814, -57.661283, -57.660069, -57.659567)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# PAMPA
# 16700 g/m^2
# hist(soc_data[name_biome == "Pampa", soc_stock_g_m2])
# rug(soc_data[name_biome == "Pampa", soc_stock_g_m2])
soc_data[name_biome == "Pampa" & soc_stock_g_m2 > 16700, ]

# ctb0037-girua-075
# -28.04955, -54.40351 (ORIGINAL)
# -28.049667, -54.403504
# -28.049547, -54.403327
# -28.049534, -54.403682
# -28.049372, -54.403527
tmp <- soc_data[id == "ctb0037-girua-075"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-28.049667, -28.049547, -28.049534, -28.049372)]
tmp[, coord_x := c(-54.403504, -54.403327, -54.403682, -54.403527)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0037-girua-086
# -28.04434, -54.41399 (ORIGINAL)
# -28.044481, -54.413983
# -28.044337, -54.414179
# -28.044309, -54.413779
# -28.044119, -54.414011
tmp <- soc_data[id == "ctb0037-girua-086"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-28.044481, -28.044337, -28.044309, -28.044119)]
tmp[, coord_x := c(-54.413983, -54.414179, -54.413779, -54.414011)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0770-98
# -32.13333, -52.58333 (ORIGINAL)
# -32.133724, -52.583323
# -32.133170, -52.582529
# -32.132379, -52.583398
# -32.132806, -52.584353
tmp <- soc_data[id == "ctb0770-98"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-32.133724, -32.133170, -32.132379, -32.132806)]
tmp[, coord_x := c(-52.583323, -52.582529, -52.583398, -52.584353)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0797-RS-113
# -30.8161997, -53.8114681 (ORIGINAL)
# -30.816407, -53.811462
# -30.816209, -53.811049
# -30.816195, -53.811794
# -30.815831, -53.811494
tmp <- soc_data[id == "ctb0797-RS-113"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-30.816407, -30.816209, -30.816195, -30.815831)]
tmp[, coord_x := c(-53.811462, -53.811049, -53.811794, -53.811494)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# AMAZÔNIA
# 40000 g/m^2
# hist(soc_data[name_biome == "Amazônia", soc_stock_g_m2])
# rug(soc_data[name_biome == "Amazônia", soc_stock_g_m2])
soc_data[name_biome == "Amazônia" & soc_stock_g_m2 > 40000, ]

# ctb0033-RO1245
# -12.25083, -63.24472 (ORIGINAL)
# -12.251281, -63.244713
# -12.250741, -63.244080
# -12.250814, -63.245319
# -12.250054, -63.244756
tmp <- soc_data[id == "ctb0033-RO1245"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-12.251281, -12.250741, -12.250814, -12.250054)]
tmp[, coord_x := c(-63.244713, -63.244080, -63.245319, -63.244756)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0033-RO1252
# -12.4275, -63.44889 (ORIGINAL)
# -12.427775, -63.448863
# -12.427425, -63.448433
# -12.427504, -63.449427
# -12.426785, -63.448901
tmp <- soc_data[id == "ctb0033-RO1252"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-12.427775, -12.427425, -12.427504, -12.426785)]
tmp[, coord_x := c(-63.448863, -63.448433, -63.449427, -63.448901)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0717-22
# -7.05, -72.65 (ORIGINAL)
# -7.051129, -72.649946
# -7.049883, -72.651427
# -7.050000, -72.648519
# -7.048733, -72.650107
tmp <- soc_data[id == "ctb0717-22"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-7.051129, -7.049883, -7.050000, -7.048733)]
tmp[, coord_x := c(-72.649946, -72.651427, -72.648519, -72.650107)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# ctb0053-RO_567_INDEFORMADA
# -12.95998, -62.64021 (ORIGINAL)
# -12.960279, -62.640199
# -12.959962, -62.639774
# -12.959971, -62.640601
# -12.959482, -62.640242
tmp <- soc_data[id == "ctb0053-RO_567_INDEFORMADA"][rep(1, 4)]
tmp[, id := paste0(id, "-XYREP", 1:4)]
tmp[, coord_y := c(-12.960279, -12.959962, -12.959971, -12.959482)]
tmp[, coord_x := c(-62.640199, -62.639774, -62.640601, -62.640242)]
set.seed(1984)
tmp[, soc_stock_g_m2 := soc_stock_g_m2 + runif(.N, -0.01 * soc_stock_g_m2, 0.01 * soc_stock_g_m2)]
soc_data <- rbind(soc_data, tmp)

# Summary
summary_soildata(soc_data)
# Layers: 12666
# Events: 12666
# Georeferenced events: 12666

# Select columns and set order
soc_data <- soc_data[, .(id, coord_x, coord_y, year, soc_stock_g_m2, IFN_index, YEAR_index)]
soc_data[, soc_stock_g_m2 := round(soc_stock_g_m2)]

# Plot with mapview
if (FALSE) {
  soc_data_sf <- sf::st_as_sf(soc_data, coords = c("coord_x", "coord_y"), crs = 4326)
  mapview::mapview(soc_data_sf, zcol = "YEAR_index")
}

# Write data to disk ###############################################################################
# Processed data
data.table::fwrite(soc_data, "data/40_soildata_soc.txt", sep = "\t")

# Training samples
folder_path <- "~/Insync/MapBiomas Solo/Trainning samples/"
file_name <- "-organic-carbon-stock-gram-per-square-meter.csv"
# List existing files in the folder_path and get the last one. Then read it.
existing_files <- list.files(path = folder_path, pattern = file_name)
last_file <- existing_files[length(existing_files)]
last_soc_data <- data.table::fread(paste0(folder_path, last_file))
# Check if last_soc_data == soc_data. If not, write soc_data to disk.
if (!identical(last_soc_data, soc_data)) {
  cat("Writing data to disk...\n")
  file_path <- paste0(folder_path, format(Sys.time(), "%Y-%m-%d"), file_name)
  file_path <- path.expand(file_path)
  data.table::fwrite(soc_data, file_path)
  cat("Done!\n")
}

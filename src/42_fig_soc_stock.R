# MapBiomas Soil (beta): Create figures
# Alessandro Samuel-Rosa & Taciara Zborowski Horst
# 2024 CC-BY
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
if (!require("rnaturalearth")) {
  install.packages("rnaturalearth", dependencies = TRUE)
}
if (!require("prettymapr")) {
  install.packages("prettymapr")
  library("prettymapr")
}

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
soildata <- soildata[!grepl("-REP[1-4]", id)]
summary_soildata(soildata)
# Layers: 12575
# Events: 12575
# Georeferenced events: 12575

# FIGURES FOR ATBD #################################################################################
# https://drive.google.com/drive/folders/1y36YUjrKYgyyRdkIHrmLFW_KRjT9qAv_?usp=drive_link
# Frequency distribution of soil organic carbon stocks
dev.off()
file_path <- "res/fig/carbon-stock-histogram.png"
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
# x11()
par(mar = c(5, 4, 2, 2) + 0.1)
hist(soildata[, soc_stock_g_m2],
  xlab = expression("Soil organic carbon stock, g/m"^2),
  ylab = paste0("Absolute frequency (n = ", length(soildata[, soc_stock_g_m2]), ")"),
  main = "Soil organic carbon stock"
)
rug(soildata[, soc_stock_g_m2])
dev.off()

# Frequency distribution of soil organic carbon stocks along the time axis
dev.off()
file_path <- "res/fig/carbon-stock-temporal-distribution.png"
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
# x11()
par(mar = c(5, 4, 2, 2) + 0.1)
year_hist <- hist(soildata[, year],
  xlab = "Year of sampling",
  ylab = paste0("Absolute frequency (n = ", length(soildata[, year]), ")"),
  main = "Year of sampling"
)
mtext("NA", side = 1, at = 1948, line = 1)
rug(soildata[, year])
dev.off()

# Spatial distribution of sampling points
# Create a spatial object
soildata_sf <- sf::st_as_sf(soildata, coords = c("coord_x", "coord_y"), crs = 4326)
# Read biomes and transform to WGS84
biomes <- geobr::read_biomes()[-7, ]
biomes <- sf::st_transform(biomes, crs = 4326)
# Read Brazil and transform to WGS84
brazil <- geobr::read_country()
brazil <- sf::st_transform(brazil, crs = 4326)
# Read South America and transform to WGS84
southamerica <- rnaturalearth::ne_countries(continent = c("south america", "europe"),
  returnclass = "sf", scale = "medium")
southamerica <- southamerica[, "iso_a2"]
# Save figures in portuguese and english
lang <- c("pt", "en")
main <- list(
  pt = "Estoque de carbono orgânico do solo",
  en = "Soil organic carbon stock"
)
dev.off()
for (i in seq_along(lang)) {
  file_path <- paste0("res/fig/carbon-stock-spatial-distribution-brazil-", lang[i], ".png")
  png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3)
  # x11()
  par(mar = rep(1.9, 4))
  plot(brazil,
    reset = FALSE, col = "transparent",
    axes = TRUE, graticule = TRUE, lwd = 0.01,
    main = ""
  )
  mtext(main[[i]], side = 3, line = 0.5, cex = 1.5, adj = 0)
  plot(southamerica, reset = FALSE, col = "gray96", add = TRUE, lwd = 0.5)
  plot(biomes["name_biome"], reset = FALSE,
    main = "", axes = TRUE, col = "#eeece1", lwd = 0.5,
    border = "gray69",
    key.pos = NULL, graticule = TRUE, add = TRUE
  )
  plot(soildata_sf["soc_stock_g_m2"], add = TRUE, cex = 0.5, col = "firebrick")
  prettymapr::addscalebar(plotunit = "latlon", plotepsg = 4326, pos = "bottomright")
  dev.off()
}

# Figure for Dataverse thumbnail
# Supported image types are JPG, TIF, or PNG and should be no larger than 500 KB.
# The maximum display size for an image file as a dataset thumbnail is 48 pixels wide by 48 pixels
# high.
dev.off()
file_path <- "res/fig/carbon-stock-spatial-distribution-brazil-thumbnail.png"
png(file_path, width = 480, height = 480, res = 72)
# x11()
par(mar = rep(0, 4))
plot(brazil,
    reset = FALSE, col = "transparent",
    axes = TRUE, graticule = TRUE, lwd = 0.01,
    main = ""
  )
plot(biomes["name_biome"], reset = FALSE, main = "", axes = FALSE, col = "#eeece1", lwd = 0.5,
  border = "gray69", key.pos = NULL
)
plot(soildata_sf["soc_stock_g_m2"], add = TRUE, cex = 0.5, col = "firebrick")
dev.off()

# FACT SHEET: Spatial distribution of sample points
dev.off()
file_path <- "res/fig/carbon-stock-spatial-distribution-brazil-fact-sheet.png"
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3, bg = "transparent")
# x11()
par(mar = rep(1.9, 4))
plot(biomes["name_biome"], reset = FALSE,
  main = "", axes = FALSE, col = "#eeece1", lwd = 0.5,
  border = "gray69", key.pos = NULL)
plot(soildata_sf["soc_stock_g_m2"], add = TRUE, cex = 0.5, col = "firebrick")
prettymapr::addscalebar(
  plotunit = "latlon", plotepsg = 4326, pos = "bottomright", padin = c(1.2, 0.75)
)
dev.off()

# Spatio temporal distribution of the soil organic carbon stock data
# Set time slices
time_slices <- c(1900, 1985, 1995, 2005, 2015, 2023)
time_main <- c("<1985", "1985-1994", "1995-2004", "2005-2014", "2015-2023")
dev.off()
# x11()
file_path <- "res/fig/carbon-stock-spatial-temporal-distribution-brazil.png"
png(file_path, width = 480 * 5, height = 480, res = 72 * 2)
par(mar = rep(1.9, 4), mfrow = c(1, 5))
for (i in 1:5) {
  plot(brazil,
    reset = FALSE, main = time_main[i], col = "transparent",
    axes = TRUE, graticule = TRUE, lwd = 0.01
  )
  plot(southamerica,
    reset = FALSE,
    col = "gray96",
    add = TRUE, lwd = 0.5
  )
  plot(biomes["name_biome"],
    reset = FALSE,
    main = "", axes = TRUE, col = "#eeece1", lwd = 0.5,
    border = "gray69",
    key.pos = NULL, add = TRUE
  )
  idx <- which(soildata_sf[["year"]] >= time_slices[i] &
    soildata_sf[["year"]] < time_slices[i + 1])
  plot(soildata_sf[idx, "soc_stock_g_m2"], add = TRUE, cex = 0.5, col = "firebrick")
  prettymapr::addscalebar(plotunit = "latlon", plotepsg = 4326, pos = "bottomright")
}
dev.off()

# FACT SHEET: Temporal distribution of samples per time slice (barplot)
dev.off()
file_path <- "res/fig/carbon-stock-temporal-distribution-brazil-fact-sheet.png"
png(file_path, width = 480 * 3, height = 480 * 3, res = 72 * 3, bg = "transparent")
# x11()
year_hist <- cut(soildata[, year], breaks = time_slices, labels = time_main)
year_hist <- table(year_hist)
par(mar = c(5, 4, 2, 2) + 0.1)
bp <- barplot(year_hist,
  col = "firebrick",
  xlab = "Ano de amostragem",
  ylab = paste0("Frequência absoluta (n = ", sum(year_hist), ")")
)
text(bp, year_hist, labels = year_hist, pos = 1, cex = 1, col = "white")
dev.off()
print(year_hist)

# Distribution of soil organic carbon stock data by biome
# Intersect soildata with biomes
soildata_sf <- sf::st_join(soildata_sf, biomes[, "name_biome"])
# Create a table with the number of samples per biome
biome_hist <- table(soildata_sf[["name_biome"]])
dev.off()
file_path <- "res/fig/carbon-stock-biome-distribution-brazil-fact-sheet.png"
png(file_path, width = 480 * 4, height = 480 * 3, res = 72 * 3, bg = "transparent")
# x11()
par(mar = c(5, 4, 2, 2) + 0.1)
bp <- barplot(biome_hist,
  col = "firebrick",
  xlab = "Bioma",
  ylab = paste0("Frequência absoluta (n = ", sum(biome_hist), ")"), ylim = c(0, 5000)
)
text(bp, biome_hist, labels = biome_hist, pos = 3, cex = 1)
dev.off()
print(t(biome_hist))

# Compute statistics
# Samples per mapped year
# ~325 amostras por ano mapeado (a maioria foi coletada antes de 1985)
12667 / (2023 - 1984)

# Areal density over Brazil
# ~ 1,5 amostra/mil km2 (vazios e agrupamentos são comuns)
12667 / 8510000 * 1000

# Areal density of points per biome
# O Pampa tem a maior densidade amostral (5,1 amostras/mil km2) e 
# o Pantanal a menor (0,6 amostras/mil km2)
biome_area <- sf::st_area(biomes) / (1000 * 1000 * 1000)
biome_density <- round(c(biome_hist) / biome_area, 1)
t(biome_density)

# Frequency distribution per biome along the time axis
# Generate a figure for each biome, showing the temporal distribution (boxplots) of samples
soildata[, bioma := soildata_sf$name_biome]
soildata[, soc_stock_t_ha := soc_stock_g_m2 / 100]
soildata[, year1985 := ifelse(year < 1985, 1985, year)]
# soildata <- soildata[bioma == "Pampa" & year1985 != 2007, ]
for (i in unique(na.exclude(soildata$bioma))) {
  idx_bio <- which(soildata$bioma == i)
  file_path <- paste0("res/fig/carbon-stock-temporal-boxplot-", i, ".png")
  png(file_path, width = 480 * 4, height = 480 * 3, res = 72 * 3)
  # x11()
  bp <- boxplot(soildata[idx_bio, soc_stock_t_ha ~ year1985], main = i)
  grid()
  abline(h = median(soildata[idx_bio, soc_stock_t_ha]), lty = "dashed", col = "firebrick")
  abline(h = 30, lty = "dotted", col = "blue")
  print(bp)
  legend("topright", legend = c("Mediana histórica", "30 t/ha"), lty = c("dashed", "dotted"),
    col = c("firebrick", "blue"))
  dev.off()
}

# tmp <- read.csv("/home/alessandro/Downloads/ids_lulc_0.csv")
# soildata <- soildata[!(id %in% tmp$ID), ]
# tmp[, .(median(V1), median(year), .N), by = bioma]

# x11()
# boxplot(soc_stock_g_m2 ~ year, data = soildata[bioma == "Caatinga", ], main = "Caatinga")
# abline(h = median(soildata[bioma == "Caatinga", soc_stock_g_m2]))

# hist(soildata[bioma == "Caatinga", soc_stock_g_m2 / 100])
# rug(soildata[bioma == "Caatinga", soc_stock_g_m2 / 100])







# Avaliar distribuição por bioma
biomes$name_biome <- gsub(" ", "-", biomes$name_biome)
biomes$name_biome <- gsub("ô", "o", biomes$name_biome)
biomes$name_biome <- gsub("â", "a", biomes$name_biome)
biomes$name_biome <- tolower(biomes$name_biome)
febr_data_sf[["biome"]] <- NA_character_
dev.off()
for (i in biomes$name_biome) {
  idx_bio <- which(biomes$name_biome == i)
  idx_sf <- sf::st_intersects(biomes[idx_bio, ], febr_data_sf)[[1]]
  febr_data_sf[["biome"]][idx_sf] <- i
  file_name <- paste0(
    "mapbiomas-soc/res/fig/carbon-stock-spatial-distribution-", i, ".png"
  )
  png(file_name, width = 480 * 3, height = 480 * 3, res = 72 * 3, bg = "transparent")
  plot(
    biomes[idx_bio, "name_biome"],
    reset = FALSE, main = "", axes = FALSE, col = "#eeece1", border = "gray69", key.pos = NULL
  )
  plot(
    febr_data_sf["cos_estoque_gm2"][idx_sf, ],
    add = TRUE, pch = 21, col = "firebrick", cex = 1.0
  )
  dev.off()
}

# Number of samples per biome
table(febr_data_sf[["biome"]])

# Areal density of points per biome
round(table(febr_data_sf[["biome"]]) / (as.numeric(sf::st_area(biomes)) / (1000 * 1000 * 1000)), 1)
round(table(febr_data_sf[["biome"]]) / (2022 - 1985))


febr_data <- data.table::fread("res/tab/2023-05-19-pontos-estoque-cos.csv", sep = ",")
febr_data <- sf::st_as_sf(febr_data, coords = c("coord_x", "coord_y"), crs = 4326)
x11()
plot(febr_data["cos_estoque_gm2"])
mapview::mapview(febr_data["cos_estoque_gm2"])

# Avaliar distribuição de frequência da espessura da camada
# dev.off()
# png("mapbiomas-soc/res/fig/carbon-stock-depth-distribution.png",
#   width = 480 * 3, height = 480 * 3, res = 72 * 3
# )
# # x11()
# par(mar = c(5, 4, 2, 2) + 0.1)
# y_max <- hist(febr_data[, espessura], plot = FALSE)$counts
# y_max <- ceiling(max(y_max) / 1000) * 1000
# hist(febr_data[, espessura] / 100,
#   panel.first = grid(nx = FALSE, ny = NULL),
#   xlab = "Espessura da camada superficial, m",
#   ylab = paste0("Frequência absoluta (n = ", nrow(febr_data), ")"),
#   ylim = c(0, y_max),
#   xlim = c(0, 30) / 100,
#   main = ""
# )
# rug(febr_data[, espessura] / 100)
# dev.off()

# Criar objeto espacial e avaliar distribuição do estoque por bioma
# soildata_sf <- sf::st_as_sf(soildata, coords = c("coord_x", "coord_y"), crs = 4326)
# nrow(soildata_sf) # 12575 events
# dev.off()
# png("fig/carbon-stock-spatial-distribution.png", width = 480 * 3, height = 480 * 3, res = 72 * 3)
# # x11()
# plot(biomes["name_biome"], reset = FALSE,
#   main = "", axes = TRUE, col = "transparent", lwd = 0.5, border = "darkgray",
#   key.pos = NULL, graticule = TRUE)
# cex <- soildata_sf[["soc_stock_g_m2"]] / (max(soildata_sf[["soc_stock_g_m2"]]) * 0.4)
# plot(soildata_sf["soc_stock_g_m2"], add = TRUE, pch = 21, cex = cex, col = "black")
# prob <- c(0, 0.5, 0.95, 0.975, 1)
# quant <- round(quantile(soildata_sf[["soc_stock_g_m2"]], prob) / 1000, 1)
# leg_text <- paste(quant, "~ kg/m^2")
# leg_text <- sapply(leg_text, function(x) parse(text = x))
# legend(x = -40, y = 8, legend = leg_text, pch = 21, box.lwd = 0, pt.cex = quantile(cex, prob))
# dev.off()
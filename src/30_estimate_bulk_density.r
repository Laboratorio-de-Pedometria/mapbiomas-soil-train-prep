# title: SoilData - Soil Organic Carbon Stock
# subtitle: Estimate soil bulk density
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("ranger")) {
  install.packages("ranger")
}
if (!require("sf")) {
  install.packages("sf")
}

# Source helper functions
source("src/00_helper_functions.r")

# Read data from disk
soildata <- data.table::fread("data/21_soildata_soc.txt", sep = "\t", na.strings = c("NA", ""))
nrow(unique(soildata[, "id"])) # Result: 11 813 events
nrow(soildata) # Result: 21 890 layers

# Identify layers missing soil bulk density data
is_na_dsi <- is.na(soildata[["dsi"]])
nrow(soildata[is.na(dsi), ]) # Result: 19 047 layers
nrow(unique(soildata[is.na(dsi), "id"])) # Result: 10 481 events

# Endpoint (MUST BE CORRECTED IN PREVIOUS SCRIPT)
soildata[is.na(endpoint), endpoint := 0]

# Covariates

# Bulk density of upper and lower layer
# First, sort the data by soil event (id) and soil layer (camada_id).
# For each soil layer (camada_id) in a soil event (id), identify the bulk density (dsi) of the
# immediately upper and lower layers. If the layer is the first or last in a given soil event (id),
# the bulk density of the upper or lower layer is set to NA, respectively.
soildata <- soildata[order(id, camada_id)]
soildata[, dsi_upper := shift(dsi, type = "lag"), by = id]
soildata[, dsi_lower := shift(dsi, type = "lead"), by = id]

# Dense horizon
soildata[grepl("t", camada_nome), BHRZN_DENSE := TRUE]
soildata[grepl("v", camada_nome), BHRZN_DENSE := TRUE]
soildata[grepl("pl", camada_nome), BHRZN_DENSE := TRUE]
soildata[grepl("n", camada_nome), BHRZN_DENSE := TRUE]
soildata[is.na(BHRZN_DENSE), BHRZN_DENSE := FALSE]
soildata[camada_nome == "???", BHRZN_DENSE := NA]
summary(soildata$BHRZN_DENSE)

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
rm(soildata_sf)

# Set covariates
colnames(soildata)
not_covars <- c(
  "dsi",
  "observacao_id", "coord_x", "coord_y", "coord_precisao", "coord_fonte", "amostra_area",
   "amostra_id", "camada_nome",
  "data_coleta_ano", "id", "coord_datum_epsg", "taxon_sibcs", "esqueleto"
)
covars_names <- colnames(soildata)[!colnames(soildata) %in% not_covars]
print(covars_names)

# Missing value imputation
# Use the missingness-in-attributes (MIA) approach with +/- Inf, with the indicator for missingness
# (mask) to impute missing values in the covariates
covariates <- imputation(soildata[, ..covars_names],
  method = "mia", na.replacement = list(cont = Inf, cat = "unknown"), na.indicator = TRUE
)
covariates <- data.table::as.data.table(covariates)
print(covariates)

# Prepare grid of hyperparameters
# num.trees, mtry, min.node.size and max.depth
num_trees <- c(100, 200, 400, 800)
mtry <- c(2, 4, 8, 16)
min_node_size <- c(1, 2, 4, 8)
max_depth <- c(10, 20, 30, 40)
hyperparameters <- expand.grid(num_trees, mtry, min_node_size, max_depth)
colnames(hyperparameters) <- c("num_trees", "mtry", "min_node_size", "max_depth")

# Fit ranger model testing different hyperparameters
t0 <- Sys.time()
hyper_results <- data.table()
for (i in 1:nrow(hyperparameters)) {
  print(hyperparameters[i, ])
  set.seed(1984)
  model <- ranger::ranger(
    y = soildata[!is_na_dsi, dsi],
    x = covariates[!is_na_dsi, ],
    num.trees = hyperparameters$num_trees[i],
    mtry = hyperparameters$mtry[i],
    min.node.size = hyperparameters$min_node_size[i],
    max.depth = hyperparameters$max_depth[i],
    replace = TRUE,
    verbose = TRUE
  )
  observed <- soildata[!is_na_dsi, dsi]
  predicted <- model$predictions
  error <- observed - predicted
  residual <- mean(observed) - observed
  me <- mean(error)
  mae <- mean(abs(error))
  mse <- mean(error^2)
  rmse <- sqrt(mse)
  nse <- 1 - mse / mean(residual^2)
  slope <- coef(lm(observed ~ predicted))[2]
  hyper_results <- rbind(hyper_results, data.table(
    num_trees = hyperparameters$num_trees[i],
    mtry = hyperparameters$mtry[i],
    min_node_size = hyperparameters$min_node_size[i],
    max_depth = hyperparameters$max_depth[i],
    me = me,
    mae = mae,
    rmse = rmse,
    nse = nse,
    slope = slope
  ))
}
Sys.time() - t0

# Export the results to a TXT file
data.table::fwrite(hyper_results, "res/tab/bulk_density_hyperparameter_tunning.txt", sep = "\t")

# Assess results
# What is the Spearman correlation between hyperparameters and model performance metrics?
correlation <- round(cor(hyper_results, method = "spearman"), 2)
print(correlation[1:4, 5:9])

# Sort the results by RMSE
hyper_results <- hyper_results[order(rmse)]

# Select the best hyperparameters
# Among smallest `rmse`, select the hyperparameters with the smallest `num_trees`.
# Then select the hyperparameters with the largest `nse`.
# Then select the hyperparameters with the smallest `max_depth`.
# Then select the hyperparameters with the smallest `mtry`.
# Then select the hyperparameters with the largest `min_node_size`.
digits <- 2
hyper_best <- round(hyper_results, digits)
hyper_best <- hyper_best[rmse == min(rmse), ]
hyper_best <- hyper_best[nse == max(nse), ]
hyper_best <- hyper_best[num_trees == min(num_trees), ]
hyper_best <- hyper_best[max_depth == min(max_depth), ]
hyper_best <- hyper_best[mtry == min(mtry), ]
hyper_best <- hyper_best[min_node_size == max(min_node_size), ]
print(hyper_best)

# Fit the best model
t0 <- Sys.time()
set.seed(2001)
dsi_model <- ranger::ranger(
  y = soildata[!is_na_dsi, dsi],
  x = covariates[!is_na_dsi, ],
  num.trees = hyper_best$num_trees,
  mtry = hyper_best$mtry,
  min.node.size = hyper_best$min_node_size,
  max.depth = hyper_best$max_depth,
  importance = "impurity",
  replace = TRUE,
  verbose = TRUE
)
Sys.time() - t0

# Compute regression model statistics
dsi_model_stats <- error_statistics(soildata[!is_na_dsi, dsi], dsi_model$predictions)
print(round(dsi_model_stats, 2))

# Write model parameters to disk
write.table(capture.output(print(dsi_model))[6:15],
  file = "res/tab/bulk_density_model_parameters.txt", sep = "\t", row.names = FALSE
)

# Check absolute error
soildata[!is_na_dsi, abs_error := abs(soildata[!is_na_dsi, dsi] - dsi_model$predictions)]
soildata[round(abs_error, 2) >= 1, .(id, camada_id, camada_nome, dsi, dsi_upper, dsi_lower, BHRZN_DENSE, abs_error)]

# Figure: Variable importance
# Keep only those with relative importance > 0.01
dsi_model_variable <- sort(dsi_model$variable.importance)
dsi_model_variable <- round(dsi_model_variable / max(dsi_model_variable), 2)

png("res/fig/bulk_density_variable_importance.png", width = 480 * 3, height = 480 * 4, res = 72 * 3)
par(mar = c(4, 6, 1, 1) + 0.1)
barplot(dsi_model_variable[dsi_model_variable > 0.01],
  horiz = TRUE, las = 1,
  col = "gray", border = "gray",
  xlab = "Relative importance > 0.01", cex.names = 0.5
)
grid(nx = NULL, ny = FALSE, col = "gray")
dev.off()

# Figure: Plot fitted versus observed values
# Set color of points as a function of the absolute error, that is, abs(y - x). The absolute error
# ranges from 0 to 1.
color_breaks <- seq(0, 1, by = 0.2)
color_class <- cut(soildata[!is_na_dsi, abs_error], breaks = color_breaks, include.lowest = TRUE)
color_palette <- RColorBrewer::brewer.pal(length(color_breaks) - 1, "Purples")
color_palette <- color_palette[as.numeric(color_class)]
png("res/fig/bulk_density_observed_versus_oob.png", width = 480 * 3, height = 480 * 3, res = 72 * 3)
par(mar = c(4, 4.5, 2, 2) + 0.1)
plot(
  y = soildata[!is_na_dsi, dsi], x = dsi_model$predictions, xlim = c(0, 2.5), ylim = c(0, 2.5),
  panel.first = grid(),
  pch = 21, bg = color_palette,
  ylab = expression("Observed soil bulk density, g cm"^-3),
  xlab = expression("Fitted bulk soil density (OOB), g cm"^-3)
)
abline(0, 1)
dev.off()

# Predict soil bulk density
dsi_digits <- 2
tmp <- predict(dsi_model, data = covariates[is_na_dsi, ])
soildata[is_na_dsi, dsi := round(tmp$predictions, dsi_digits)]
nrow(unique(soildata[, "id"])) # Result: 11 794
nrow(soildata) # Result: 21 847

# Figure. Distribution of soil bulk density data
png("res/fig/bulk_density_histogram.png", width = 480 * 3, height = 480 * 3, res = 72 * 3)
par(mar = c(5, 4, 2, 2) + 0.1)
hist(soildata[, dsi],
  xlab = expression("Soil bulk density, g cm"^-3),
  ylab = paste0("Absolute frequency (n = ", length(soildata[, dsi]), ")"),
  main = "", col = "gray", border = "gray"
)
grid(nx = FALSE, ny = NULL, col = "gray")
rug(soildata[!is_na_dsi, dsi])
# Legend: bars contain all data points, while the rug plot shows only the non-missing values used
# for model training
legend("topright",
  legend = c("All data (columns)", "Training data (rug)"), fill = c("gray", "black"),
  border = "white", box.lwd = "white" 
)
dev.off()

# Write data to disk
nrow(soildata[is_na_dsi, ]) # 19 036 layers
nrow(unique(soildata[, "id"])) # 11 751 events
data.table::fwrite(soildata, "data/30_soildata_soc.txt", sep = "\t")

# PREVIOUS /////////////////////////////////////////////////////////////////////////////////////////
# MapBiomas Soil (beta): Script 05. Estimate soil bulk density
# Alessandro Samuel-Rosa & Taciara Zborowski Horst
# 2024 CC-BY
# The bulk density is a key soil property to compute SOC stocks. However, data on this soil property
# is missing for various soil layers. We deal with this issue by training a random forest regression
# model to estimate the bulk density of soil samples that are missing data on such variable. We use
# soil and environmental covariates as predictor variables.
# 
# KEY RESULTS
# 
# rm(list = ls())
# # Install and load required packages
# if (!require("ranger")) {
#   install.packages("ranger")
# }
# if (!require("caret")) {
#   install.packages("caret")
# }
# 
# # Ler dados do disco
# febr_data <- data.table::fread("mapbiomas-soc/data/04-febr-data.txt",
#   dec = ",", sep = "\t",
#   stringsAsFactors = TRUE
# )
# str(febr_data)
# nrow(unique(febr_data[, "id"])) # Result: 11 359 events
# nrow(febr_data) # Result: 17 606 layers
# colnames(febr_data)
# 
# # Identify layers missing soil bulk density data
# # We noticed that very high values (> 2.3 g/cm^3) were recorded for a few layers. There
# # also were two B horizons and one A horizon with too low density values (< 0.5). Checking their
# # source soil surveys, we identified that these data were erroneous. The data was then deleted.
# nrow(febr_data[dsi > 2.3, ]) # Result: 7 layers
# febr_data[dsi > 2.3, dsi := NA_real_]
# febr_data[dsi < 0.25, dsi := NA_real_]
# febr_data[dsi < 0.5 & grepl("B", camada_nome), dsi := NA_real_]
# febr_data[
#   dataset_id == "ctb0654" & observacao_id == "11-V-RCC" & camada_nome == "A",
#   dsi := NA_real_
# ]
# dsi_isna <- is.na(febr_data[["dsi"]])
# sum(!dsi_isna)
# sum(dsi_isna) # Result: 2787 and 14 819
# 
# # Figure 1: Distribution of soil bulk density data
# dev.off()
# png("mapbiomas-soc/res/fig/bulk-density-training-data.png",
#   width = 480 * 3, height = 480 * 3, res = 72 * 3
# )
# par(mar = c(5, 4, 2, 2) + 0.1)
# hist(febr_data[["dsi"]],
#   panel.first = grid(nx = FALSE, ny = NULL), 
#   xlab = expression("Densidade do solo, g cm"^-3),
#   ylab = paste0("Frequência absoluta (n = ", sum(!dsi_isna), ")"),
#   ylim = c(0, 1000),
#   xlim = c(0, 2.5),
#   main = "")
# rug(febr_data[["dsi"]])
# dev.off()
# 
# # Estimate random forest model
# colnames(febr_data)
# covars <- colnames(febr_data)
# idx <- which(covars == "ORDER")
# covars <- covars[idx:length(covars)]
# dsi_formula <- as.formula(paste0("dsi ~ ", paste0(covars, collapse = " + ")))
# 
# t0 <- proc.time()
# set.seed(1984)
# dsi_model <- ranger::ranger(
#   formula = dsi_formula,
#   data = febr_data[!dsi_isna, ],
#   num.trees = ceiling(nrow(febr_data[!dsi_isna, ]) * 0.25),
#   importance = "impurity"
# )
# proc.time() - t0
# 
# # Compute regression model statistics
# errorStatistics <-
#   function(observed, predicted) {
#     error <- predicted - observed
#     residual <- mean(observed) - observed
#     me <- mean(error)
#     mae <- mean(abs(error))
#     mse <- mean(error^2)
#     rmse <- sqrt(mse)
#     nse <- 1 - mse/mean(residual^2)
#     slope <- coef(lm(observed ~ predicted))[2]
#     return(data.frame(me, mae, mse, rmse, nse, slope))
# }
# print(dsi_model)
# 
# # Write model parameters to disk
# write.table(capture.output(print(dsi_model))[6:15],
#     file = "mapbiomas-soc/res/tab/bulk-density-model-parameters.txt", sep = "\t",
#     row.names = FALSE
# )
# 
# # Variable importance
# dev.off()
# png("mapbiomas-soc/res/fig/bulk-density-variable-importance.png",
#   width = 480 * 3, height = 480 * 4, res = 72 * 3)
# par(mar = c(4, 6, 1, 1) + 0.1)
# barplot(sort(dsi_model$variable.importance) / max(dsi_model$variable.importance),
#   horiz = TRUE, las = 1, col = "white", border = "white", axes = FALSE,
#   xlab = "Importância relativa", cex.names = 0.5)
# grid(nx = NULL, ny = FALSE)
# barplot(sort(dsi_model$variable.importance) / max(dsi_model$variable.importance),
#   horiz = TRUE, las = 1, add = TRUE, cex.names = 0.5)
# dev.off()
# 
# # Fitted versus observed
# dev.off()
# png("mapbiomas-soc/res/fig/bulk-density-observed-versus-oob.png",
#   width = 480 * 3, height = 480 * 3, res = 72 * 3
# )
# par(mar = c(4, 4.5, 2, 2) + 0.1)
# plot(y = febr_data[!dsi_isna, dsi], x = dsi_model$predictions, xlim = c(0, 2.5), ylim = c(0, 2.5), 
#   panel.first = grid(),
#   ylab = expression("Densidade do solo observada, g cm"^-3),
#   xlab = expression("Densidade do solo predita (OOB), g cm"^-3)
# )
# abline(0, 1)
# dev.off()
# 
# # k-fold cross-validation with k = 10
# t0 <- proc.time()
# loocv_dsi_model <- caret::train(
#   form = dsi_formula,
#   method = "ranger",
#   num.trees = dsi_model$num.trees,
#   trControl = caret::trainControl(method = "cv", number = 10, savePredictions = TRUE),
#   tuneGrid = data.frame(
#     mtry = dsi_model$mtry,
#     min.node.size = dsi_model$min.node.size,
#     splitrule = dsi_model$splitrule),
#   data = febr_data[!dsi_isna, ]
# )
# proc.time() - t0
# print(loocv_dsi_model)
# dsi_model_stats <- rbind(
#   round(errosStatistics(febr_data[!dsi_isna, dsi], dsi_model$predictions), 4),
#   round(errosStatistics(loocv_dsi_model$pred$obs, loocv_dsi_model$pred$pred), 4)
# )
# rownames(dsi_model_stats) <- c("out-of-bag", "10-fold cv")
# print(dsi_model_stats)
# 
# # Write model statistics to disk
# write.table(dsi_model_stats,
#     file = "mapbiomas-soc/res/tab/bulk-density-model-statistics.txt", sep = "\t"
# )
# 
# dev.off()
# png("mapbiomas-soc/res/fig/bulk-density-observed-versus-10cv.png",
#   width = 480 * 3, height = 480 * 3, res = 72 * 3
# )
# par(mar = c(4, 4.5, 2, 2) + 0.1)
# plot(y = loocv_dsi_model$pred$obs, x = loocv_dsi_model$pred$pred, xlim = c(0, 2.5),
#   ylim = c(0, 2.5),
#   panel.first = grid(),
#   ylab = expression("Densidade do solo observada, g cm"^-3),
#   xlab = expression("Densidade do solo predita (CV), g cm"^-3)
# )
# abline(0, 1)
# dev.off()
# 
# # Predict soil bulk density
# tmp <- predict(dsi_model, data = febr_data[dsi_isna, ])
# febr_data[dsi_isna, dsi := round(tmp$predictions, 2)]
# nrow(unique(febr_data[, "id"])) # Result: 11 359
# nrow(febr_data) # Result: 17 606
# 
# # Write data to disk
# data.table::fwrite(febr_data, "mapbiomas-soc/data/05-febr-data.txt", sep = "\t", dec = ",")


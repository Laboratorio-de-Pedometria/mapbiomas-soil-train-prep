# title: SoilData - Soil Organic Carbon Stock
# subtitle: Estimate SOC stock
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# data: 2024 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("randomForestSRC")) {
  install.packages("randomForestSRC")
  library(randomForestSRC)
}

# Source helper functions
source("src/00_helper_functions.r")

# Read data processed in the previous script
soildata <- data.table::fread("data/41_soildata_soc.txt", sep = "\t")
summary_soildata(soildata)
# Layers: 10378
# Events: 10378
# Georeferenced events: 8311

# Fix NAs
soildata[order == "", order := NA_character_]
soildata[suborder == "", suborder := NA_character_]
soildata[lulc == "", lulc := NA_character_]
soildata[br_state == "", br_state := NA_character_]

# Missingness
# Select all columns of soildata, except id, soc_stock_kgm2, and endpoint
# randomForestSRC::rfsrc does not handle the +/- Inf MIA approach, so that we use +/- 99999 instead
colnames(soildata)
soildata_mia <- imputation(soildata[, !c("id", "soc_stock_kgm2", "endpoint"), with = FALSE],
  method = "mia", na.indicator = TRUE, na.replacement = list(cat = "???", cont = 99999)
)
# Identify character columns. Then set them as factors, except id
char_cols <- sapply(soildata_mia, is.character)
soildata_mia[, char_cols] <- lapply(soildata_mia[, char_cols], as.factor)
head(soildata_mia)
str(soildata_mia)

# Combine the imputed data with the original data
soildata_mia <- cbind(soildata[, c("id", "soc_stock_kgm2", "endpoint")], soildata_mia)
colnames(soildata_mia)

# Model training
# ?randomForestSRC::rfsrc
# Set maximum number of trees
# num_trees <- ceiling(nrow(soildata_mia) * 0.25) # Result: 1509
cols_skip <- c(
  "id"
  # "year",
  # "dataset_id",
  # "coord_x_isna", "coord_x_minf", "coord_x_pinf",
  # "coord_y_isna", "coord_y_minf", "coord_y_pinf"
)
set.seed(1984)
soc_stock_survival <- randomForestSRC::rfsrc(
  Surv(soc_stock_kgm2, endpoint) ~ ., soildata_mia[, !..cols_skip],
  ntree = 200, # Number of trees to grow
  nodesize = 15,
  nsplit = 10, # Non-negative integer specifying number of random splits for splitting a variable
  importance = "none", # Can be recovered later using vimp() or predict()
  block.size = 1, # Number of trees in a block used to compute the cumulative error rate
  do.trace = 1
  # Vector of values specifying the time points to be used for survival to constrain ensemble
  # calculations
  # ntime = seq_len(ceiling(max(soildata_mia[, soc_stock_kgm2]))),
  # save.memory = TRUE
)
# Save the model
saveRDS(soc_stock_survival, "data/42_soc_stock_survival.rds")
print(soc_stock_survival)

# Save figure with error rate
file_path <- "res/fig/42_soc_stock_survival_error_rate.png"
png(file_path, width = 480 * 2, height = 480 * 2 , res = 72 * 2)
plot(soc_stock_survival)
grid(col = "gray")
dev.off()

# Compute and save variable importance
soc_stock_survival_vimp <- randomForestSRC::vimp(soc_stock_survival)
print(soc_stock_survival_vimp)
saveRDS(soc_stock_survival_vimp, "data/42_soc_stock_survival_vimp.rds")

# Save figure with variable importance
var_imp <- sort(soc_stock_survival_vimp$importance, decreasing = TRUE)[1:20]
file_path <- "res/fig/42_soc_stock_survival_vimp.png"
png(file_path, width = 480 * 2, height = 480 * 2, res = 72 * 2)
par(mar = c(4, 9, 2, 2))
barplot(
  sort(var_imp, decreasing = FALSE),
  horiz = TRUE, las = 1, border = "gray", col = "gray", xlab = "Variable importance"
)
grid(nx = NULL, ny = NA, col = "gray")
dev.off()



#

which(apply(soc_stock_survival[["survival"]][idx_censored, ], 1, function(x) any(x < 0.5)))


# Predict SOC stock of censored events using the survival function
str(soc_stock_survival)
idx_censored <- which(soc_stock_survival$yvar$endpoint == 0)
i <- 157
soildata[idx_censored[i], 1:10]
# x11()
par(mfrow = c(1, 2))
# Survival
plot(
  x = soc_stock_survival$time.interest,
  y = soc_stock_survival[["survival"]][idx_censored[i], ],
  type = "b", xlab = "Time", ylab = "Survival", cex = 0.5,
  ylim = c(0, 1), main = "Survival"
)
grid()
abline(h = 0.5, col = "red", lty = 2)
abline(v = soc_stock_survival$yvar$soc_stock_kgm2[idx_censored[i]], col = "blue", lty = 2)
idx_min_50 <- which.min(abs(soc_stock_survival[["survival"]][idx_censored[i], ] - 0.5))
new_soc_stock <- soc_stock_survival$time.interest[idx_min_50]
abline(v = new_soc_stock, col = "purple", lty = 2)
# Survival OOB
plot(
  x = soc_stock_survival$time.interest,
  y = soc_stock_survival[["survival.oob"]][idx_censored[i], ],
  type = "b", xlab = "Time", ylab = "Survival", cex = 0.5,
  ylim = c(0, 1), main = "Survival OOB"
)
grid()
abline(h = 0.5, col = "red", lty = 2)
abline(v = soc_stock_survival$yvar$soc_stock_kgm2[idx_censored[i]], col = "blue", lty = 2)
idx_min_50 <- which.min(abs(soc_stock_survival[["survival.oob"]][idx_censored[i], ] - 0.5))
new_soc_stock <- soc_stock_survival$time.interest[idx_min_50]
abline(v = new_soc_stock, col = "purple", lty = 2)

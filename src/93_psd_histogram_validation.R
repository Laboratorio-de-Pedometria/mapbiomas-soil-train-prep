# title: Validation of Soil Particle Size Distribution (PSD) 
# author: Alessandro Samuel-Rosa and Taciara Zborowski Horst
# date: 2025 CC-BY
# summary: This script compares the mapped histogram of the Soil Particle Size Distribution (PSD)
# data with field data from commercial laboratories. 

rm(list = ls())

####################################################################################################
# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Read field soil data
file_path <- path.expand("~/ownCloud/geoURI/res/01_argila_hist.csv")
field_data <- data.table::fread(file_path)

# Select only data for Rio Grande do Sul (RS) in 2010 and with more than 50 samples
field_data <- field_data[estado_id == "RS" & data_ano == 2010]
field_data[, n := sum(argila), by = cidade_id]
field_data <- field_data[n > 50]

# Read mapped soil data
mapped_data <- data.table::fread("data/Argila_RS_Agricultura2010.csv")
data.table::setnames(mapped_data, "NM_MUN", "cidade_id")
mapped_data[, cidade_id := toupper(cidade_id)]
mapped_data <- mapped_data[cidade_id %in% unique(field_data$cidade_id)]


city <- "CERRO GRANDE"
observed_counds <- field_data[cidade_id == city, argila]
observed_counds <- observed_counds
n <- sum(observed_counds)
mapped_area_prop <- mapped_data[cidade_id == city, -1]
mapped_area_prop <- as.numeric(mapped_area_prop / sum(mapped_area_prop))
expected_counts <- n * mapped_area_prop
counts <- data.table(
  observed = observed_counds,
  expected = expected_counts
)
print(counts)

# Fisher's Exact Test
fisher_test_result <- fisher.test(counts$observed, counts$expected, simulate.p.value = TRUE)

# Earth Mover's Distance (EMD) / Wasserstein Metric
install.packages("emdist")
emdist::emd(
  A = matrix(c(counts$observed, seq(5, 95, 10)), ncol = 2),
  B = matrix(c(counts$expected, seq(5, 95, 10)), ncol = 2)
)


cat("Fisher's Exact Test p-value:", fisher_test_result$p.value, "\n")
cat("Earth Mover's Distance (EMD):", emd_result, "\n")
# Check if the observed and expected counts are valid for EMD
            



# Chi-square test
observed_counds <- counts$observed
expected_counts <- counts$expected + 0.0001  # Avoid division by zero
if (length(observed_counds) != length(expected_counts)) {
  stop("Observed and expected counts must have the same length.")
}
chi_square_statistic <- sum((observed_counds - expected_counts)^2 / expected_counts, na.rm = TRUE)
df <- length(observed_counds) - 1
p_value <- pchisq(chi_square_statistic, df, lower.tail = FALSE)
cat("Chi-square statistic:", chi_square_statistic, "\n")
cat("Degrees of freedom:", df, "\n")
cat("P-value:", p_value, "\n")
if (p_value < 0.1) {
  cat("The observed counts differ significantly from the expected counts.\n")
} else {
  cat("The observed counts do not differ significantly from the expected counts.\n")
}


# title: SoilData
# subtitle: Clean data
# author: Alessandro Samuel-Rosa
# data: 2025 CC-BY
rm(list = ls())

# Install and load required packages
if (!require("data.table")) {
  install.packages("data.table")
}

# Source helper functions
source("src/00_helper_functions.r")

# Read SoilData data processed in the previous script
soildata <- data.table::fread("data/10_soildata.txt", sep = "\t")

summary_soildata(soildata)
# Layers: 57077
# Events: 16824
# Georeferenced events: 14334
# Datasets: 255

# Clean datasets ###################################################################################
# ctb0042
# Alteração do pH do solo por influência da diluição, tipo de solvente e tempo de contato
# Contains data from a laboratory experiment, thus not representing real soil conditions required
# for digital soil mapping.
soildata <- soildata[dataset_id != "ctb0042", ]

# ctb0009
# Variáveis pedogeoquímicas e mineralógicas na identificação de fontes de sedimentos em uma
# bacia hidrográfica de encosta
# Contains soil data from roadsides and riversides. The data, however, is not yet available.
soildata <- soildata[dataset_id != "ctb0009", ]

# ctb0001
# Conteúdo de ferro do solo sob dois sistemas de cultivo na Estação Experimental Terras Baixas nos
# anos de 2012 e 2013
# soildata <- soildata[dataset_id != "ctb0001", ]

# ctb0026
# Conteúdo de ferro do solo no ano de 1998
# soildata <- soildata[dataset_id != "ctb0026", ]


summary_soildata(soildata)
# Layers: 57019
# Events: 16766
# Georeferenced events: 14276
# Datasets: 254

# Clean layers #####################################################################################

# MISSING DEPTH
# Check if there are layers missing profund_sup or profund_inf
soildata[, na_depth := is.na(profund_sup) | is.na(profund_inf)]
soildata[na_depth == TRUE, .(id, camada_nome, profund_sup, profund_inf, carbono)]

# Soil end point (<= 100 cm)
# The variable 'endpoint' identifies if the soil profile sampling and description went all the way
# till its end, i.e. down to the bedrock (R).
# Identify layers that have the following:
# 1) camada_nome containing the letter R and
# 2) profund_sup starting before or at 100 cm and
# 3) carbono == NA
# 4) argila == NA
max_depth <- 100
soildata[,
  r_endpoint := grepl("R", camada_nome, ignore.case = TRUE) & profund_sup <= max_depth &
    is.na(carbono) & is.na(argila)
]
soildata[r_endpoint == TRUE, .N] # 324 layers
soildata[, endpoint := ifelse(any(r_endpoint == TRUE), 1, NA_integer_), by = id]
soildata[, r_endpoint := NULL]
# If dataset_id == ctb0003 and profund_inf < 20, then endpoint := 1
soildata[
  dataset_id == "ctb0003" & profund_inf < 20,
  endpoint := 1
]
sum(soildata[["endpoint"]], na.rm = TRUE) # 1144 events with endpoint <= 100 cm
# View(soildata[endpoint == 1, .(id, camada_nome, profund_sup, profund_inf, carbono)])

# Filter out soil layers starting below a maximum depth of 100 cm
# We work only with data from the first 100 cm and deeper layers that start at or before 100 cm.
nrow(soildata[profund_sup >= max_depth, ]) # 7969 layers with profund_sup >= 100
soildata <- soildata[profund_sup >= 0 & profund_sup <= max_depth, ]
summary_soildata(soildata)
# Layers: 50395
# Events: 16740
# Georeferenced events: 14260
# Datasets: 254

# Adjacent layers
# For each event (id), profund_inf of layer i should be equal to profund_sup of layer i + 1.
# For records with abs(diff) %in% 1:10, set profund_inf = profund_inf + (diff * -1)
# Filter out records for which abs(diff) > 10
soildata[, diff := profund_inf - data.table::shift(profund_sup, type = "lead"), by = id]
nrow(soildata[abs(diff) %in% 1:10, ]) # 2563 layers
soildata[abs(diff) %in% 1:10, profund_inf := profund_inf + (diff * -1)]
soildata[, diff10 := any(diff > 10), id]
nrow(soildata[diff10 == TRUE, ]) # 814 layers
soildata <- soildata[diff10 == FALSE | is.na(diff10), ]
summary_soildata(soildata)
# Layers: 49581
# Events: 16581
# Georeferenced events: 14200
# Datasets: 254
soildata[, diff := NULL]
soildata[, diff10 := NULL]

# Filter out layers with profund_sup == profund_inf
soildata <- soildata[profund_sup < profund_inf, ]
summary_soildata(soildata)
# Layers: 49569
# Events: 16581
# Georeferenced events: 14200
# Datasets: 254

# Thickness
# Compute layer thickness
soildata[, espessura := profund_inf - profund_sup]

# Maximum layer thickness (CANCELED)
# Filter out soil layers with thickness > 50 cm
# Some of these layers are below 30 cm depth or result form typing errors: a common recommendation
# of soil description and sampling manuals is to use a maximum layers thickness of 50 cm
max_thickness <- 50
nrow(soildata[espessura > max_thickness, ]) # 3387 layers
soildata[espessura > max_thickness, .(id, camada_nome, profund_sup, profund_inf, espessura)]
# soildata <- soildata[espessura <= max_thickness, ]

# Update layer id
# Sort each event (id) by layer depth (profund_sup and profund_inf)
soildata <- soildata[order(id, profund_sup, profund_inf)]
soildata[, camada_id := 1:.N, by = id]

# Topsoil
# For each event (id), check if there is a layer with profund_sup == 0. Missing a surface layer is
# common in reconnaissance soil surveys, where only the diagnostic subsurface horizons are 
# described. It can also occur in studies that use data from various sources and have a focus on
# subsurface horizons.
# ctb0033 has 851 events without a topsoil layer. This is because, for many soil profiles, soil
# samples for laboratory were not collected from the entire soil horizon. This seems to be due to 
# the presence of a thin organic layer at the soil surface or coarse fragments that were not
# sampled.
# Filter out whole events without a topsoil layer.
soildata[, topsoil := any(profund_sup == 0), by = id]
nrow(unique(soildata[topsoil != TRUE, "id"])) # 710 events
print(soildata[topsoil != TRUE, .N, by = dataset_id])
# For each soil profile (id) in dataset_id == "ctb0033", identify the minimum value of profund_sup.
# If the minimum value is between 0 and 10 cm, add that value to profund_sup and profund_inf of all
# layers in that soil profile.
miss_limit <- 10
soildata[, min_profund_sup := min(profund_sup), by = id]
soildata[min_profund_sup > miss_limit, min_profund_sup := 0]
soildata[
  dataset_id == "ctb0033" & min_profund_sup > 0,
  profund_sup := profund_sup + min_profund_sup
]
soildata[
  dataset_id == "ctb0033" & min_profund_sup > 0,
  profund_inf := profund_inf + min_profund_sup
]
soildata[, min_profund_sup := NULL]
# Recompute
soildata[, topsoil := any(profund_sup == 0), by = id]
soildata <- soildata[topsoil == TRUE, ]
soildata[, topsoil := NULL]
summary_soildata(soildata)
# Layers: 47913
# Events: 15871
# Georeferenced events: 13560
# Datasets: 254

# Endpoint
soildata[is.na(endpoint), endpoint := 0]

# Clean events

# Remove remaining duplicated events (NOT MOVED TO SOILDATA INTEGRATION)
duplo <- soildata_events[duplo, V1]
soildata <- soildata[!id %in% duplo, ] # remove duplicated events
summary_soildata(soildata)
# Layers: 29881
# Events: 15729
# Georeferenced events: 13381



# Write data to disk ############################################################
# Export cleaned data
data.table::fwrite(soildata, "data/14_soildata_soc.txt", sep = "\t")

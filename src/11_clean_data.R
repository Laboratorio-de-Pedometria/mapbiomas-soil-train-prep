# title: MapBiomas Soil
# subtitle: Clean data
# author: Alessandro Samuel-Rosa
# data: 2025 CC-BY
rm(list = ls())

# Source helper functions
source("src/00_helper_functions.r")

# Read SoilData data processed in the previous script
soildata <- data.table::fread("data/10_soildata.txt", sep = "\t", na.strings = c("", "NA", "NaN"))
summary_soildata(soildata)
# Layers: 58718
# Events: 17561
# Georeferenced events: 14674
# Datasets: 261

# Clean datasets ###################################################################################
# ctb0042
# "Alteração do pH do solo por influência da diluição, tipo de solvente e tempo de contato"
# Contains data from a laboratory experiment, thus not representing real soil conditions required
# for digital soil mapping.
soildata <- soildata[dataset_id != "ctb0042", ]

# ctb0009
# "Variáveis pedogeoquímicas e mineralógicas na identificação de fontes de sedimentos em uma
# bacia hidrográfica de encosta"
# Contains soil data from roadsides and riversides. The data, however, is not yet available.
target <- c("carbono", "argila", "areia", "silte", "ph", "ctc", "dsi")
if (all(is.na(soildata[dataset_id == "ctb0009", ..target]))) {
  message("All soil properties are missing in dataset_id == 'ctb0009'. Removing this dataset.")
  soildata <- soildata[dataset_id != "ctb0009", ]
}

# ctb0001 - MAY BE USEFUL FOR VALIDATION
# "Conteúdo de ferro do solo sob dois sistemas de cultivo na Estação Experimental Terras Baixas nos
# anos de 2012 e 2013"
# soildata <- soildata[dataset_id != "ctb0001", ]

# ctb0026 - MAY BE USEFUL FOR VALIDATION
# "Conteúdo de ferro do solo no ano de 1998"
# soildata <- soildata[dataset_id != "ctb0026", ]

summary_soildata(soildata)
# Layers: 58660
# Events: 17503
# Georeferenced events: 14616
# Datasets: 260

# Clean layers #####################################################################################

# EQUAL DEPTH
# Some layers have equal values of 'profund_sup' and 'profund_inf'.
# Check if there is any
equal_depth <-
  soildata[!is.na(profund_sup) & !is.na(profund_inf) & profund_sup == profund_inf, id]
if (length(equal_depth) > 0) {
  stop(
    "Layers with equal values of 'profund_sup' and 'profund_inf' found in the following dataset(s):\n",
    paste(equal_depth, collapse = ", ")
  )
}

# NEGATIVE DEPTH
# Some topsoil layers have negative values of 'profund_sup' or 'profund_inf'. These are used to
# represent litter layers or organic layers above the mineral soil. For modelling purposes, however,
# we need to have all layers starting from 0 cm depth. We do so by moving the layer to start at 0 cm
# depth. We do so by getting the minimum value of 'profund_sup' for each event 'id' and adding the
# absolute value of this minimum to both 'profund_sup' and 'profund_inf'.
soildata[, min_profund_sup := min(profund_sup, na.rm = TRUE), by = id]
soildata[min_profund_sup < 0, profund_sup := profund_sup + abs(min_profund_sup)]
soildata[min_profund_sup < 0, profund_inf := profund_inf + abs(min_profund_sup)]
soildata[, min_profund_sup := NULL]
summary(soildata[, .(profund_sup, profund_inf)])

# LAYER ID (ORDER)
# Make sure that the layer id (camada_id) exists and is in the correct order
soildata <- soildata[order(id, profund_sup, profund_inf)]
soildata[, camada_id := 1:.N, by = id]

# TOPSOIL
# For each event ('id'), check if there is a topsoil layer, i.e., a layer with 'profund_sup' == 0.
# Missing a surface layer is common in reconnaissance soil surveys, where only the diagnostic 
# subsurface horizons are described and sampled. It can also occur in studies that use data from
# various sources and have a focus on subsurface horizons.
# Start by identifying events without a topsoil layer.
soildata[!is.na(profund_sup), has_topsoil := any(profund_sup == 0, na.rm = TRUE), by = id]
nrow(unique(soildata[has_topsoil != TRUE, "id"]))
# 738 events without a topsoil layer
print(soildata[has_topsoil != TRUE, .N, by = dataset_id][order(N)])
# This occurs in 48 datasets, but most of the events without a topsoil layer are from ctb0033 (476), 
# the second being ctb0770 (48). The absence of a top soil layer in ctb0033 happens because, for
# many soil profiles, soil samples for laboratory analysis were not collected from the entire soil
# horizon, but from its central part. Perhaps this was done because of the presence of a thin
# organic layer at the soil surface or coarse fragments that were not sampled. IN THE FUTURE, THIS
# SHOULD BE DEALT WITH WHEN PROCESSING THE RAW DATA FROM CTB0033.
soildata[has_topsoil != TRUE & dataset_id == "ctb0033", .N, by = id]
# For now, we will use a simple approach to deal with this issue: if the missing topsoil layer is
# less than 10 cm thick, we will add this thickness to existing topmost layer. This is done by
# setting the minimum value of 'profund_sup' to 0 cm for the first layer of each event 'id'.
miss_limit <- 10
soildata[, min_profund_sup := min(profund_sup), by = id]
soildata[min_profund_sup < miss_limit & camada_id == 1, profund_sup := 0]
# Recompute
soildata[!is.na(profund_sup), has_topsoil := any(profund_sup == 0, na.rm = TRUE), by = id]
nrow(unique(soildata[has_topsoil != TRUE, "id"]))
# Now, there are 264 events without a topsoil layer. We will keep them for now.
soildata[, has_topsoil := NULL]
soildata[, min_profund_sup := NULL]
summary(soildata[, .(profund_sup, profund_inf)])

# MISSING DEPTH
# Check if there are layers missing profund_sup or profund_inf
# Some datasets have missing depth values for some events. This occurs when 1) events are
# pseudo-samples, i.e. samples taken on the computer screen, 2) records of exposed R layers observed
# in the field, i.e. rock outcrops, or 3) auger holes used to check the soil classification in the
# field, but not sampled for laboratory analysis.
#  - ctb0044: pseudo-samples and rock outcrops
#  - ctb0047: auger holes
#  - ctb0048: auger holes
ctb_to_ignore <- c(
  "ctb0044", "ctb0047", "ctb0048",
  "ctb0052" # Gateados: process depths
)
soildata[, na_depth := is.na(profund_sup) | is.na(profund_inf)]
soildata[
  na_depth == TRUE & !dataset_id %in% ctb_to_ignore,
  .(id, camada_nome, profund_sup, profund_inf, argila, carbono, taxon_sibcs)
]
# Other datasets:
# - ctb0050: extra samples obtained from other datasets
# - ctb0051: extra samples obtained from other datasets
# - ctb0053: soil samples not collected or lost
# Filter out events from these three datasets
soildata <- soildata[!(na_depth == TRUE & dataset_id %in% c("ctb0050", "ctb0051", "ctb0053")), ]
soildata[, na_depth := NULL]
summary_soildata(soildata)
# Layers: 58284
# Events: 17127
# Georeferenced events: 14608
# Datasets: 260

# LITTER LAYERS
# Some datasets contain litter layers at the soil surface. These layers are identified by the
# use of H or O in camada_nome and carbono == NA & argila == NA. We start by identifying these
# layers (is_litter). We have to be careful here because we will drop some topsoil layers, which
# will create a situation similar to the one that we had before when dealing with missing topsoil
# layers. But this is a completely different situation! That is why we must also identify if a soil
# profile has a litter layer (has_litter), not just if a layer is a litter layer.
soildata[
  grepl("H|O", camada_nome, ignore.case = FALSE) & camada_id == 1 & is.na(carbono) & is.na(argila),
  is_litter := TRUE,
  by = id
]
soildata[, has_litter := any(is_litter == TRUE), by = id]
soildata[is_litter == TRUE, .N] # 166 layers
soildata[has_litter == TRUE, .N, by = id] # 166 events had litter layers
soildata[, .N, by = is_litter]
# View litter layers
if (FALSE) {
  View(soildata[
    is_litter == TRUE,
    .(id, camada_nome, camada_id, profund_sup, profund_inf, carbono, argila, taxon_sibcs)
  ])
}
# View events with litter layers
if (FALSE) {
  View(soildata[
    has_litter == TRUE,
    .(id, camada_nome, camada_id, profund_sup, profund_inf, carbono, argila, taxon_sibcs)
  ])
}
# We will remove these litter layers from the dataset and reset the layer ids (camada_id).
soildata <- soildata[is.na(is_litter), ]
soildata[, is_litter := NULL]
soildata[, camada_id := 1:.N, by = id]
summary_soildata(soildata)
# Layers: 58118
# Events: 17127
# Georeferenced events: 14608
# Datasets: 260
# Check if there are still litter layers
soildata[
  grepl("H|O", camada_nome, ignore.case = FALSE) & camada_id == 1 & is.na(carbono) & is.na(argila),
  is_litter := TRUE,
  by = id
]
soildata[is_litter == TRUE, .N] # 16 layers
soildata[has_litter == TRUE, .N, by = id] # 166 events had litter layers
soildata[, .N, by = is_litter]
# View examples of litter layers
if (FALSE) {
  View(soildata[
    is_litter == TRUE,
    .(id, camada_nome, camada_id, profund_sup, profund_inf, carbono, argila, taxon_sibcs)
  ])
}
# We will remove these litter layers from the dataset and reset the layer ids (camada_id).
soildata <- soildata[is.na(is_litter), ]
soildata[, is_litter := NULL]
soildata[, camada_id := 1:.N, by = id]
summary_soildata(soildata)
# Layers: 58102
# Events: 17126: WE LOST 1 EVENT HERE!
# Georeferenced events: 14608
# Datasets: 260
# Check if there are still litter layers
soildata[
  grepl("H|O", camada_nome, ignore.case = FALSE) & camada_id == 1 & is.na(carbono) & is.na(argila),
  is_litter := TRUE,
  by = id
]
soildata[is_litter == TRUE, .N] # 0 layers
soildata[has_litter == TRUE, .N, by = id] # 165 events had litter layers... this means that we
# lost some complete events when removing litter layers. We will deal with this later. ATTENTION!
summary_soildata(soildata)
# Layers: 58102
# Events: 17126
# Georeferenced events: 14608
# Datasets: 260
# Adjust depths of the remaining layers of the events that had litter layers. We do so by
# getting the minimum value of 'profund_sup' for each event 'id' that had litter layers removed and
# subtracting this value from both 'profund_sup' and 'profund_inf'.
soildata[has_litter == TRUE, min_profund_sup := min(profund_sup, na.rm = TRUE), by = id]
# Most cases are between 0 and 10 cm, but there are cases as high as 60 cm.
soildata[has_litter == TRUE, .N, by = min_profund_sup][order(min_profund_sup)]
# View events with min_profund_sup >= 20: all of them are from dataset_id == "ctb0753"
# PROJETO RADAMBRASIL - Levantamento de Recursos Naturais. Volume 18. There layers seem to be layers
# of completely decomposed organic material above the mineral soil. WE SHOULD INVESTIGATE THIS 
# FURTHER. For now, we will simply adjust the depths.
soildata[has_litter == TRUE & min_profund_sup >= 20, id]
# Adjust depths
soildata[has_litter == TRUE, profund_sup := profund_sup - min_profund_sup]
soildata[has_litter == TRUE, profund_inf := profund_inf - min_profund_sup]
soildata[, min_profund_sup := NULL]
all(soildata[has_litter == TRUE & camada_id == 1, profund_sup] == 0) # should all be 0 now







# SOIL/NON-SOIL LAYERS
# The variable 'is_soil' identifies if a soil layer is considered a "true" soil layer or not.
# - A non soil layer generally is represented using the capital letter "R" in the layer name.
soildata[, is_soil := !grepl("^R$", camada_nome, ignore.case = FALSE)]
soildata[is_soil == FALSE, .N] # 281 layers
# - Older studies may use the letter "D" such as in ctb0674 and ctb0787 to represent the bedrock or
#   saprolithic material. We will consider these layers as non-soil layers when they lack data on
#   carbon or clay content.
soildata[camada_nome == "D" & (is.na(argila) | is.na(carbono)), is_soil := FALSE]
soildata[is_soil == FALSE, .N] # 299 layers
# - Some researchers use the symbols CR and RCr to represent the bedrock or hard saprolithic
#   material, such as ctb0005, ctb0006, ctb0025, ctb0030. Note that most of these studies were
#   carried out in the south of Brazil. We will consider these layers as non-soil layers when they
#   lack data on carbon or clay content.
soildata[
  grepl("^CR|RCr$", camada_nome, ignore.case = FALSE) & (is.na(argila) | is.na(carbono)),
  is_soil := FALSE
]
soildata[is_soil == FALSE, .N] # 335 layers
# - We may also find designations such as 2C/R, 2C/R, 2C/R, 2RC, 2RC, 2RC, C/CR, and C/R. These
#   designations indicate that the layer is a transition between a soil horizon and the bedrock. We
#   will consider these layers as non-soil layers when they lack data on carbon or clay content.
soildata[
  grepl("C/CR$|C/R$|RC$|RCr$|2C/RC$|2C/R$|2RC$|2RCr$", camada_nome, ignore.case = FALSE) &
    (is.na(argila) | is.na(carbono)),
  is_soil := FALSE
]
soildata[is_soil == FALSE, .N] # 343 layers

# Special cases
# ctb0003
# The study planned to sample the 0-20 cm layer only. When the soil was shallower than 20 cm, the
# soil was sampled until the bedrock. Thus, if profund_inf < 20 cm, add a 20 cm thick layer
# starting from profund_inf and name it "R".
ctb0003 <- soildata[dataset_id == "ctb0003" & profund_inf < 20, ]
ctb0003[, profund_sup := profund_inf]
ctb0003[, profund_inf := profund_sup + (20 - profund_sup)]
ctb0003[, camada_nome := "R"]
ctb0003[, is_soil := FALSE]
soildata <- rbind(soildata, ctb0003)
# sort data
soildata <- soildata[order(id, profund_sup, profund_inf)]
soildata[is_soil == FALSE, .N] # 456 layers

# Check multiple endpoints per event
# For each 'id', count the number of layers where is_soil == FALSE. If there are multiple layers
# where is_soil == FALSE, print the 'camada_nome' of these layers. We expect only one non-soil layer
# per event, representing the bedrock. However, for now, we will ignore these cases and deal with
# them later on.
soildata[, multiple_endpoints := sum(is_soil == FALSE), by = id]
unique(soildata[multiple_endpoints > 1 & is_soil == FALSE, camada_nome])

# For each 'id', identify the layer with the maximum profund_inf and print 'camada_nome' 
soildata[, max_profund_inf := max(profund_inf, na.rm = TRUE), by = id]
soildata[max_profund_inf == profund_inf & is_soil == FALSE, .N, by = camada_nome][order(N)]
if (FALSE) {
  View(soildata[max_profund_inf == profund_inf & is_soil == TRUE, .N,
    by = camada_nome
  ][order(camada_nome)])
}



# MAXIMUM DEPTH
# Filter out soil layers starting below the maximum depth. We will work only with data from layers
# starting from the soil surface down to max_depth.
max_depth <- 100
nrow(soildata[profund_sup >= max_depth, ]) # 8096 layers with profund_sup >= max_depth
soildata <- soildata[profund_sup >= 0 & profund_sup <= max_depth, ]
summary_soildata(soildata)
# Layers: 51229
# Events: 17028
# Georeferenced events: 14396
# Datasets: 260












# MISSING LAYERS
# Check for missing layers within each event (id)
print(id_missing <- check_missing_layer(soildata))
# There are 9864 complaints.
if(FALSE) {
  View(soildata[id %in% id_missing$id, .(id, camada_nome, profund_sup, profund_inf, argila, carbono)])
}
# If the soil classification (taxon_sibcs) is Latossolo, Neossolo Quartzarênico, or Gleissolo, we
# will add the missing layers, as these soils are quite homogeneous in the vertical profile.
unique(soildata[id %in% id_missing$id & grepl("Latossol|Quartz|Gleissol", taxon_sibcs), taxon_sibcs])
# Add missing layers for these events
soildata[id %in% id_missing$id & grepl("Latossol|Quartz|Gleissol", taxon_sibcs), ] <-
  add_missing_layer(soildata[id %in% id_missing$id & grepl("Latossol|Quartz|Gleissol", taxon_sibcs), ])





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

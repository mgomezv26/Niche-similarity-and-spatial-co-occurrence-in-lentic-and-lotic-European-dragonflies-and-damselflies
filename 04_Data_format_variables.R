"
###########################################
##           LOAD LIBRARIES              ##
###########################################
R version 4.5.1 (2025-06-13 ucrt)
"
library(tidyverse)
library(dplyr)
library(hypervolume)
library(Rcpp)
library(sf)
library(terra)

cat("Versions of the libraries used:\n")
cat("tidyverse:", as.character(packageVersion("tidyverse")), "\n") # tidyverse: 2.0.0 
cat("dplyr:", as.character(packageVersion("dplyr")), "\n") # dplyr: 1.1.4
cat("Rcpp:", as.character(packageVersion("Rcpp")), "\n") # Rcpp: 1.1.0
cat("sf:", as.character(packageVersion("sf")), "\n") # sf: 1.0.21
cat("terra:", as.character(packageVersion("terra")), "\n") # terra: 1.8.54

"
Four species are eliminated due to their very small hypervolumes, because they do not have enougth 'random points' and 'minimum density'. 
The first three species are from the lentic group and the last is from the lotic group:
#hv25 = sp_14 = Anax junius (Drury, 1773)
#hv60 = sp_67 = Pantala flavescens (Fabricius, 1798)
#hv81 = sp_91 = Coenagrion hylas (Trybom, 1889)
#hv10 = sp_21 = Cordulegaster helladica (Lohmann, 1993)
"

"
# -----------------------------------------------------------------------------------------------#
# LOAD STUDY AREA
# -----------------------------------------------------------------------------------------------#
"

study_area <- st_read("01_Data/01_Study_Area/Study_Area_WGS_84_CLC.gpkg")
length(unique(study_area$ET_ID)) #2317
ET_IDS_list = unique(study_area$ET_ID)

"
# -----------------------------------------------------------------------------------------------#
# 1. DATA ELEVATION
# -----------------------------------------------------------------------------------------------#

Elevation variables (mean, minimum, maximum, and range) were derived from species distributions using a digital elevation model 
based on the GMTED2010 dataset (U.S. Geological Survey and National Geospatial-Intelligence Agency), 
previously processed by the research team and projected to WGS84. 

Below we show the code for processing the DEM (raster) to extract elevation data. 
However, the raster is not available in this repository, only the extracted and processed data.
"
# 
# dem_elevation <- rast("Data/dem_elevation_WGS84.tif") # The raster is not available in the repository to save space.
# crs(dem_elevation)
# 
# # Check and align projections
# study_area <- project(study_area, crs(dem_elevation))
# 
# # Cut the raster
# raster_crop <- crop(dem_elevation, study_area)  # Cut off the extension
# raster_mask <- mask(raster_crop, study_area)    # Apply the mask
# 
# # Extract elevation data by grid cell of the study area ------------------------------------------
# ##  Extract statistics (average, minimum, maximum)
# stats <- extract(raster_mask, study_area, fun = function(x) {
#   c(mean = mean(x, na.rm = TRUE), 
#     min = min(x, na.rm = TRUE), 
#     max = max(x, na.rm = TRUE))
# })
# 
# # Convert to data.frame and assign names to columns
# stats_df <- as.data.frame(stats)
# colnames(stats_df) <- c("ID", "mean_elev", "min_elev", "max_elev")
# 
# # Join statistics to the shapefile based on the ID
# study_area$ID <- 1:nrow(study_area)
# study_area <- merge(study_area, stats_df, by = "ID", all.x = TRUE)
# 
# study_area_df <- as.data.frame(study_area)
# study_area_df <- select(study_area_df, -ID)
# 
# #Calculate elevation range
# study_area_df$range_elev <- study_area_df$max_elev - study_area_df$min_elev
# # Save elevation data. 
# #write.csv(study_area_df, "elevation_study_area.csv") # This is the CSV file available in the repository.

# LOAD ELEVATION DATA FROM THE REPOSITORY (GitHub) -------------------------------------------------------------------

elevation_study_area <- read.csv("01_Data/02_Variables/elevation_study_area.csv")
#head(elevation_study_area)
#length(unique(elevation_study_area$ET_ID))

elevation_study_area <- elevation_study_area %>% 
  filter(ET_ID %in% ET_IDS_list)

# Lentic --------------------------------------------------------------------------------------------

lentic_presence_data_long <- read.csv("01_Data/03_Data_species/lentic_presence_data_long.csv")
#head(lentic_presence_data_long)
#length(unique(lentic_presence_data_long$ET_ID))

lentic_presence_data_long <- lentic_presence_data_long %>%
  filter(ET_ID %in% ET_IDS_list)

lentic_presence_data_long <- lentic_presence_data_long %>% filter(!(specie %in% c("sp_14", "sp_67", "sp_91")))
length(unique(lentic_presence_data_long$specie))

data_joined <- lentic_presence_data_long %>%
  left_join(elevation_study_area, by = "ET_ID")

mean_elevation_by_species <- data_joined %>%
  group_by(specie) %>%
  summarise(mean_mean_elev = mean(mean_elev, na.rm = TRUE),
            mean_min_elev = mean(min_elev, na.rm = TRUE),
            mean_max_elev = mean(max_elev, na.rm = TRUE),
            mean_range_elev = mean(range_elev, na.rm = TRUE)) %>%
  ungroup()

Elevation_lentic <- mean_elevation_by_species

# Lotic --------------------------------------------------------------------------------------------

lotic_presence_data_long <- read.csv("01_Data/03_Data_species/lotic_presence_data_long.csv")
#head(lotic_presence_data_long)
#length(unique(lotic_presence_data_long$ET_ID))
lotic_presence_data_long <- lotic_presence_data_long %>% 
  filter(ET_ID %in% ET_IDS_list)

lotic_presence_data_long <- lotic_presence_data_long %>% filter(!(specie %in% c("sp_124", "sp_21")))

data_joined <- lotic_presence_data_long %>%
  left_join(elevation_study_area, by = "ET_ID")
#length(unique(data_joined$specie))

mean_elevation_by_species <- data_joined %>%
  group_by(specie) %>%
  summarise(mean_mean_elev = mean(mean_elev, na.rm = TRUE),
            mean_min_elev = mean(min_elev, na.rm = TRUE),
            mean_max_elev = mean(max_elev, na.rm = TRUE),
            mean_range_elev = mean(range_elev, na.rm = TRUE)) %>%
  ungroup()

Elevation_lotic <- mean_elevation_by_species

"
# -----------------------------------------------------------------------------------------------#
# 2. HABITATS RICHNESS 
# -----------------------------------------------------------------------------------------------#

Habitat heterogeneity was estimated using the CORINE Land Cover dataset (CLC 2018, version 2020_20u1; European Union’s Copernicus Land Monitoring Service), 
with land-cover categories aggregated into broader ecological classes (see Supplementary Appendix S1).
Habitat richness and Shannon diversity were calculated per species using a spatially explicit processing workflow implemented in Python (see script 03_Processing_CLC_Python in the GitHub repository).
Here, we directly import the resulting habitat richness and Shannon diversity metrics from the repository.
"

richness_habitats <- read.csv("01_Data/02_Variables/richness_habitats.csv")
#head(richness_habitats)
#length(unique(richness_habitats$ET_ID))
richness_habitats = distinct(richness_habitats)

# Lentic --------------------------------------------------------------------------------------------

data_joined <- lentic_presence_data_long %>%
  merge(richness_habitats, by = "ET_ID")

mean_richness_habitats_by_species <- data_joined %>%
  group_by(specie) %>%
  summarise(mean_richness_h = mean(Habitat_Richness, na.rm = TRUE)) %>%
  ungroup()

Richness_H_lentic <- mean_richness_habitats_by_species

# Lotic --------------------------------------------------------------------------------------------

data_joined <- lotic_presence_data_long %>%
  left_join(richness_habitats, by = "ET_ID")

mean_richness_habitats_by_species <- data_joined %>%
  group_by(specie) %>%
  summarise(mean_richness_h = mean(Habitat_Richness, na.rm = TRUE)) %>%
  ungroup()

Richness_H_lotic <- mean_richness_habitats_by_species

"
# -----------------------------------------------------------------------------------------------#
# 4. HABITATS DIVERSITY (SHANON)
# -----------------------------------------------------------------------------------------------#
"

Shannon_index_habitats <- read.csv("01_Data/02_Variables/Shannon_index_habitats.csv")
#head(Shannon_index_habitats)
#length(unique(Shannon_index_habitats$ET_ID))
Shannon_index_habitats = distinct(Shannon_index_habitats)


# Lentic --------------------------------------------------------------------------------------------
data_joined <- lentic_presence_data_long %>%
  merge(Shannon_index_habitats, by = "ET_ID")

mean_Shannon_index_habitats_by_species <- data_joined %>%
  group_by(specie) %>%
  summarise(mean_shannon_h = mean(H_shannon, na.rm = TRUE)) %>%
  ungroup()

Shannon_H_lentic <- mean_Shannon_index_habitats_by_species

# Lotic --------------------------------------------------------------------------------------------
data_joined <- lotic_presence_data_long %>%
  left_join(Shannon_index_habitats, by = "ET_ID")

mean_Shannon_index_habitats_by_species <- data_joined %>%
  group_by(specie) %>%
  summarise(mean_shannon_h = mean(H_shannon, na.rm = TRUE)) %>%
  ungroup()

Shannon_H_lotic <- mean_Shannon_index_habitats_by_species

"
# -----------------------------------------------------------------------------------------------#
# 3. VOLUME OF THE HYPERVOLUMES
# -----------------------------------------------------------------------------------------------#
"

# Lentic --------------------------------------------------------------------------------------------
load("02_Hypervolume/hv_list_Lentic_sp.RData")
#hv_list_Species

# Convert HypervolumeList to normal list
results_list <- hv_list_Species@HVList

# Delete items 25, 60, and 81
results_list_lentic = results_list[-c(25, 60, 81)]
#length(results_list_lentic)

# Transform to hypervolumes 
hv_list <- hypervolume_join(results_list_lentic)
#hv_list

volume_Species<-as.data.frame(get_volume(hv_list))

colnames(volume_Species) <- c("volume")
volume_Species$specie <- rownames(volume_Species)
volume_Species <- volume_Species[, c("specie", "volume")]
#write.csv(volume_Species, "02_Hypervolume/Data_Volume_Lentic.csv")

Volume_lentic <- volume_Species

# Lotic --------------------------------------------------------------------------------------------

load("02_Hypervolume/hv_list_Lotic_sp.RData")
#hv_list_Species

results_list <- hv_list_Species@HVList
results_list_lotic = results_list[-10]
#length(results_list_lotic)
hv_list <- hypervolume_join(results_list_lotic)

volume_Species<-as.data.frame(get_volume(hv_list))

colnames(volume_Species) <- c("volume")
volume_Species$specie <- rownames(volume_Species)
volume_Species <- volume_Species[, c("specie", "volume")]
#write.csv(volume_Species, "02_Hypervolume/Data_Volume_Lotic.csv")

Volume_lotic <- volume_Species

"
# -----------------------------------------------------------------------------------------------#
# 6. JOIN ALL VALUES IN ONE DF
# -----------------------------------------------------------------------------------------------#
"

# Lentic --------------------------------------------------------------------------------------------

#Elevation_lentic
#Richness_H_lentic
#Shannon_H_lentic
#Volume_lentic
Climatic_Ranges <- read.csv("01_Data/02_Variables/Climatic_Ranges_lentic.csv")

length(unique(Elevation$specie))
length(unique(Richness_H$specie))
length(unique(Shannon_H$specie))
length(unique(Volume$specie))

colnames(Elevation)
colnames(Richness_H)
colnames(Shannon_H)
colnames(Volume)
colnames(Climatic_Ranges)

Elevation <- Elevation %>% select(-X)
Richness_H <- Richness_H %>% select(-X)
Shannon_H <- Shannon_H %>% select(-X)
Volume <- Volume %>% select(-X)
Climatic_Ranges <- Climatic_Ranges %>% select(specie, Multi_Climate_Range)

Data_Analysis_Lentic <- Elevation %>%
  full_join(Richness_H, by = "specie") %>%
  full_join(Shannon_H, by = "specie") %>%
  full_join(Volume, by = "specie")%>%
  full_join(Climatic_Ranges, by = "specie")

head(Data_Analysis_Lentic)
str(Data_Analysis_Lentic)

length(unique(Data_Analysis_Lentic$specie))
Data_Analysis_Lentic <- Data_Analysis_Lentic %>% filter(!(specie %in% c("sp_14", "sp_67", "sp_91")))

write.csv(Data_Analysis_Lentic, "01_DATA/04_Data_Analysis/Data_Analysis_Lentic.csv")


# Lotic --------------------------------------------------------------------------------------------

Elevation_lotic
Richness_H_lotic
Shannon_H_lotic
Volume_lotic
Climatic_Ranges <- read.csv("01_Data/02_Variables/Climatic_Ranges_lotic.csv")

length(unique(Elevation$specie))
length(unique(Richness_H$specie))
length(unique(Shannon_H$specie))
length(unique(Volume$specie))
length(unique(Climatic_Ranges$specie))

colnames(Elevation)
colnames(Richness_H)
colnames(Shannon_H)
colnames(Volume)
colnames(Climatic_Ranges)


Elevation <- Elevation %>% select(-X)
Richness_H <- Richness_H %>% select(-X)
Shannon_H <- Shannon_H %>% select(-X)
Volume <- Volume %>% select(-X)
Climatic_Ranges <- Climatic_Ranges %>% select(specie, Multi_Climate_Range)


Data_Analysis_Lotic <- Elevation %>%
  full_join(Richness_H, by = "specie") %>%
  full_join(Shannon_H, by = "specie") %>%
  full_join(Volume, by = "specie")%>%
  full_join(Climatic_Ranges, by = "specie")

head(Data_Analysis_Lotic)
str(Data_Analysis_Lotic)

length(unique(Data_Analysis_Lotic$specie))
Data_Analysis_Lotic <- Data_Analysis_Lotic %>% filter(!(specie %in% c("sp_124", "sp_21")))

write.csv(Data_Analysis_Lotic, "01_DATA/04_Data_Analysis/Data_Analysis_Lotic.csv")


# -----------------------------------------------------------------------------------------------#
# 7. TRANSFORMATION TO MATRIX FORMAT - JACCARD INDEX
# -----------------------------------------------------------------------------------------------#

library(tidyverse)
library(dplyr)

# Lentic --------------------------------------------------------------------------------------------
lentic__jac <- read.csv("02_Hypervolume/Results_Overlap_pairs_wise/results_overlap_Lentic.csv")

Lentic_Jaccard <- lentic__jac %>%
  select(sp1, sp2, jaccard)
Lentic_Jaccard <- Lentic_Jaccard %>%
  filter(!(sp1 %in% species_drop | sp2 %in% species_drop))

# Create similarity matrix
Lentic_MJaccard <- Lentic_Jaccard %>%
  bind_rows(Lentic_Jaccard %>% rename(sp1 = sp2, sp2 = sp1)) %>%
  pivot_wider(names_from = sp2, values_from = jaccard, values_fill = 0) %>%
  column_to_rownames("sp1")

write.csv(Lentic_MJaccard, "01_DATA/04_Data_Analysis/01_Lentic/MJacc_lentic.csv")


# Lotic --------------------------------------------------------------------------------------------

lotic__jac <- read.csv("02_Hypervolume/Results_Overlap_pairs_wise/results_overlap_Lotic.csv")

Lotic_Jaccard <- lotic__jac %>%
  select(sp1, sp2, jaccard)

Lotic_MJaccard <- Lotic_Jaccard %>%
  bind_rows(Lotic_Jaccard %>% rename(sp1 = sp2, sp2 = sp1)) %>%
  pivot_wider(names_from = sp2, values_from = jaccard, values_fill = 0) %>%
  column_to_rownames("sp1")

write.csv(Lotic_MJaccard, "01_DATA/04_Data_Analysis/02_Lotic/MJacc_lotic.csv")

# Lentic vs Lotic species ----------------------------

LenticVSLotic <- read.csv("02_Hypervolume/Results_Overlap_pairs_wise/results_overlap_all_species.csv")
LenticVSLotic_Jaccard <- LenticVSLotic %>%
  select(sp1, sp2, jaccard)

LenticVSLotic_MJaccard <- LenticVSLotic_Jaccard %>%
  bind_rows(LenticVSLotic_Jaccard %>% rename(sp1 = sp2, sp2 = sp1)) %>%
  pivot_wider(names_from = sp2, values_from = jaccard, values_fill = 0) %>%
  column_to_rownames("sp1")

write.csv(LenticVSLotic_MJaccard, "01_DATA/04_Data_Analysis/03_LenticVSLotic/MJacc_LenticVSLotic.csv")


# -----------------------------------------------------------------------------------------------#
# 8. TRANSFORMATION TO MATRIX FORMAT - VARIABLES
# -----------------------------------------------------------------------------------------------#

# Lentic --------------------------------------------------------------------------------------------

Lentic_Data <- read.csv("01_DATA/04_Data_Analysis/Data_Analysis_Lentic.csv")

Lentic_Data <- Lentic_Data %>% select(-X)
str(Lentic_Data)

# MODIFICACION DE DATOS PARA VALORES MEDIOS Y DIFERENCIAS

# Variables para diferencia
vars_diferencia <- c("mean_mean_elev", "mean_max_elev", "mean_min_elev")

# Variables para promedio
vars_promedio <- c("mean_range_elev", "mean_richness_h", "mean_shannon_h", "volume", "Multi_Climate_Range")

# FUNCION PARA DIFERENCIAS
matriz_diferencias <- function(var) {
  x <- lentic_vars[[var]]
  outer(x, x, FUN = function(a, b) abs(a - b)) %>%
    as.data.frame() %>%
    `colnames<-`(rownames(lentic_vars)) %>%
    `rownames<-`(rownames(lentic_vars))
}

# FUNCION VALORES MEDIOS

matriz_promedios <- function(var) {
  x <- lentic_vars[[var]]
  outer(x, x, FUN = function(a, b) (a + b)/2) %>%
    as.data.frame() %>%
    `colnames<-`(rownames(lentic_vars)) %>%
    `rownames<-`(rownames(lentic_vars))
}

# Convierte species a rownames
lentic_vars <- Lentic_Data %>%
  select(-X, -AOO) %>%
  column_to_rownames("specie")

# Matrices de diferencias
list_diferencias <- lapply(vars_diferencia, matriz_diferencias)
names(list_diferencias) <- vars_diferencia

# Matrices de promedios
list_promedios <- lapply(vars_promedio, matriz_promedios)
names(list_promedios) <- vars_promedio

# save data
for (var in names(list_diferencias)) {
  write.csv(list_diferencias[[var]],
            file = paste0("01_DATA/04_Data_Analysis/01_Lentic/Mdif_", var, ".csv"),
            row.names = TRUE)
}

for (var in names(list_promedios)) {
  write.csv(list_promedios[[var]],
            file = paste0("01_DATA/04_Data_Analysis/01_Lentic/Mmean_", var, ".csv"),
            row.names = TRUE)
}


# Lotic --------------------------------------------------------------------------------------------
Lotic_Data <- read.csv("01_DATA/05_Data_Analysis/Data_Analysis_Lotic.csv")

# MODIFICACION DE DATOS PARA VALORES MEDIOS Y DIFERENCIAS

# Variables para diferencia
vars_diferencia <- c("mean_mean_elev", "mean_max_elev", "mean_min_elev")

# Variables para promedio
vars_promedio <- c("mean_range_elev", "mean_richness_h", "mean_shannon_h", "volume", "Multi_Climate_Range")

# FUNCION PARA DIFERENCIAS
matriz_diferencias <- function(var) {
  x <- data_vars[[var]]
  outer(x, x, FUN = function(a, b) abs(a - b)) %>%
    as.data.frame() %>%
    `colnames<-`(rownames(data_vars)) %>%
    `rownames<-`(rownames(data_vars))
}

# FUNCION VALORES MEDIOS

matriz_promedios <- function(var) {
  x <- data_vars[[var]]
  outer(x, x, FUN = function(a, b) (a + b)/2) %>%
    as.data.frame() %>%
    `colnames<-`(rownames(data_vars)) %>%
    `rownames<-`(rownames(data_vars))
}

# Convierte species a rownames (si no lo hiciste ya)
data_vars <- Lotic_Data %>%
  select(-X, -AOO) %>%
  column_to_rownames("specie")

# Matrices de diferencias
list_diferencias <- lapply(vars_diferencia, matriz_diferencias)
names(list_diferencias) <- vars_diferencia

# Matrices de promedios
list_promedios <- lapply(vars_promedio, matriz_promedios)
names(list_promedios) <- vars_promedio

# save data
for (var in names(list_diferencias)) {
  write.csv(list_diferencias[[var]],
            file = paste0("01_DATA/04_Data_Analysis/02_Lotic/Mdif_", var, ".csv"),
            row.names = TRUE)
}

for (var in names(list_promedios)) {
  write.csv(list_promedios[[var]],
            file = paste0("01_DATA/04_Data_Analysis/02_Lotic/Mmean_", var, ".csv"),
            row.names = TRUE)
}




# 3. LenticVSLotic DATA -------------

# MODIFICACION DE DATOS PARA VALORES MEDIOS Y DIFERENCIAS

# Variables para diferencia
vars_diferencia <- c("mean_mean_elev", "mean_max_elev", "mean_min_elev")

# Variables para promedio
vars_promedio <- c("mean_range_elev", "mean_richness_h", "mean_shannon_h", "volume", "Multi_Climate_Range")

# FUNCION PARA DIFERENCIAS
matriz_diferencias <- function(var) {
  x <- data_vars[[var]]
  outer(x, x, FUN = function(a, b) abs(a - b)) %>%
    as.data.frame() %>%
    `colnames<-`(rownames(data_vars)) %>%
    `rownames<-`(rownames(data_vars))
}

# FUNCION VALORES MEDIOS

matriz_promedios <- function(var) {
  x <- data_vars[[var]]
  outer(x, x, FUN = function(a, b) (a + b)/2) %>%
    as.data.frame() %>%
    `colnames<-`(rownames(data_vars)) %>%
    `rownames<-`(rownames(data_vars))
}

Lentic_Data$group <- "Lentico"
Lotic_Data$group <- "Lotico"

combined_data <- rbind(Lentic_Data, Lotic_Data)
str(combined_data)

# Convierte species a rownames (si no lo hiciste ya)
data_vars <- combined_data %>%
  select(-X) %>%
  column_to_rownames("specie")

# Matrices de diferencias
list_diferencias <- lapply(vars_diferencia, matriz_diferencias)
names(list_diferencias) <- vars_diferencia

# Matrices de promedios
list_promedios <- lapply(vars_promedio, matriz_promedios)
names(list_promedios) <- vars_promedio


# save data
for (var in names(list_diferencias)) {
  write.csv(list_diferencias[[var]],
            file = paste0("01_DATA/04_Data_Analysis/03_LenticVSLotic/Mdif_", var, ".csv"),
            row.names = TRUE)
}

for (var in names(list_promedios)) {
  write.csv(list_promedios[[var]],
            file = paste0("01_DATA/04_Data_Analysis/03_LenticVSLotic/Mmean_", var, ".csv"),
            row.names = TRUE)
}



"
###########################################
##           LOAD LIBRARIES              ##
###########################################
R version 4.5.1 (2025-06-13 ucrt)
"
library(tidyverse)
library(dplyr)
library(GGally)
library(geodata)
library(raster)
library(sf)
library(hypervolume)
library(rgl)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)


cat("Versions of the libraries used:\n")
cat("tidyverse:", as.character(packageVersion("tidyverse")), "\n") # tidyverse: 2.0.0 
cat("dplyr:", as.character(packageVersion("dplyr")), "\n") # dplyr: 1.1.4 
cat("GGally:", as.character(packageVersion("GGally")), "\n") # GGally: 2.4.0
cat("geodata:", as.character(packageVersion("geodata")), "\n") # geodata: 0.6.6
cat("raster:", as.character(packageVersion("raster")), "\n") # raster: 3.6.32
cat("sf:", as.character(packageVersion("sf")), "\n") # sf: 1.0.21
cat("hypervolume:", as.character(packageVersion("hypervolume")), "\n") # hypervolume: 3.1.6
cat("rgl:", as.character(packageVersion("rgl")), "\n") # rgl: 1.3.24

cat("ggplot2:", as.character(packageVersion("ggplot2")), "\n") # ggplot2: 4.0.0 
cat("rnaturalearth:", as.character(packageVersion("rnaturalearth")), "\n") # rnaturalearth: 1.1.0
cat("rnaturalearthdata:", as.character(packageVersion("rnaturalearthdata")), "\n") # rnaturalearthdata: 1.0.0 

"
# -----------------------------------------------------------------------------------------------#
# LOAD DATA SPECIES
# -----------------------------------------------------------------------------------------------#

The data used for the statistical analysis are stored in the ‘01_Data/03_Data_species’ folder. This folder contains 3 files in CSV anf excel format, 
where the delimiter is ';' and the decimal separator is '.'. 

The available files are:

1) lentic_presence_data_long.csv: 
Records of presence for lentic species.
Each row in the dataset refers to a 50 x 50 km geographical grid.
The dataset contains a total of three variables.

The variables are:

- ET_ID: The ID of each cell in the geospatial grid corresponding to the study area
- specie : the indicator of the species. ID_Grid in excel file 'List_of_species.xlsx'. 
- presence: Presence register in the grid

2) lotic_presence_data_long.csv: 
Records of presence for lotic species.
Each row in the dataset refers to a 50 x 50 km geographical grid.
The dataset contains a total of three variables.

The variables are:

- ET_ID: The ID of each cell in the geospatial grid corresponding to the study area
- specie : the indicator of the species. ID_Grid in excel file 'List_of_species.xlsx'. 
- presence: Presence register in the grid

"

lentic_presence_data_long <- read.csv("01_Data/03_Data_species/lentic_presence_data_long.csv")
head(lentic_presence_data_long)

lotic_presence_data_long <- read.csv("01_Data/03_Data_species/lotic_presence_data_long.csv")
head(lotic_presence_data_long)

"
# -----------------------------------------------------------------------------------------------#
# 2. WORLDCLIM DATA
# -----------------------------------------------------------------------------------------------#
"

# Download
climatelayers <- worldclim_global(var='bio', res=10,  path=tempdir())
names(climatelayers)

# Select variables (BIO1, BIO5, BIO12, BIO15, BIO18)
climatelayers_ss <- climatelayers[[c(1, 12, 4, 15, 8)]]

# Conversion from terra SpatRaster to raster RasterLayer
climatelayers_ss_2 <- lapply(1:nlyr(climatelayers_ss), function(i) {
  raster(climatelayers_ss[[i]])
})

# Assign names to converted layers
names(climatelayers_ss_2) <- c("BIO_01", "BIO_12", "BIO_04", "BIO_15", "BIO_08")
names(climatelayers_ss_2)

# Combine all layers into one RasterStack
climatelayers_ss_2_stack <- stack(climatelayers_ss_2)
class(climatelayers_ss_2_stack)

# z-transform climate layers to make axes comparable
for (i in 1:nlayers(climatelayers_ss_2_stack))
{
  climatelayers_ss_2_stack[[i]] <- 
  (climatelayers_ss_2_stack[[i]] - cellStats(climatelayers_ss_2_stack[[i]], 'mean')) /
  cellStats(climatelayers_ss_2_stack[[i]], 'sd') 
}

"
# -----------------------------------------------------------------------------------------------#
# 3. STUDY AREA
# -----------------------------------------------------------------------------------------------#
"

study_area <- st_read("01_Data/01_Study_Area/Study_Area_WGS_84_CLC.gpkg")
crs(study_area)

# Reprojecting the study area
study_area <- st_transform(study_area, crs = crs(climatelayers_ss_2_stack))

# Extract coordinates of centroids
centroides <- st_centroid(study_area)
centroides_coords <- st_coordinates(centroides)

study_area$x <- centroides_coords[, 1]
study_area$y <- centroides_coords[, 2]

# X = Longitude; Y = Latitude
study_area = study_area %>%
  rename(Longitude = x, Latitude = y)
colnames(study_area)

# print(extent(study_area))
# print(extent(climatelayers_ss_2_stack))

climatelayers_ss_2_stack <- crop(climatelayers_ss_2_stack, extent(-11,36,30,72))
#plot(climatelayers_ss_2_stack[[1]], main = "Bio 1")
#writeRaster(climatelayers_ss_2_stack, filename="01_Data/04_Data_clim/climatelayers_stack.tif", format="GTiff", overwrite=TRUE)

"
# -----------------------------------------------------------------------------------------------#
# MAP OF THE STUDY AREA
# -----------------------------------------------------------------------------------------------#
"

# Load base map (continents at medium scale)
world <- ne_countries(scale = "medium", returnclass = "sf")

study_area_clean <- study_area %>%
  distinct(ET_ID, Longitude, Latitude, .keep_all = TRUE)

bbox_grid <- st_bbox(study_area_clean)
world_crop <- st_crop(world, bbox_grid)
map_zone <- ggplot() +
  geom_sf(data = world_crop, fill = "grey90", color = "white") +  # fondo recortado
  geom_sf(data = study_area_clean, fill = "#FFA07A", color = "#FF7F24", alpha = 0.8) +
  labs(title = "Study area", x = "Longitude", y = "Latitude") +
  theme_bw() +
  theme(
    legend.position = "right",
    plot.title = element_text(size = 20, face = "bold"),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )

print(map_zone)
ggsave("04_Figures/Fig_Study_Area.png", plot = map_zone, width = 10, height = 15, units = "in", dpi = 600)

# -----------------------------------------------------------------------------------------------#
# Add climate values to the entire study area
# -----------------------------------------------------------------------------------------------#

study_area <- st_transform(study_area, crs = crs(climatelayers_ss_2_stack))
centroids <- st_centroid(study_area)
coords <- st_coordinates(centroids)
bioclim_vals <- raster::extract(climatelayers_ss_2_stack, coords)

data_vars_study_area <- data.frame(
  ET_ID     = study_area$ET_ID,
  longitude = coords[,1],
  latitude  = coords[,2],
  bioclim_vals
)

# str(data_vars_study_area)
# head(data_vars_study_area)
data_vars_study_area <- distinct(data_vars_study_area)

#write.csv(data_vars_study_area, "01_Data/04_Data_clim/data_clim_study_area.csv")

"
# -----------------------------------------------------------------------------------------------#
# 4. JOIN DATA
# -----------------------------------------------------------------------------------------------#
"
# Lentic --------------------------------------------------------------------------------------------
data_lentic_study_area <- merge(x = lentic_presence_data_long, y = study_area)
data_lentic_study_area_df <- as.data.frame(data_lentic_study_area)

head(data_lentic_study_area_df)

#we extract the climatic variables in the presence coordinates
bioclim_extract <- raster::extract(climatelayers_ss_2_stack,
                           data_lentic_study_area_df[,c("Longitude","Latitude")])

clima_df <- as.data.frame(bioclim_extract)

#we combine it with species data
data_lentic_clim <- data.frame(cbind(data_lentic_study_area_df, clima_df))
data_lentic_clim <- na.omit(data_lentic_clim)

data_lentic_clim <- dplyr::select(data_lentic_clim, -geometry)
#write.csv(data_lentic_clim, "01_Data/04_Data_clim/data_lentic_clim.csv")


# Lotic --------------------------------------------------------------------------------------------
data_lotic_study_area <- merge(x = lotic_presence_data_long, y = study_area)
data_lotic_study_area_df <- as.data.frame(data_lotic_study_area)

#extraemos las variables climáticas en las coordenadas de presencias
bioclim_extract <- raster::extract(climatelayers_ss_2_stack,
                           data_lotic_study_area_df[,c("Longitude","Latitude")])

head(bioclim_extract)
clima_df <- as.data.frame(bioclim_extract)

#lo unimos a los datos de las especies
data_lotic_clim <- data.frame(cbind(data_lotic_study_area_df, clima_df))
data_lotic_clim <- na.omit(data_lotic_clim)

data_lotic_clim <- dplyr::select(data_lotic_clim, -geometry)

#write.csv(data_lotic_clim, "01_Data/04_Data_clim/data_lotic_clim.csv")


"
# -----------------------------------------------------------------------------------------------#
# 5. BUILDING HYPERVOLUME - Lentic
# -----------------------------------------------------------------------------------------------#
"

## =====================
## Lentic hypervolumes
## =====================

#data <- read.csv("01_Data/04_Data_clim/data_lentic_clim.csv")
data <- data_lentic_clim
print(names(data))

ispe=which(colnames(data)=="specie")

v1=which(colnames(data)=="BIO_01")
v2=which(colnames(data)=="BIO_12")
v3=which(colnames(data)=="BIO_04")
v4=which(colnames(data)=="BIO_15")
v5=which(colnames(data)=="BIO_08")

data_species=data[,c(ispe,v1,v2,v3,v4,v5)]
head(data_species)

# Select colum of interest
print(unique(data_species$specie))

data_ready=cbind(data_species[1],lapply(data_species[2:6],as.numeric))
str(data_ready)

local= "Niche-similarity-and-spatial-co-occurrence-in-lentic-and-lotic-European-dragonflies-and-damselflies"
save_path=paste0(local,"/02_Hypervolume")
setwd(local)
ggpairs(data_ready[,c(2:6)])
dev.off()

# Select vars
bioclim_axes <- c("BIO_01","BIO_12","BIO_04","BIO_15", "BIO_08")
num_bioclim = length(bioclim_axes)
print(paste0("The number of considered bioclimatic variable is : ", num_bioclim))

# Check the number of individual per hypervolume (i.e. per specie)
table(data_ready$specie)

# list the hypervolumes to be calculated
cat_list_Species = sort(as.character(unique(data_ready$sp)))
num_cat_Species = length(cat_list_Species)
print(paste0("The number of Species is : ", num_cat_Species))

# Calculate hypervolumes (1 per species) and join all of them
hv_list_Species = new("HypervolumeList") # list that will contain all the hypervolumes
hv_list_Species@HVList = vector(mode="list",length=num_cat_Species)

for (i in 1:num_cat_Species){ 
  # select data
  data_ready_ce_cat_Species = data_ready[data_ready$sp==cat_list_Species[i],bioclim_axes]
  # Calculate hypervolume for each SppName
  hv_list_Species@HVList[[i]] <- hypervolume_svm(data_ready_ce_cat_Species,name=as.character(cat_list_Species[i]))
}

save(hv_list_Species, file = file.path(paste0(save_path,"/hv_list_Lentic_sp.RData")))

hv_list_Species



## =====================
## Lotic hypervolumes
## =====================

#data <- read.csv("01_Data/04_Data_clim/data_lotic_clim.csv")
data <- data_lotic_clim
data <- data %>% filter(specie != "sp_124")
# Species 124, Platycnemissubdilatata, does not count because it is distributed in North Africa and does not fall within the study area.
# It only has two grid squares. It should be removed.

print(names(data))
ispe=which(colnames(data)=="specie")
v1=which(colnames(data)=="BIO_01")
v2=which(colnames(data)=="BIO_12")
v3=which(colnames(data)=="BIO_04")
v4=which(colnames(data)=="BIO_15")
v5=which(colnames(data)=="BIO_08")

data_species=data[,c(ispe,v1,v2,v3,v4,v5)]
# Select colum of interest
print(unique(data_species$specie))
data_ready=cbind(data_species[1],lapply(data_species[2:6],as.numeric))
str(data_ready)

# Select vars
bioclim_axes <- c("BIO_01","BIO_12","BIO_04","BIO_15", "BIO_08")
num_bioclim = length(bioclim_axes)
print(paste0("The number of considered bioclimatic variable is : ", num_bioclim))

# Check the number of individual per hypervolume (i.e. per specie)
table(data_ready$specie)

# list the hypervolumes to be calculated
cat_list_Species = sort(as.character(unique(data_ready$sp)))
num_cat_Species = length(cat_list_Species)
print(paste0("The number of Species is : ", num_cat_Species))

# Calculate hypervolumes (1 per species) and join all of them
hv_list_Species = new("HypervolumeList") # list that will contain all the hypervolumes
hv_list_Species@HVList = vector(mode="list",length=num_cat_Species)

for (i in 1:num_cat_Species){ 
  # select data
  data_ready_ce_cat_Species = data_ready[data_ready$sp==cat_list_Species[i],bioclim_axes]
  # Calculate hypervolume for each SppName
  hv_list_Species@HVList[[i]] <- hypervolume_svm(data_ready_ce_cat_Species,name=as.character(cat_list_Species[i]))
}

save(hv_list_Species, file = file.path(paste0(save_path,"/hv_list_Lotic_sp.RData")))

hv_list_Species

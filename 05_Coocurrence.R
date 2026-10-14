"
###########################################
##           LOAD LIBRARIES              ##
###########################################
R version 4.5.1 (2025-06-13 ucrt)
"

library(dplyr)
library(tidyverse)
library(sf)
library(readxl)

cat("Versions of the libraries used:\n")
cat("dplyr:", as.character(packageVersion("dplyr")), "\n") # dplyr: 1.1.4
cat("tidyverse:", as.character(packageVersion("tidyverse")), "\n") # tidyverse: 2.0.0 
cat("sf:", as.character(packageVersion("sf")), "\n") # sf: 1.0.21
cat("readxl:", as.character(packageVersion("readxl")), "\n") # readxl: 

"
# -----------------------------------------------------------------------------------------------#
## 1. SPATIAL CO-OCURRENCE - SHARED GRIDS - OVERLAP AREA
# -----------------------------------------------------------------------------------------------#

Spatial co-occurrence was quantified as the number of shared 50-km grid cells occupied by each pair of species.

Four species were excluded from the analyses due to extremely small estimated hypervolumes, 
which did not meet the minimum requirements for the generation of random points and minimum density during hypervolume construction. 
Three of these species belong to the lentic group, and one to the lotic group:

#hv25 = sp_14 = Anax junius (Drury, 1773)
#hv60 = sp_67 = Pantala flavescens (Fabricius, 1798)
#hv81 = sp_91 = Coenagrion hylas (Trybom, 1889)
#hv10 = sp_21 = Cordulegaster helladica (Lohmann, 1993)

# There are 84 lentic species (there were 87 but 3 have been removed)
# There are 35 lotic species (there were 36 but one species has been removed)
"

study_area <- st_read("01_Data/01_Study_Area/Study_Area_WGS_84_CLC.gpkg", quiet = TRUE)
if (!"ET_ID" %in% names(study_area) || anyNA(study_area$ET_ID) ||
    anyDuplicated(as.character(study_area$ET_ID))) {
  stop("Study-area ET_ID values must be present and unique.")
}
study_ids <- as.character(study_area$ET_ID)

filter_grid_to_study_area <- function(path, label) {
  grid <- st_read(path, quiet = TRUE)
  grid_ids <- as.character(grid$ET_ID)
  if (anyNA(grid_ids) || anyDuplicated(grid_ids)) {
    stop(label, " grid ET_ID values must be present and unique.")
  }
  missing_ids <- setdiff(study_ids, grid_ids)
  if (length(missing_ids) > 0) {
    stop(label, " grid lacks ", length(missing_ids), " study-area ET_ID values.")
  }
  grid <- grid[match(study_ids, grid_ids), ]
  if (!identical(as.character(grid$ET_ID), study_ids)) {
    stop(label, " grid could not be aligned to the study-area ET_ID order.")
  }
  grid
}

# LOTIC --------------------------------------------------------------------------------------------

lotic_grid <- filter_grid_to_study_area(
  "01_Data/03_Data_species/Grid_Lotic.gpkg", "Lotic"
)
lotic_grid_df <- st_drop_geometry(lotic_grid)

# LENTIC --------------------------------------------------------------------------------------------

lentic_grid <- filter_grid_to_study_area(
  "01_Data/03_Data_species/Grid_Lentic.gpkg", "Lentic"
)
lentic_grid_df <- st_drop_geometry(lentic_grid)

geometry_matches <- st_equals_exact(
  st_geometry(lentic_grid), st_geometry(lotic_grid), par = 0.1
)
if (!all(vapply(seq_along(geometry_matches),
                function(i) i %in% geometry_matches[[i]], logical(1)))) {
  stop("Lentic and lotic grid geometries do not align by ET_ID.")
}

species_group <- read_excel(
  "01_Data/03_Data_species/List_of_species.xlsx", sheet = "species_group"
)
if (!all(c("species", "group") %in% names(species_group)) ||
    anyNA(species_group$species) || anyNA(species_group$group) ||
    anyDuplicated(species_group$species)) {
  stop("The species_group sheet must contain unique, complete species and group fields.")
}

excluded_lentic <- c("sp_14", "sp_67", "sp_91")
excluded_lotic <- c("sp_21", "sp_124")
lentic_species <- as.character(species_group$species[species_group$group == "Lentic"])
lotic_species <- as.character(species_group$species[species_group$group == "Lotic"])
lentic_species <- setdiff(lentic_species, excluded_lentic)
lotic_species <- setdiff(lotic_species, excluded_lotic)

lentic_fields <- setdiff(names(lentic_grid_df), "ET_ID")
lotic_fields <- setdiff(names(lotic_grid_df), "ET_ID")
if (!setequal(lentic_fields, c(lentic_species, excluded_lentic)) ||
    !setequal(lotic_fields, c(lotic_species, excluded_lotic))) {
  stop("Species grid columns do not match the species_group sheet and exclusions.")
}

validate_presence <- function(data, species, label) {
  values <- unlist(data[species], use.names = FALSE)
  if (anyNA(values) || !all(values %in% c(0, 1))) {
    stop(label, " presence fields must contain only 0/1 values without NA.")
  }
}
validate_presence(lentic_grid_df, lentic_species, "Lentic")
validate_presence(lotic_grid_df, lotic_species, "Lotic")

# COMBINE DATA --------------------------------------------------------------------------------------------

all_sp <- data.frame(
  ET_ID = study_ids,
  lentic_grid_df[lentic_species],
  lotic_grid_df[lotic_species],
  check.names = FALSE
)
if (anyDuplicated(names(all_sp)) || nrow(all_sp) != 2317L ||
    !identical(as.character(all_sp$ET_ID), study_ids)) {
  stop("Combined presence data are not one row per study-area ET_ID.")
}

# CALCULATE COOCURRENCE --------------------------------------------------------------------------------------------

# Function to calculate shared grids between lentic–lotic pairs
get_intergroup_shared_grids <- function(data, lentic_sp, lotic_sp) {
  result <- list()
  
  for (sp1 in lentic_sp) {
    for (sp2 in lotic_sp) {
      if (sp1 %in% names(data) && sp2 %in% names(data)) {
        shared_grids <- sum(data[[sp1]] == 1 & data[[sp2]] == 1, na.rm = TRUE)
        result <- append(result, list(data.frame(sp1 = sp1,
                                                 sp2 = sp2,
                                                 cooccurrence_cells = shared_grids)))
      }
    }
  }
  
  do.call(rbind, result)
}

df_intergroup_overlap <- get_intergroup_shared_grids(all_sp, lentic_species, lotic_species)

df_intergroup_overlap <- df_intergroup_overlap %>%
  mutate(PairID = ifelse(sp1 < sp2,
                         paste(sp1, sp2, sep = "_"),
                         paste(sp2, sp1, sep = "_")))
head(df_intergroup_overlap)

# SAVE RESULTS
write.csv(df_intergroup_overlap, "01_Data/04_Data_Analysis/03_LenticVSLotic/intergroup_cooccurrence.csv", row.names = FALSE)

# Function to calculate shared grids within a group (e.g., lentic–lentic or lotic–lotic)
get_intragroup_shared_grids <- function(data, species_vec) {
  result <- list()
  
  # generates all unique combinations of 2 elements of x. Only unique combinations (avoids A-A and duplicates A-B / B-A)
  combs <- combn(species_vec, 2, simplify = FALSE)
  
  for (pair in combs) {
    sp1 <- pair[1]
    sp2 <- pair[2]
    
    if (sp1 %in% names(data) && sp2 %in% names(data)) {
      shared_grids <- sum(data[[sp1]] == 1 & data[[sp2]] == 1, na.rm = TRUE)
      
      result <- append(result, list(
        data.frame(sp1 = sp1, sp2 = sp2, cooccurrence_cells = shared_grids)
      ))
    }
  }
  
  do.call(rbind, result)
}

# LENTIC–LENTIC
df_lentic_lentic <- get_intragroup_shared_grids(all_sp, lentic_species) %>%
  mutate(PairID = paste(sp1, sp2, sep = "_"))

# LOTIC–LOTIC
df_lotic_lotic <- get_intragroup_shared_grids(all_sp, lotic_species) %>%
  mutate(PairID = paste(sp1, sp2, sep = "_"))

# SAVE RESULTS
write.csv(df_lentic_lentic, "01_Data/04_Data_Analysis/01_Lentic/lentic_lentic_cooccurrence.csv", row.names = FALSE)
write.csv(df_lotic_lotic,  "01_Data/04_Data_Analysis/02_Lotic/lotic_lotic_cooccurrence.csv",  row.names = FALSE)

message("Co-occurrence calculated over ", length(study_ids), " study-area cells.")
message("Pair counts: lentic-lentic ", nrow(df_lentic_lentic),
  "; lotic-lotic ", nrow(df_lotic_lotic),
  "; lentic-lotic ", nrow(df_intergroup_overlap), ".")




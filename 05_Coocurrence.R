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

# LOTIC --------------------------------------------------------------------------------------------

lotic_grid <-st_read("01_Data/03_Data_species/Grid_Lotic.gpkg")
lotic_grid_df <- as.data.frame(lotic_grid)
#colnames(lotic_grid_df)
lotic_grid_df <- select(lotic_grid_df, c(-sp_21, -sp_124, -geometry))

# LENTIC --------------------------------------------------------------------------------------------

lentic_grid <-st_read("01_Data/03_Data_species/Grid_Lentic.gpkg")
lentic_grid_df <- as.data.frame(lentic_grid) #transformar en df
#colnames(lentic_grid_df)
lentic_grid_df <- select(lentic_grid_df, c(-sp_14, -sp_67, -sp_91, -geometry))

# COMBINE DATA --------------------------------------------------------------------------------------------

all_sp = cbind(lotic_grid_df, lentic_grid_df[, setdiff(names(lentic_grid_df), "ET_ID")])
#colnames(all_sp)


# FILTER BY GROUP --------------------------------------------------------------------------------------------

species_group <- read_excel("01_Data/03_Data_species/List_of_species.xlsx", sheet = "species_group")
lentic_species <- species_group %>% filter(group == "Lentic") %>% pull(species)
lotic_species  <- species_group %>% filter(group == "Lotic")  %>% pull(species)

# CALCULATE COOCURRENCE --------------------------------------------------------------------------------------------
# Function to calculate shared grids between lentic–lotic pairs
get_intergroup_shared_grids <- function(data, lentic_sp, lotic_sp) {
  result <- list()
  
  for (sp1 in lentic_sp) {
    for (sp2 in lotic_sp) {
      if (sp1 %in% names(data) & sp2 %in% names(data)) {
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

# SAVE RESULTS --------------------------------------------------------------------------------------------
write.csv(df_intergroup_overlap, "intergroup_cooccurrence.csv", row.names = FALSE)
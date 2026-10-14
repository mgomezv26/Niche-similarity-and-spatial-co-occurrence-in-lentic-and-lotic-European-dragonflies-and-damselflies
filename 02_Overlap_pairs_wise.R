"
###########################################
##           LOAD LIBRARIES              ##
###########################################
R version 4.5.1 (2025-06-13 ucrt)
"
library(hypervolume)
library(dplyr)

cat("Versions of the libraries used:\n")
cat("hypervolume:", as.character(packageVersion("hypervolume")), "\n") # hypervolume: 3.1.6
cat("dplyr:", as.character(packageVersion("dplyr")), "\n") # dplyr: 1.1.4


"
# -----------------------------------------------------------------------------------------------#
# LOAD HYPERVOLUMES FOR LENTIC AND LOTIC SPECIES 
# -----------------------------------------------------------------------------------------------#

Four species are eliminated due to their very small hypervolumes, because they do not have enougth 'random points' and 'minimum density'. The first three species are from the lentic group and the last is from the lotic group:
#hv25 = sp_14 = Anax junius (Drury, 1773)
#hv60 = sp_67 = Pantala flavescens (Fabricius, 1798)
#hv81 = sp_91 = Coenagrion hylas (Trybom, 1889)
#hv10 = sp_21 = Cordulegaster helladica (Lohmann, 1993)

# There are 84 lentic species (there were 87 but 3 have been removed)
# There are 35 lotic species (there were 36 but one species has been removed)
"

# Lentic --------------------------------------------------------------------------------------------
load("02_Hypervolume/hv_list_Lentic_sp.RData")
hv_list_Species
class(hv_list_Species)

species_names <- sapply(hv_list_Species@HVList, function(x) x@Name)
species_names

# Convert HypervolumeList to normal list
results_list <- hv_list_Species@HVList
class(results_list)
results_list

length(results_list)

# Remove hv 25, 60, and 81
results_list_lentic = results_list[-c(25, 60, 81)]
length(results_list_lentic)

# Transform to hypervolumes 
hv_list <- hypervolume_join(results_list_lentic)
hv_list

species_names <- sapply(hv_list@HVList, function(x) x@Name)
species_names

results <- data.frame(sp1 = character(),
                      sp2 = character(),
                      jaccard = numeric(),
                      sorensen = numeric(),
                      frac_unique_1 = numeric(),
                      frac_unique_2 = numeric(),
                      distance = numeric(),
                      stringsAsFactors = FALSE)

for (i in 1:(length(hv_list@HVList) - 1)) { 
  for (j in (i + 1):length(hv_list@HVList)) {
    cat("i:", i, "j:", j, "\n")
    
    # The hypervolumes of the species pairs to go in hypervolume_set
    hv1 <- hv_list[[i]]
    hv2 <- hv_list[[j]]
    
    # applay hypervolume_set
    hv_set = hypervolume_set(hv1, hv2, check.memory=FALSE)
    
    # Function overlap
    overlap <- hypervolume_overlap_statistics(hv_set)
    
    # Calculate the euclidian distance with hypervolume_distance
    hv_distance <- hypervolume_distance(hv1,hv2, type = "centroid", num.points.max = 1000, check.memory = TRUE)
    
    # Add results to the dataframe
    results <- rbind(results, data.frame(
      sp1 = species_names[[i]],
      sp2 = species_names[[j ]],
      jaccard = overlap["jaccard"],
      sorensen = overlap["sorensen"],
      frac_unique_1 = overlap["frac_unique_1"],
      frac_unique_2 = overlap["frac_unique_2"],
      distance = hv_distance
    ))
  }
}
results

write.csv(results, "02_Hypervolume/R_Overlap_pairs_wise/results_overlap_Lentic.csv", row.names = FALSE)


# Lotic --------------------------------------------------------------------------------------------

load("02_Hypervolume/hv_list_Lotic_sp.RData")
hv_list_Species

class(hv_list_Species)

species_names <- sapply(hv_list_Species@HVList, function(x) x@Name)
species_names

results_list <- hv_list_Species@HVList
class(results_list)
results_list

#hv10 = sp_21 = Cordulegaster helladica (Lohmann, 1993)
length(results_list)
results_list_lotic = results_list[-10]
length(results_list_lotic)

hv_list <- hypervolume_join(results_list_lotic)

species_names <- sapply(hv_list@HVList, function(x) x@Name)

results <- data.frame(sp1 = character(),
                      sp2 = character(),
                      jaccard = numeric(),
                      sorensen = numeric(),
                      frac_unique_1 = numeric(),
                      frac_unique_2 = numeric(),
                      distance = numeric(),
                      stringsAsFactors = FALSE)

for (i in 1:(length(hv_list@HVList) - 1)) {  # Ajustar el rango del primer bucle
  for (j in (i + 1):length(hv_list@HVList)) {
    cat("i:", i, "j:", j, "\n")

    hv1 <- hv_list[[i]]
    hv2 <- hv_list[[j]]

    name_hv1 <- species_names[[i]]
    name_hv2 <- species_names[[j]]

    print(paste("Par de especies:", name_hv1, "y", name_hv2))

    hv_set = hypervolume_set(hv1, hv2, check.memory=FALSE)

    overlap <- hypervolume_overlap_statistics(hv_set)

    hv_distance <- hypervolume_distance(hv1,hv2, type = "centroid", num.points.max = 1000, check.memory = TRUE)
    
    # Add results to the dataframe
    results <- rbind(results, data.frame(
      sp1 = species_names[[i]],
      sp2 = species_names[[j ]],
      jaccard = overlap["jaccard"],
      sorensen = overlap["sorensen"],
      frac_unique_1 = overlap["frac_unique_1"],
      frac_unique_2 = overlap["frac_unique_2"],
      distance = hv_distance
    ))
  }
}

write.csv(results, "02_Hypervolume/R_Overlap_pairs_wise/results_overlap_Lotic.csv", row.names = FALSE)


# All species --------------------------------------------------------------------------------------------
results_list_lentic
results_list_lotic

list_all <- c(results_list_lentic, results_list_lotic)
hv_list <- hypervolume_join(list_all)

species_names <- sapply(hv_list@HVList, function(x) x@Name)
#species_names <- paste0("hv_sp_", gsub(".*sp_", "", species_names)) 
species_names

results <- data.frame(sp1 = character(),
                      sp2 = character(),
                      jaccard = numeric(),
                      sorensen = numeric(),
                      frac_unique_1 = numeric(),
                      frac_unique_2 = numeric(),
                      distance = numeric(),
                      stringsAsFactors = FALSE)

for (i in 1:(length(hv_list@HVList) - 1)) {
  for (j in (i + 1):length(hv_list@HVList)) {
    cat("i:", i, "j:", j, "\n")
    
    hv1 <- hv_list[[i]]
    hv2 <- hv_list[[j]]

    name_hv1 <- species_names[[i]]
    name_hv2 <- species_names[[j]]

    print(paste("Par de especies:", name_hv1, "y", name_hv2))

    hv_set = hypervolume_set(hv1, hv2, check.memory=FALSE)

    overlap <- hypervolume_overlap_statistics(hv_set)

    hv_distance <- hypervolume_distance(hv1,hv2, type = "centroid", num.points.max = 1000, check.memory = TRUE)

    results <- rbind(results, data.frame(
      sp1 = species_names[[i]],
      sp2 = species_names[[j ]],
      jaccard = overlap["jaccard"],
      sorensen = overlap["sorensen"],
      frac_unique_1 = overlap["frac_unique_1"],
      frac_unique_2 = overlap["frac_unique_2"],
      distance = hv_distance
    ))
  }
}

write.csv(results, "02_Hypervolume/R_Overlap_pairs_wise/results_overlap_all_species.csv", row.names = FALSE)

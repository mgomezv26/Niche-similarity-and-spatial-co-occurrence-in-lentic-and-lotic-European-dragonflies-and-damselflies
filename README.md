# Niche similarity and spatial co-occurrence in lentic and lotic European dragonflies and damselflies

This repository contains the data, scripts, and analytical workflows used in the study:

**Niche similarity and spatial co-occurrence in lentic and lotic European dragonflies and damselflies (Insecta: Odonata)**  
*Manuscript currently under revision and preparation for submission.*

The aim of this study is to investigate how macroclimatic niche similarity relates to large-scale spatial co-occurrence patterns in European Odonata, explicitly contrasting species associated with lentic (standing water) and lotic (running water) habitats.

---

## Overview

Using occurrence data for 119 European odonate species, we:

- Quantify macroclimatic niches as **multidimensional hypervolumes** based on key bioclimatic variables.
- Measure **pairwise niche similarity** using Jaccard indices derived from hypervolume overlap.
- Evaluate the relationship between **niche similarity and spatial co-occurrence** at the continental scale.
- Test how this relationship differs between **lentic and lotic species**, and how it is modulated by ecological descriptors such as elevation, habitat heterogeneity, and niche breadth.

This repository accompanies the analyses presented in the manuscript and is intended to ensure transparency and reproducibility.

---

## Main analyses

The analytical workflow follows these main steps:

1. **Data preparation**  
   Formatting species occurrence data, environmental variables, and ecological descriptors.

2. **Hypervolume construction**  
   Macroclimatic niches are modelled as multidimensional hypervolumes for each species using selected bioclimatic variables.

3. **Pairwise niche similarity**  
   Niche overlap is quantified using Jaccard indices computed between all species pairs.

4. **Spatial co-occurrence**  
   Pairwise spatial co-occurrence is calculated across the European grid.

5. **Statistical analyses**  
   - Differences in Jaccard niche similarity among the three habitat-based species groups (lentic–lentic, lotic–lotic, lentic–lotic) were tested using a **Kruskal–Wallis test**, followed by **Dunn’s post-hoc pairwise comparisons**.
   - **Mantel tests** were used to assess the relationship between niche similarity and spatial co-occurrence.
   - **Mixed-effects models** were used to identify the factors explaining variation in niche similarity.

Rendered HTML files corresponding to the main analyses are available in the `03_Docs_HTML/` directory.

---

## Project status

⚠️ **Important note**

This repository corresponds to a manuscript that is **still under revision**.  
Scripts, results, and outputs may change prior to final publication.

Please do not cite this repository as a final reference until the article is published.

---

## Reproducibility

The corrected spatial co-occurrence and niche-distance analyses use **R 4.5.1**. Required R packages for these steps are `sf`, `readxl`, `vegan`, `ggplot2`, `patchwork`, `knitr`, and `rmarkdown`. The rendered HTML uses Pandoc (tested with 3.10). No Python preprocessing is required for these two steps.

### Corrected co-occurrence and association analyses

The spatial domain is the **2,317 unique `ET_ID` cells** in `01_Data/01_Study_Area/Study_Area_WGS_84_CLC.gpkg`. The presence grids contain additional cells; the corrected script filters both grids to the study-area IDs and aligns them by `ET_ID` without changing the source GPKGs. It validates unique IDs, matching grid geometries, species fields, and binary presence values.

The analysis includes 119 species after exclusions: 84 lentic and 35 lotic. `Platycnemis subdilatata` is excluded because its range does not extend into the study area (only two cells were present); four additional species are excluded because their occurrence data were insufficient for hypervolume construction (`Anax junius`, `Coenagrion hylas`, `Cordulegaster helladica`, and `Pantala flavescens`). The comparison groups contain 3,486 lentic–lentic, 595 lotic–lotic, and 2,940 lentic–lotic pairs. Spatial co-occurrence is the absolute number of shared 50-km cells. Climatic niche distance is `1 - Jaccard` hypervolume overlap.

- `05_Coocurrence.R` writes pairwise tables to `01_Data/04_Data_Analysis/01_Lentic/`, `02_Lotic/`, and `03_LenticVSLotic/`.
- `07_Mantel_Test_niche_coocurrence_odonata.Rmd` reads those tables and the corresponding Jaccard matrices, writes `01_Data/04_Data_Analysis/Mantel_results.csv`, renders `03_Docs_HTML/07_Mantel_Test_niche_coocurrence_odonata.html`, and exports Figure 3 to `04_Figures/Figure_3_niche_cooccurrence.png`.
- Run the co-occurrence script from the repository root, then render the report from the same working directory:

```r
source("05_Coocurrence.R")
rmarkdown::render("07_Mantel_Test_niche_coocurrence_odonata.Rmd")
```

Lentic–lentic and lotic–lotic associations use Pearson Mantel tests with a prespecified one-sided negative alternative, 9,999 permutations, and the plus-one permutation p-value. The fixed seeds are 20261014 and 20261015.

The intergroup analysis is **not a classical Mantel test**. It is a Pearson correlation on the rectangular 84 × 35 Lentic–Lotic matrices, with a bipartite QAP-style permutation test. Each permutation independently reassigns species labels within the lentic and lotic groups and applies each reassignment consistently to all pairs involving that species. This preserves dependence caused by species shared among pairs. The test assumes species labels are exchangeable within each group. It uses 9,999 permutations and seed 20261016.

The number of shared cells is an absolute co-occurrence measure and may increase with species' geographic range size. These associations are not interpreted as causal evidence of climatic filtering or direct ecological interaction.

This workflow reproduces the corrected co-occurrence and association results from the supplied grids and Jaccard matrices. Reproducing hypervolume construction additionally requires the documented WorldClim source and the package versions recorded in `01_Calculate_Hypervolume.R`.

---

## Citation

A formal citation will be added once the manuscript is accepted and published.  
In the meantime, please contact the authors if you wish to use or adapt parts of this workflow.

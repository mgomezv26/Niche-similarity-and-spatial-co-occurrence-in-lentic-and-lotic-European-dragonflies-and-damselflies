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

- Analyses are primarily conducted in **R**, with additional preprocessing steps implemented in **Python**.
- Scripts are organized to reflect the logical order of the analytical workflow.
- All key results presented in the manuscript can be reproduced using the materials provided here.

---

## Citation

A formal citation will be added once the manuscript is accepted and published.  
In the meantime, please contact the authors if you wish to use or adapt parts of this workflow.

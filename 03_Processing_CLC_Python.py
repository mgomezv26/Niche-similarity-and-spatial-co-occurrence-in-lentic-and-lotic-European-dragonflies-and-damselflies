
import rasterio
import geopandas as gpd
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import contextily as ctx

# -----------------------------------------------------------------------------------------------#
# STUDY AREA
# -----------------------------------------------------------------------------------------------#

study_area = gpd.read_file(r'01_Data/01_Study_Area/Study_Area_WGS_84_CLC.gpkg')
study_area
study_area.crs

fig, ax = plt.subplots(figsize=(6, 6))
study_area.plot(ax=ax, edgecolor='blue', linewidth=0.05)
ctx.add_basemap(ax, crs=study_area.crs.to_string(), source=ctx.providers.CartoDB.PositronNoLabels)
ax.set_axis_off()
plt.title("Study Area", fontsize=10)
plt.show()


# -----------------------------------------------------------------------------------------------#
# DATA CLC
# -----------------------------------------------------------------------------------------------#

# This file corresponds to the CLC download. It has not been uploaded to GitHub.
data_clc = gpd.read_file(r'u2018_clc2018_v2020_20u1_geoPackage/DATA/U2018_CLC2018_V2020_20u1.gpkg')
data_clc.head()
print(data_clc.dtypes)

# -----------------------------------------------------------------------------------------------#
# Forest and semi natural areas
# -----------------------------------------------------------------------------------------------#

## Forest ---------------------------------------------------------------------------------------
### 311 - Broad-leaved forest
h_311 = data_clc[(data_clc['Code_18'] == '311')]
### 312 - Coniferous forest 
h_312 = data_clc[(data_clc['Code_18'] == '312')]
### 313 - Mixed forest
h_313 = data_clc[(data_clc['Code_18'] == '313')]

### plots
titles = ["311 - Broad-leaved forest", "312 - Coniferous forest", "313 - Mixed forest"]

def plot_habitat(df, title):
    fig, ax = plt.subplots(figsize=(6, 6))
    df.plot(ax=ax, edgecolor='blue', linewidth=0.05)
    ctx.add_basemap(ax, crs=df.crs.to_string(), source=ctx.providers.CartoDB.PositronNoLabels)
    ax.set_axis_off()
    plt.title(title, fontsize=10)
    plt.show()

plot_habitat(h_311, titles[0])
plot_habitat(h_312, titles[1])
plot_habitat(h_313, titles[2])

## Scrub and/or herbaceous vegetation associations ----------------------------------------------
### 321 Natural grasslands
h_321 = data_clc[(data_clc['Code_18'] == '321')]
### 322 Moors and heathland (Páramos y brezales)
h_322 = data_clc[(data_clc['Code_18'] == '322')]
### 323 Sclerophyllous vegetation
h_323 = data_clc[(data_clc['Code_18'] == '323')]
### 324 Transitional woodland-shrub
h_324 = data_clc[(data_clc['Code_18'] == '324')]

## Open spaces with little or no vegetation -----------------------------------------------------
### 331 - Beaches, dunes, sands
h_331 = data_clc[(data_clc['Code_18'] == '331')]
### 332 - Bare rocks 
h_332 = data_clc[(data_clc['Code_18'] == '332')]
### 333 - Sparsely vegetated areas
h_333 = data_clc[(data_clc['Code_18'] == '333')]
### 334 - Burnt areas
h_334 = data_clc[(data_clc['Code_18'] == '334')]
### 335 - Glaciers and perpetual snow
h_335 = data_clc[(data_clc['Code_18'] == '335')]

# -----------------------------------------------------------------------------------------------#
# wetlands
# -----------------------------------------------------------------------------------------------#

## Inland wetlands -------------------------------------------------------------------------------
### 411 - Inland marshes
h_411 = data_clc[(data_clc['Code_18'] == '411')]
### 412 - Peat bogs
h_412 = data_clc[(data_clc['Code_18'] == '412')]

## Maritime wetlands -------------------------------------------------------------------------------
### 421 - Salt marshes
h_421 = data_clc[(data_clc['Code_18'] == '421')]
### 422 - Maritime wetlands
h_422 = data_clc[(data_clc['Code_18'] == '422')]
### 423 - Intertidal flats
h_423 = data_clc[(data_clc['Code_18'] == '423')]

# -----------------------------------------------------------------------------------------------#
# Water bodies
# -----------------------------------------------------------------------------------------------#

## Inland waters  -------------------------------------------------------------------------------
### 421 - Water courses
h_511 = data_clc[(data_clc['Code_18'] == '511')]
### 423 - Water bodies
h_512 = data_clc[(data_clc['Code_18'] == '512')]

## Marine waters  -------------------------------------------------------------------------------
### 521 - Coastal lagoons
h_521 = data_clc[(data_clc['Code_18'] == '521')]
### 522 - Estuaries
h_522 = data_clc[(data_clc['Code_18'] == '522')]
### 523 - Marine waters
h_523 = data_clc[(data_clc['Code_18'] == '523')]

# -----------------------------------------------------------------------------------------------#
# Agricultural areas
# -----------------------------------------------------------------------------------------------#
### Arable land
h_211_213 = data_clc[data_clc['Code_18'].isin(['211', '212', '213'])]
### Permanent crops
h_221_223 = data_clc[data_clc['Code_18'].isin(['221', '222', '223'])]
### Pastures
h_231 = data_clc[(data_clc['Code_18'] == '231')]
### Heterogeneous agricultural areas
h_241_244 = data_clc[data_clc['Code_18'].isin(['241', '242', '243', '244'])]

# -----------------------------------------------------------------------------------------------#
# Artificial surfaces
# -----------------------------------------------------------------------------------------------#

h_111_142 = data_clc[data_clc['Code_18'].isin(['111', '112', '121', '122', '123', '124', '131', '132', '133', '141', '142'])]

# -----------------------------------------------------------------------------------------------#
# Calculation of the area of each habitat per grid square
# -----------------------------------------------------------------------------------------------#

study_area = study_area.to_crs(h_511.crs)
print(study_area.crs)

# Intersection of the grid with habitat polygons
h_511_intersec = gpd.overlay(study_area, h_511, how='intersection')
h_511_intersec

# Calculation of the area for each of the polygons created with the intersection in km²
h_511_intersec['Area_H_511'] = h_511_intersec.geometry.area / 10**6  # de m² a km²

# Add up the areas of the polygons within each grid cell
area_per_grid = h_511_intersec.groupby('ET_ID')['Area_H_511'].sum().reset_index()

Grid_Habitats = study_area.merge(area_per_grid, on='ET_ID', how='left')
Grid_Habitats

# List of hábitats
habitat_list = [
    'h_111_142', 
    'h_211_213', 'h_221_223', 'h_231', 'h_241_244',
    'h_311', 'h_312', 'h_313', 'h_321', 'h_322', 'h_323', 'h_324', 'h_331', 'h_332', 'h_333', 'h_334', 'h_335', 
    'h_411', 'h_412','h_421', 'h_422', 'h_423', 
    'h_511', 'h_512', 'h_521', 'h_522', 'h_523'
]

# Automated process---------------------------------------------------------------------------------

## Create an output DataFrame based on the study area.
final_grid = study_area.copy()

for habitat in habitat_list:
    # Intersection between the study area and the habitat.
    habitat_intersec = gpd.overlay(study_area, eval(habitat), how='intersection')
    
    # Calculate the area in km² for intersection polygons.
    habitat_intersec[f'Area_{habitat}'] = habitat_intersec.geometry.area / 10**6  # de m² a km²
    
    # Sum of the areas for each grid cell
    area_per_grid = habitat_intersec.groupby('ET_ID')[f'Area_{habitat}'].sum().reset_index()
    
    # Combine with the final grid
    final_grid = final_grid.merge(area_per_grid, on='ET_ID', how='left')
    
    # Fill with zeroes for cells without intersection
    final_grid[f'Area_{habitat}'] = final_grid[f'Area_{habitat}'].fillna(0)

#final_grid.to_csv('01_Data/02_Variables/area_habitats.csv', index=False, decimal='.')
final_grid.head()

# -----------------------------------------------------------------------------------------------#
# PERCENTAGE OF EACH HABITAT AREA PER GRID CELL
# -----------------------------------------------------------------------------------------------#

# Calculate total area per grid cell in km²
final_grid['Total_Area'] = final_grid.geometry.area / 10**6  # Total in km² per grid

# Calculate the percentage of each habitat area per grid cell
for habitat in habitat_list:
    final_grid[f'Perc_{habitat}'] = (
        final_grid[f'Area_{habitat}'] / final_grid['Total_Area'] * 100
    ).fillna(0)

final_grid.head(2)

df_percentages = final_grid[['ET_ID']].copy() 

# Calculate percentages for each habitat
for habitat in habitat_list:
    df_percentages[f'Perc_{habitat}'] = (
        final_grid[f'Area_{habitat}'] / final_grid['Total_Area'] * 100
    ).fillna(0)

# Export results to CSV
#df_percentages.to_csv('01_Data/02_Variables/percentages_habitats.csv', index=False)

df_percentages = final_grid[['ET_ID']].copy() 
print(final_grid.dtypes)


# -----------------------------------------------------------------------------------------------#
# HABITAT RICHNESS
# -----------------------------------------------------------------------------------------------#

# Calculate habitat richness (number of habitats with area > 0 per grid square)
df_richness = final_grid[['ET_ID']].copy()

# Select only the columns representing areas (excluding “ET_ID” and “geometry”)
area_columns = [col for col in final_grid.columns if col.startswith('Area_')]

# Count how many habitats have an area > 0 per grid square
df_richness['Habitat_Richness'] = final_grid[area_columns].gt(0).sum(axis=1)

# Export results to CSV
#df_richness.to_csv('01_Data/02_Variables/richness_habitats.csv', index=False)
#df_richness.head()

# -----------------------------------------------------------------------------------------------#
# # SHANNON'S DIVERSITY
# -----------------------------------------------------------------------------------------------#

#The Shannon diversity index (H') is a measure widely used in ecology to assess the heterogeneity of a system, 
# in this case, the heterogeneity of habitats in each grid. Its interpretation includes the following aspects:

#- H′≥0: The minimum value is 0, which occurs if there is only one type of habitat in the grid (i.e., there is no diversity).
#- No fixed upper limit, but a higher H' indicates greater diversity.

# Select only the percentage columns (ignoring “ET_ID”)
habitat_cols = [col for col in df_percentages.columns if col.startswith("Perc_h_")]

# Convert percentage values to proportions (divide by 100)
df_percentages[habitat_cols] = df_percentages[habitat_cols].div(100)

# Function to calculate the Shannon index
def shannon_diversity(row):
    proportions = row[row > 0]  # Ignore null values or zeros
    return -np.sum(proportions * np.log(proportions))

# Create a new DataFrame with 'ET_ID'
df_shannon = df_percentages[['ET_ID']].copy()

# Apply the function to each row of the original DataFrame to calculate Shannon
df_shannon["H_shannon"] = df_percentages[habitat_cols].apply(shannon_diversity, axis=1)
df_shannon.head()
# Export results to CSV
#df_shannon.to_csv('01_Data/02_Variables/Shannon_index_habitats.csv', index=False)
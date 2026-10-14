# Mapa de riqueza de las 119 especies incluidas en el estudio.
# Ejecutar con el directorio de trabajo situado en la raiz del repositorio.

required_packages <- c("sf", "readxl", "ggplot2")

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
}

library(sf)
library(readxl)
library(ggplot2)

study_area_path <- file.path(
  "01_Data", "01_Study_Area", "Study_Area_WGS_84_CLC.gpkg"
)
lentic_grid_path <- file.path(
  "01_Data", "03_Data_species", "Grid_Lentic.gpkg"
)
lotic_grid_path <- file.path(
  "01_Data", "03_Data_species", "Grid_Lotic.gpkg"
)
species_catalog_path <- file.path(
  "01_Data", "03_Data_species", "List_of_species.xlsx"
)

output_dir <- "04_Figures"
png_path <- file.path(output_dir, "Figure_1_species_richness_119.png")
pdf_path <- file.path(output_dir, "Figure_1_species_richness_119.pdf")

input_paths <- c(
  study_area_path,
  lentic_grid_path,
  lotic_grid_path,
  species_catalog_path
)

if (!all(file.exists(input_paths))) {
  stop(
    "No se encuentran todos los archivos de entrada. ",
    "Ejecuta el script desde la raiz del repositorio."
  )
}

# Leer capas y catalogos usando los nombres de hoja/campos comprobados.
study_area <- sf::st_read(study_area_path, quiet = TRUE)
lentic_grid <- sf::st_read(lentic_grid_path, quiet = TRUE)
lotic_grid <- sf::st_read(lotic_grid_path, quiet = TRUE)

species_catalog <- as.data.frame(
  readxl::read_excel(species_catalog_path, sheet = "Species")
)
species_groups <- as.data.frame(
  readxl::read_excel(species_catalog_path, sheet = "species_group")
)

catalog_fields <- c(
  "Genus", "Species", "Habitat", "ID_Grid", "Used for this study"
)
group_fields <- c("species", "group")

if (!all(catalog_fields %in% names(species_catalog))) {
  stop("La hoja Species no contiene las columnas esperadas del catalogo.")
}
if (!all(group_fields %in% names(species_groups))) {
  stop("La hoja species_group no contiene species y group.")
}
if (!"ET_ID" %in% names(study_area) ||
    !"ET_ID" %in% names(lentic_grid) ||
    !"ET_ID" %in% names(lotic_grid)) {
  stop("Falta la columna ET_ID en una o mas capas.")
}

# Normalizar campos de texto para comparar IDs y nombres cientificos.
species_catalog$ID_Grid <- trimws(as.character(species_catalog$ID_Grid))
species_catalog$Habitat <- trimws(as.character(species_catalog$Habitat))
species_catalog$used_flag <- toupper(
  trimws(as.character(species_catalog[["Used for this study"]]))
)
species_catalog$scientific_name <- paste(
  trimws(as.character(species_catalog$Genus)),
  trimws(as.character(species_catalog$Species))
)

species_groups$species <- trimws(as.character(species_groups$species))
species_groups$group <- trimws(as.character(species_groups$group))

used_catalog <- species_catalog[
  !is.na(species_catalog$used_flag) &
    species_catalog$used_flag == "YES",
  ,
  drop = FALSE
]

if (nrow(used_catalog) != 119L ||
    anyNA(used_catalog$ID_Grid) ||
    anyDuplicated(used_catalog$ID_Grid)) {
  stop("El catalogo no contiene exactamente 119 IDs YES unicos.")
}

if (anyNA(species_groups$species) ||
    anyNA(species_groups$group) ||
    anyDuplicated(species_groups$species)) {
  stop("species_group debe tener IDs unicos y sin valores faltantes.")
}

if (!setequal(species_groups$species, used_catalog$ID_Grid)) {
  stop("species_group no coincide con los 119 IDs marcados YES.")
}

lentic_species <- species_groups$species[
  species_groups$group == "Lentic"
]
lotic_species <- species_groups$species[
  species_groups$group == "Lotic"
]

if (length(lentic_species) != 84L || length(lotic_species) != 35L) {
  stop("Se esperaban 84 especies lentic y 35 lotic.")
}

# Comprobar que los nombres descartados no forman parte de los 119 taxones.
excluded_species <- c(
  "Platycnemis subdilatata",
  "Anax junius",
  "Coenagrion hylas",
  "Cordulegaster helladica",
  "Pantala flavescens"
)

included_names <- used_catalog$scientific_name
if (any(excluded_species %in% included_names)) {
  stop("Una especie descartada aparece entre los IDs marcados YES.")
}

# Las cuatro exclusiones por hipervolumen deben constar como NO en el catalogo.
excluded_hypervolume <- c(
  "Anax junius",
  "Coenagrion hylas",
  "Cordulegaster helladica",
  "Pantala flavescens"
)

for (species_name in excluded_hypervolume) {
  rows <- which(species_catalog$scientific_name == species_name)
  
  if (length(rows) != 1L ||
      species_catalog$used_flag[rows] != "NO") {
    stop(
      "No se pudo confirmar la exclusion NO de ",
      species_name,
      " en la hoja Species."
    )
  }
}

study_ids <- as.character(study_area$ET_ID)

if (length(study_ids) != 2317L ||
    anyNA(study_ids) ||
    anyDuplicated(study_ids)) {
  stop("El area debe contener exactamente 2.317 ET_ID unicos.")
}

# Alinear cada grid con el orden de ET_ID del area de estudio.
align_grid_to_study_area <- function(grid, label) {
  grid_ids <- as.character(grid$ET_ID)
  
  if (anyNA(grid_ids) || anyDuplicated(grid_ids)) {
    stop(label, " contiene ET_ID vacios o duplicados.")
  }
  
  missing_ids <- setdiff(study_ids, grid_ids)
  if (length(missing_ids) > 0) {
    stop(label, " no contiene todos los ET_ID del area.")
  }
  
  aligned_grid <- grid[match(study_ids, grid_ids), ]
  
  if (!identical(as.character(aligned_grid$ET_ID), study_ids)) {
    stop("No se pudo alinear ", label, " con el area de estudio.")
  }
  
  aligned_grid
}

lentic_grid <- align_grid_to_study_area(lentic_grid, "Grid_Lentic")
lotic_grid <- align_grid_to_study_area(lotic_grid, "Grid_Lotic")

if (nrow(lentic_grid) != 2317L || nrow(lotic_grid) != 2317L) {
  stop("Los grids filtrados deben tener exactamente 2.317 filas.")
}

# Los dos grids deben describir las mismas celdas en el mismo orden.
geometry_matches <- sf::st_equals_exact(
  sf::st_geometry(lentic_grid),
  sf::st_geometry(lotic_grid),
  par = 0.1
)

if (!all(vapply(
  seq_along(geometry_matches),
  function(i) i %in% geometry_matches[[i]],
  logical(1)
))) {
  stop("Las geometrias lentic y lotic no coinciden por ET_ID.")
}

missing_lentic_columns <- setdiff(lentic_species, names(lentic_grid))
missing_lotic_columns <- setdiff(lotic_species, names(lotic_grid))

if (length(missing_lentic_columns) > 0 ||
    length(missing_lotic_columns) > 0) {
  stop("Faltan columnas de presencia para especies seleccionadas.")
}

# Convertir las columnas individualmente: sf puede leer campos 0/1 como
# character, aunque algunas columnas del mismo GPKG sean numericas.
convert_presence_columns <- function(data, species, label) {
  presence_data <- data[, species, drop = FALSE]
  converted_columns <- vector("list", length(species))
  names(converted_columns) <- species

  for (species_id in species) {
    values <- presence_data[[species_id]]

    if (anyNA(values)) {
      stop(label, " contiene NA en la columna ", species_id, ".")
    }

    if (is.logical(values)) {
      converted <- as.integer(values)
    } else if (is.numeric(values) || is.integer(values)) {
      if (!all(values %in% c(0, 1))) {
        stop(label, " contiene valores distintos de 0/1 en ", species_id, ".")
      }
      converted <- as.numeric(values)
    } else if (is.character(values) || is.factor(values)) {
      text_values <- trimws(as.character(values))
      if (!all(text_values %in% c("0", "1"))) {
        stop(label, " contiene valores no binarios en ", species_id, ".")
      }
      converted <- as.numeric(text_values)
    } else {
      stop(
        label, " tiene un tipo de dato no admitido en ", species_id,
        ": ", paste(class(values), collapse = "/"), "."
      )
    }

    if (anyNA(converted) || !all(converted %in% c(0, 1))) {
      stop("La conversion altero o invalido la presencia en ", species_id, ".")
    }

    converted_columns[[species_id]] <- converted
  }

  as.matrix(as.data.frame(converted_columns, check.names = FALSE))
}

lentic_data <- sf::st_drop_geometry(lentic_grid)
lotic_data <- sf::st_drop_geometry(lotic_grid)
lentic_presence <- convert_presence_columns(lentic_data, lentic_species, "Lentic")
lotic_presence <- convert_presence_columns(lotic_data, lotic_species, "Lotic")

presence_matrix <- cbind(lentic_presence, lotic_presence)

if (ncol(presence_matrix) != 119L ||
    anyDuplicated(colnames(presence_matrix))) {
  stop("La matriz final no contiene exactamente 119 especies unicas.")
}

# Cada fila es una celda; cada columna es una especie.
species_richness <- rowSums(presence_matrix)

if (length(species_richness) != 2317L ||
    anyNA(species_richness)) {
  stop("El vector de riqueza no coincide con las 2.317 celdas.")
}

study_area$species_richness <- as.numeric(species_richness)

richness_min <- min(species_richness)
richness_max <- max(species_richness)
richness_mean <- mean(species_richness)
zero_richness_cells <- sum(species_richness == 0)

cat("Resumen de riqueza de especies\n")
cat("Celdas:", length(species_richness), "\n")
cat("Especies: 119 (84 lentic, 35 lotic)\n")
cat("Minima:", richness_min, "\n")
cat("Maxima:", richness_max, "\n")
cat("Media:", round(richness_mean, 2), "\n")
cat("Celdas con riqueza cero:", zero_richness_cells, "\n")

# Mapa de riqueza total con una escala secuencial continua.
richness_breaks <- pretty(
  c(0, richness_max),
  n = 6
)

richness_map <- ggplot2::ggplot(study_area) +
  ggplot2::geom_sf(
    ggplot2::aes(fill = .data$species_richness),
    color = "white",
    linewidth = 0.06
  ) +
  ggplot2::scale_fill_viridis_c(
    option = "D",
    direction = 1,
    limits = c(0, richness_max),
    breaks = richness_breaks,
    name = "Species richness\n(per 50-km cell)"
  ) +
  ggplot2::coord_sf(expand = FALSE) +
  ggplot2::labs(
    title = "Species richness of analyzed European Odonata",
    subtitle = "119 species (84 lentic, 35 lotic); 2,317 50-km cells",
    x = "Longitude",
    y = "Latitude"
  ) +
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(
      fill = "#F5F7FA",
      color = NA
    ),
    panel.grid.major = ggplot2::element_line(
      color = "#D9E0E7",
      linewidth = 0.25
    ),
    panel.grid.minor = ggplot2::element_blank(),
    axis.title = ggplot2::element_text(face = "bold"),
    plot.title = ggplot2::element_text(face = "bold"),
    plot.background = ggplot2::element_rect(
      fill = "white",
      color = NA
    )
  )

if (interactive()) {
  print(richness_map)
}

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

ggplot2::ggsave(
  filename = png_path,
  plot = richness_map,
  width = 180,
  height = 145,
  units = "mm",
  dpi = 600,
  bg = "white"
)

ggplot2::ggsave(
  filename = pdf_path,
  plot = richness_map,
  width = 180,
  height = 145,
  units = "mm",
  device = "pdf",
  bg = "white"
)

cat("PNG guardado en:", png_path, "\n")
cat("PDF guardado en:", pdf_path, "\n")

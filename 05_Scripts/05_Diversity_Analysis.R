#-------------------------------------------------------------------------------
# ANÁLISIS DE BIODIVERSIDAD 
#
# Este script calcula la diversidad vegetal de los sitios BIO3 a partir de
# los registros de vegetación obtenidos en terreno.
#
# Criterios principales:
#
# - Se consideran tallos con DAP >= 3 cm.
# - La diversidad observada utiliza individuos únicos.
# - iNEXT utiliza abundancia de tallos.
# - Las tres parcelas efectivas se agrupan como una comunidad por sitio.
# - La diversidad total, nativa y exótica se analiza por separado.
#-------------------------------------------------------------------------------

library(tidyverse)
library(vegan)
library(iNEXT)

# Paths
field_dir <- "02_field"
out_dir   <- file.path("04_integrated_data", "diversity")

veg_file <- file.path(
  field_dir,
  "StructurePlots_Clean.csv"
)

sites_file <- file.path(
  field_dir,
  "0_sitio.xlsx - Sheet1.csv"
)

species_ref_file <- file.path(
  out_dir,
  "00_species_reference.csv"
)

required_files <- c(
  veg_file,
  sites_file
)

if (!all(file.exists(required_files))) {
  stop(
    "One or more required input files are missing:\n",
    paste(required_files[!file.exists(required_files)], collapse = "\n")
  )
}

# Parámetros del análisis
#-------------------------------------------------------------------------------

dap_min_cm <- 3

coverage_level <- 0.95

if (!dir.exists(out_dir)) {
  dir.create(
    out_dir,
    recursive = TRUE
  )
}

# 1. PREPARACIÓN DE LOS DATOS
#-------------------------------------------------------------------------------

# 1.1 Leer vegetación
veg_raw <- read_csv(
  veg_file,
  locale = locale(
    encoding = "Latin1"
  ),
  show_col_types = FALSE
)

# Estandarizar nombres y formatos 
veg_raw <- veg_raw %>%
  rename(
    site = id_site,
    individual = ind,
    individual_id = ind_uid
  ) %>%
  mutate(
    site = str_to_upper(
      str_squish(site)
    ),
    
    plot = as.numeric(plot),
    
    scientific_name = str_squish(
      scientific_name
    ),
    
    individual_id = str_squish(
      individual_id
    ),
    
    dap = suppressWarnings(
      as.numeric(dap)
    ),
    
    uom = str_to_lower(
      str_squish(uom)
    )
  )

# Comprobar columnas necesarias
required_cols <- c(
  "site",
  "plot",
  "scientific_name",
  "individual",
  "individual_id",
  "dap",
  "uom"
)

missing_cols <- setdiff(
  required_cols,
  names(veg_raw)
)

if (length(missing_cols) > 0) {
  
  stop(
    paste(
      "Faltan columnas necesarias en la base de vegetación:",
      paste(
        missing_cols,
        collapse = ", "
      )
    )
  )
}

# 1.2 Aplicar criterio de DAP
# StructurePlots_Clean.csv contiene los DAP corregidos, pero todavía incluye
# registros bajo el umbral utilizado para el análisis.
#
# Se conservan solamente tallos con DAP >= 3 cm

veg_filtered <- veg_raw %>%
  filter(
    !is.na(site),
    !is.na(plot),
    !is.na(scientific_name),
    scientific_name != "",
    !is.na(dap),
    dap >= dap_min_cm,
    uom == "cm"
  )

cat(
  "\nRegistros de vegetación después del filtro DAP:",
  nrow(veg_filtered),
  "\n"
)

# 2. ORIGEN DE LAS ESPECIES
#-------------------------------------------------------------------------------

# Crear referencia inicial si todavía no existe
species_list <- veg_filtered %>%
  distinct(
    scientific_name
  ) %>%
  arrange(
    scientific_name
  ) %>%
  mutate(
    Origin = case_when(
      
      scientific_name %in% c(
        "Acacia dealbata",
        "Eucalyptus globulus",
        "Pinus radiata",
        "Rosa rubiginosa"
      ) ~ "Exotic",
      
      TRUE ~ "Native"
    )
  )


if (!file.exists(species_ref_file)) {
  
  write_csv(
    species_list,
    species_ref_file
  )
  
  cat(
    "\nTabla de referencia de especies creada:\n",
    species_ref_file,
    "\n"
  )
}

# Leer referencia de origen
species_ref <- read_csv(
  species_ref_file,
  locale = locale(
    encoding = "Latin1"
  ),
  show_col_types = FALSE
) %>%
  mutate(
    scientific_name = str_squish(
      scientific_name
    )
  )

# Incorporar origen a la base de vegetación
veg <- veg_filtered %>%
  left_join(
    species_ref,
    by = "scientific_name"
  )

# Comprobar especies sin clasificación
if (any(is.na(veg$Origin))) {
  
  cat(
    "\nEspecies sin clasificación Native / Exotic:\n"
  )
  
  veg %>%
    filter(
      is.na(Origin)
    ) %>%
    distinct(
      scientific_name
    ) %>%
    print(
      n = Inf
    )
  
  stop(
    "Completar 00_species_reference.csv antes de continuar."
  )
}

# 3. ESFUERZO DE MUESTREO Y UNIDADES DE ABUNDANCIA
#-------------------------------------------------------------------------------

# 3.1 Parcelas efectivamente visitadas
sites_ref <- read_csv(
  sites_file,
  locale = locale(
    encoding = "Latin1"
  ),
  show_col_types = FALSE
) %>%
  rename_with(
    ~ str_trim(.x)
  ) %>%
  rename(
    site = ID_site
  ) %>%
  mutate(
    site = str_to_upper(
      str_squish(site)
    ),
    
    plot = as.numeric(plot),
    
    visited = as.numeric(visited)
  )

plots_effort <- sites_ref %>%
  filter(
    visited == 1,
    !is.na(plot)
  ) %>%
  distinct(
    site,
    plot
  )

plots_per_site <- plots_effort %>%
  count(
    site,
    name = "n_effective_plots"
  ) %>%
  arrange(
    site
  )

valid_sites <- plots_per_site %>%
  filter(
    n_effective_plots == 3
  ) %>%
  pull(
    site
  )

excluded_sites <- plots_per_site %>%
  filter(
    n_effective_plots != 3
  )

cat("\n--- Parcelas efectivas por sitio ---\n")

print(
  plots_per_site,
  n = Inf
)

if (nrow(excluded_sites) > 0) {
  
  cat(
    "\nSitios excluidos por no tener exactamente 3 parcelas efectivas:\n"
  )
  
  print(
    excluded_sites,
    n = Inf
  )
}

# Mantener solamente registros de parcelas efectivamente visitadas
veg_valid <- veg %>%
  filter(
    site %in% valid_sites
  ) %>%
  semi_join(
    plots_effort,
    by = c(
      "site",
      "plot"
    )
  )

# 3.2 Dos unidades de abundancia
#-------------------------------------------------------------------------------
#
# Se usan dos representaciones distintas según el análisis:
#
# - Individuos únicos:
#   diversidad observada y composición Native / Exotic.
#
# - Tallos:
#   abundancia utilizada en iNEXT.

# Individuos únicos
individuals <- veg_valid %>%
  filter(
    !is.na(individual_id),
    individual_id != ""
  ) %>%
  distinct(
    site,
    plot,
    scientific_name,
    individual_id,
    Origin
  )

# Tallos
stems <- veg_valid

cat(
  "\nIndividuos únicos:",
  nrow(individuals),
  "\n"
)

cat(
  "Tallos retenidos:",
  nrow(stems),
  "\n"
)

# 4. DIVERSIDAD OBSERVADA Y COMPOSICIÓN NATIVE / EXOTIC
#-------------------------------------------------------------------------------

# 4.1 Función para diversidad observada
# La abundancia corresponde a individuos únicos.

calc_diversity <- function(
    df,
    grouping_vars,
    assemblage_name
) {
  
  if (nrow(df) == 0) {
    return(tibble())
  }
  
  df %>%
    count(
      across(
        all_of(grouping_vars)
      ),
      scientific_name,
      name = "abundance"
    ) %>%
    
    group_by(
      across(
        all_of(grouping_vars)
      )
    ) %>%
    
    summarise(
      assemblage = assemblage_name,
      
      n_individuals = sum(abundance),
      
      richness_obs =
        n_distinct(scientific_name),
      
      shannon =
        vegan::diversity(
          abundance,
          index = "shannon"
        ),
      
      simpson_1_D =
        vegan::diversity(
          abundance,
          index = "simpson"
        ),
      
      pielou_evenness =
        if_else(
          richness_obs > 1,
          shannon / log(richness_obs),
          NA_real_
        ),
      
      hill_q0_obs =
        richness_obs,
      
      hill_q1_obs =
        exp(shannon),
      
      hill_q2_obs =
        vegan::diversity(
          abundance,
          index = "invsimpson"
        ),
      
      .groups = "drop"
    )
}

# 4.2 Diversidad observada por sitio
# Las tres parcelas efectivas se agrupan como una comunidad por sitio.

diversity_by_site <- bind_rows(
  
  calc_diversity(
    individuals,
    "site",
    "Total"
  ),
  
  calc_diversity(
    individuals %>%
      filter(
        Origin == "Native"
      ),
    "site",
    "Native"
  ),
  
  calc_diversity(
    individuals %>%
      filter(
        Origin == "Exotic"
      ),
    "site",
    "Exotic"
  )
) %>%
  arrange(
    site,
    assemblage
  )

cat("\n--- Diversidad observada por sitio ---\n")

print(
  diversity_by_site,
  n = Inf,
  width = Inf
)

# 4.3 Composición Native / Exotic
native_exotic_summary <- individuals %>%
  group_by(
    site
  ) %>%
  summarise(
    n_individuals_total = n(),
    
    n_individuals_native =
      sum(
        Origin == "Native"
      ),
    
    n_individuals_exotic =
      sum(
        Origin == "Exotic"
      ),
    
    prop_individuals_native =
      n_individuals_native /
      n_individuals_total,
    
    prop_individuals_exotic =
      n_individuals_exotic /
      n_individuals_total,
    
    richness_total =
      n_distinct(
        scientific_name
      ),
    
    richness_native =
      n_distinct(
        scientific_name[
          Origin == "Native"
        ]
      ),
    
    richness_exotic =
      n_distinct(
        scientific_name[
          Origin == "Exotic"
        ]
      ),
    
    prop_richness_native =
      richness_native /
      richness_total,
    
    prop_richness_exotic =
      richness_exotic /
      richness_total,
    
    .groups = "drop"
  )

# Agregar diversidad observada nativa y exótica
observed_native <- diversity_by_site %>%
  filter(
    assemblage == "Native"
  ) %>%
  select(
    site,
    shannon_native = shannon,
    simpson_1_D_native = simpson_1_D,
    pielou_native = pielou_evenness
  )


observed_exotic <- diversity_by_site %>%
  filter(
    assemblage == "Exotic"
  ) %>%
  select(
    site,
    shannon_exotic = shannon,
    simpson_1_D_exotic = simpson_1_D,
    pielou_exotic = pielou_evenness
  )

native_exotic_summary <- native_exotic_summary %>%
  left_join(
    observed_native,
    by = "site"
  ) %>%
  left_join(
    observed_exotic,
    by = "site"
  ) %>%
  arrange(
    site
  )

cat("\n--- Composición Native / Exotic por sitio ---\n")

print(
  native_exotic_summary,
  n = Inf,
  width = Inf
)

# 5. DIVERSIDAD TOTAL ESTANDARIZADA CON iNEXT
#-------------------------------------------------------------------------------
#
# La diversidad total se estima a partir de abundancia de tallos.
# Las tres parcelas efectivas se agrupan como una comunidad por sitio.
#
# Se estiman números de Hill:
#
#   q = 0  riqueza de especies
#   q = 1  diversidad de Shannon
#   q = 2  diversidad de Simpson
#
# Las comparaciones se estandarizan a una cobertura de muestra de 0.95.
#-------------------------------------------------------------------------------

# 5.1 Abundancia de tallos por sitio y especie
stem_abundance_total <- stems %>%
  count(
    site,
    scientific_name,
    name = "n_stems"
  ) %>%
  arrange(
    site,
    scientific_name
  )

# Convertir a lista de vectores de abundancia para iNEXT
abundance_list_total <- stem_abundance_total %>%
  split(
    .$site
  ) %>%
  map(
    ~ deframe(
      select(
        .x,
        scientific_name,
        n_stems
      )
    )
  )

# 5.2 Cobertura observada
#-------------------------------------------------------------------------------

coverage_total <- DataInfo(
  abundance_list_total,
  datatype = "abundance"
) %>%
  as_tibble() %>%
  select(
    Assemblage,
    n,
    S.obs,
    SC
  )

cat("\n--- Cobertura observada: diversidad total ---\n")

print(
  coverage_total,
  n = Inf,
  width = Inf
)

# 5.3 Estimación iNEXT a cobertura 0.95
inext_total <- estimateD(
  abundance_list_total,
  q = c(
    0,
    1,
    2
  ),
  datatype = "abundance",
  base = "coverage",
  level = coverage_level,
  nboot = 100
)

cat(
  "\n--- Diversidad total iNEXT a cobertura ",
  coverage_level,
  " ---\n",
  sep = ""
)

inext_total %>%
  as_tibble() %>%
  print(
    n = Inf,
    width = Inf
  )

# 5.4 Tabla final por sitio
site_diversity_summary <- inext_total %>%
  transmute(
    ID_site = Assemblage,
    Order.q,
    qD,
    qD.LCL,
    qD.UCL
  ) %>%
  
  pivot_wider(
    names_from = Order.q,
    values_from = c(
      qD,
      qD.LCL,
      qD.UCL
    ),
    names_glue = "{.value}_q{Order.q}"
  ) %>%
  
  rename(
    qD_q0 = qD_q0,
    qD_q1 = qD_q1,
    qD_q2 = qD_q2,
    
    qD_q0_LCL = qD.LCL_q0,
    qD_q1_LCL = qD.LCL_q1,
    qD_q2_LCL = qD.LCL_q2,
    
    qD_q0_UCL = qD.UCL_q0,
    qD_q1_UCL = qD.UCL_q1,
    qD_q2_UCL = qD.UCL_q2
  ) %>%
  
  left_join(
    plots_per_site %>%
      rename(
        ID_site = site
      ),
    by = "ID_site"
  ) %>%
  
  arrange(
    ID_site
  )

cat("\n--- Resumen final de diversidad total ---\n")

print(
  site_diversity_summary,
  n = Inf,
  width = Inf
)

# write_csv(
#   site_diversity_summary,
#   file.path(
#     out_dir,
#     "Diversity_total.csv"
#   )
# )

# 6. DIVERSIDAD NATIVA ESTANDARIZADA CON iNEXT
#-------------------------------------------------------------------------------
#
# La comunidad nativa se analiza por separado porque al excluir las especies
# exóticas cambian las abundancias y la cobertura de muestra.
#
# La cobertura común se obtiene a partir del propio conjunto nativo.

# 6.1 Tallos nativos por sitio y especie
stems_native <- stems %>%
  filter(
    Origin == "Native"
  )

stem_abundance_native <- stems_native %>%
  count(
    site,
    scientific_name,
    name = "n_stems"
  ) %>%
  arrange(
    site,
    scientific_name
  )

abundance_list_native <- stem_abundance_native %>%
  split(
    .$site
  ) %>%
  map(
    ~ deframe(
      select(
        .x,
        scientific_name,
        n_stems
      )
    )
  )

# 6.2 Cobertura observada
coverage_native <- DataInfo(
  abundance_list_native,
  datatype = "abundance"
) %>%
  as_tibble() %>%
  transmute(
    site = Assemblage,
    n_stems_native_iNEXT = n,
    observed_native_species_iNEXT = S.obs,
    native_coverage_observed = SC
  ) %>%
  arrange(
    site
  )

cat("\n--- Cobertura observada: biodiversidad nativa ---\n")

print(
  coverage_native,
  n = Inf,
  width = Inf
)

# 6.3 Determinar cobertura común
native_coverage_auto <- estimateD(
  abundance_list_native,
  q = c(0, 1, 2),
  datatype = "abundance",
  base = "coverage",
  level = NULL,
  conf = 0.95
) %>%
  as_tibble()

coverage_level_native <- min(
  native_coverage_auto$SC,
  na.rm = TRUE
)

cat(
  "\nCobertura común seleccionada para biodiversidad nativa:",
  round(
    coverage_level_native,
    4
  ),
  "\n"
)

# 6.4 Estimar diversidad nativa a cobertura común
inext_native <- estimateD(
  abundance_list_native,
  q = c(0, 1, 2),
  datatype = "abundance",
  base = "coverage",
  level = coverage_level_native,
  conf = 0.95
) %>%
  as_tibble()


cat("\n--- Diversidad nativa estandarizada ---\n")

print(
  inext_native,
  n = Inf,
  width = Inf
)

# Método utilizado por sitio
native_method <- inext_native %>%
  distinct(
    Assemblage,
    m,
    Method,
    SC
  ) %>%
  transmute(
    site = Assemblage,
    native_coverage_standardized = SC,
    native_method = Method,
    native_sample_size_m = m
  )

# 6.5 Tabla final
site_diversity_native_summary <- inext_native %>%
  mutate(
    hill_order = paste0(
      "q",
      Order.q
    )
  ) %>%
  select(
    site = Assemblage,
    hill_order,
    qD,
    qD.LCL,
    qD.UCL
  ) %>%
  pivot_wider(
    names_from = hill_order,
    values_from = c(
      qD,
      qD.LCL,
      qD.UCL
    )
  ) %>%
  rename(
    qD_q0_native = qD_q0,
    qD.LCL_q0_native = qD.LCL_q0,
    qD.UCL_q0_native = qD.UCL_q0,
    
    qD_q1_native = qD_q1,
    qD.LCL_q1_native = qD.LCL_q1,
    qD.UCL_q1_native = qD.UCL_q1,
    
    qD_q2_native = qD_q2,
    qD.LCL_q2_native = qD.LCL_q2,
    qD.UCL_q2_native = qD.UCL_q2
  ) %>%
  left_join(
    coverage_native,
    by = "site"
  ) %>%
  left_join(
    native_method,
    by = "site"
  ) %>%
  select(
    site,
    n_stems_native_iNEXT,
    observed_native_species_iNEXT,
    native_coverage_observed,
    native_coverage_standardized,
    native_method,
    native_sample_size_m,
    
    qD_q0_native,
    qD.LCL_q0_native,
    qD.UCL_q0_native,
    
    qD_q1_native,
    qD.LCL_q1_native,
    qD.UCL_q1_native,
    
    qD_q2_native,
    qD.LCL_q2_native,
    qD.UCL_q2_native
  ) %>%
  arrange(
    site
  )

cat("\n--- Resumen final de biodiversidad nativa ---\n")

print(
  site_diversity_native_summary,
  n = Inf,
  width = Inf
)

# write_csv(
#   site_diversity_native_summary,
#   file.path(
#     out_dir,
#     "Diversity_native.csv"
#   )
# )

# 7. DIVERSIDAD EXÓTICA ESTANDARIZADA CON iNEXT
#-------------------------------------------------------------------------------
#
# Solo los sitios con tallos exóticos pueden entrar a iNEXT.
#
# Los sitios sin especies exóticas se incorporan posteriormente como ceros
# estructurales. Estos valores indican ausencia del componente exótico y no
# corresponden a estimaciones realizadas por iNEXT.


# 7.1 Tallos exóticos por sitio y especie
stems_exotic <- stems %>%
  filter(
    Origin == "Exotic"
  )

stem_abundance_exotic <- stems_exotic %>%
  count(
    site,
    scientific_name,
    name = "n_stems"
  ) %>%
  arrange(
    site,
    scientific_name
  )

sites_with_exotics <- unique(
  stems_exotic$site
)

sites_without_exotics <- setdiff(
  valid_sites,
  sites_with_exotics
)


cat("\nSitios con especies exóticas:\n")
print(sites_with_exotics)

cat("\nSitios sin especies exóticas:\n")
print(sites_without_exotics)

# Lista de abundancias para iNEXT
abundance_list_exotic <- stem_abundance_exotic %>%
  split(
    .$site
  ) %>%
  map(
    ~ deframe(
      select(
        .x,
        scientific_name,
        n_stems
      )
    )
  )

# 7.2 Cobertura observada
coverage_exotic <- DataInfo(
  abundance_list_exotic,
  datatype = "abundance"
) %>%
  as_tibble() %>%
  transmute(
    site = Assemblage,
    n_stems_exotic_iNEXT = n,
    observed_exotic_species_iNEXT = S.obs,
    exotic_coverage_observed = SC
  )

cat("\n--- Cobertura observada: biodiversidad exótica ---\n")

print(
  coverage_exotic,
  n = Inf,
  width = Inf
)

# 7.3 Determinar cobertura común
exotic_coverage_auto <- estimateD(
  abundance_list_exotic,
  q = c(0, 1, 2),
  datatype = "abundance",
  base = "coverage",
  level = NULL,
  conf = 0.95
) %>%
  as_tibble()


coverage_level_exotic <- min(
  exotic_coverage_auto$SC,
  na.rm = TRUE
)

cat(
  "\nCobertura común seleccionada para biodiversidad exótica:",
  round(
    coverage_level_exotic,
    4
  ),
  "\n"
)

# 7.4 Estimar diversidad exótica
inext_exotic <- estimateD(
  abundance_list_exotic,
  q = c(0, 1, 2),
  datatype = "abundance",
  base = "coverage",
  level = coverage_level_exotic,
  conf = 0.95
) %>%
  as_tibble()

cat("\n--- Diversidad exótica estandarizada ---\n")

print(
  inext_exotic,
  n = Inf,
  width = Inf
)

# Método utilizado por sitio
exotic_method <- inext_exotic %>%
  distinct(
    Assemblage,
    m,
    Method,
    SC
  ) %>%
  transmute(
    site = Assemblage,
    exotic_coverage_standardized = SC,
    exotic_method = Method,
    exotic_sample_size_m = m
  )

# 7.5 Sitios con especies exóticas
exotic_present <- inext_exotic %>%
  mutate(
    hill_order = paste0(
      "q",
      Order.q
    )
  ) %>%
  select(
    site = Assemblage,
    hill_order,
    qD,
    qD.LCL,
    qD.UCL
  ) %>%
  pivot_wider(
    names_from = hill_order,
    values_from = c(
      qD,
      qD.LCL,
      qD.UCL
    )
  ) %>%
  rename(
    qD_q0_exotic = qD_q0,
    qD.LCL_q0_exotic = qD.LCL_q0,
    qD.UCL_q0_exotic = qD.UCL_q0,
    
    qD_q1_exotic = qD_q1,
    qD.LCL_q1_exotic = qD.LCL_q1,
    qD.UCL_q1_exotic = qD.UCL_q1,
    
    qD_q2_exotic = qD_q2,
    qD.LCL_q2_exotic = qD.LCL_q2,
    qD.UCL_q2_exotic = qD.UCL_q2
  ) %>%
  left_join(
    coverage_exotic,
    by = "site"
  ) %>%
  left_join(
    exotic_method,
    by = "site"
  ) %>%
  mutate(
    exotic_estimation_status = "iNEXT estimate"
  )

# 7.6 Ceros estructurales
exotic_zero <- tibble(
  site = sites_without_exotics,
  
  n_stems_exotic_iNEXT = 0,
  observed_exotic_species_iNEXT = 0,
  
  exotic_coverage_observed = NA_real_,
  exotic_coverage_standardized = NA_real_,
  exotic_method = "Structural zero",
  exotic_sample_size_m = NA_real_,
  
  qD_q0_exotic = 0,
  qD.LCL_q0_exotic = 0,
  qD.UCL_q0_exotic = 0,
  
  qD_q1_exotic = 0,
  qD.LCL_q1_exotic = 0,
  qD.UCL_q1_exotic = 0,
  
  qD_q2_exotic = 0,
  qD.LCL_q2_exotic = 0,
  qD.UCL_q2_exotic = 0,
  
  exotic_estimation_status = "Structural zero"
)

# 7.7 Tabla final de diversidad exótica
site_diversity_exotic_summary <- bind_rows(
  exotic_present,
  exotic_zero
) %>%
  select(
    site,
    
    n_stems_exotic_iNEXT,
    observed_exotic_species_iNEXT,
    
    exotic_coverage_observed,
    exotic_coverage_standardized,
    exotic_method,
    exotic_sample_size_m,
    
    qD_q0_exotic,
    qD.LCL_q0_exotic,
    qD.UCL_q0_exotic,
    
    qD_q1_exotic,
    qD.LCL_q1_exotic,
    qD.UCL_q1_exotic,
    
    qD_q2_exotic,
    qD.LCL_q2_exotic,
    qD.UCL_q2_exotic,
    
    exotic_estimation_status
  ) %>%
  arrange(
    site
  )

cat("\n--- Resumen final de biodiversidad exótica ---\n")

print(
  site_diversity_exotic_summary,
  n = Inf,
  width = Inf
)

# write_csv(
#   site_diversity_exotic_summary,
#   file.path(
#     out_dir,
#     "Diversity_exotic.csv"
#   )
# )

# 8. COMPOSICIÓN NATIVE / EXOTIC
#-------------------------------------------------------------------------------

site_native_exotic_composition <- native_exotic_summary %>%
  select(
    site,
    
    n_individuals_total,
    n_individuals_native,
    n_individuals_exotic,
    
    prop_individuals_native,
    prop_individuals_exotic,
    
    richness_total,
    richness_native,
    richness_exotic,
    
    prop_richness_native,
    prop_richness_exotic,
    
    shannon_native,
    simpson_1_D_native
  ) %>%
  arrange(
    site
  )

# write_csv(
#   site_native_exotic_composition,
#   file.path(
#     out_dir,
#     "Native_exotic_composition.csv"
#   )
# )

cat("\n--- Composición Native / Exotic ---\n")
print(
  site_native_exotic_composition,
  n = Inf,
  width = Inf
)

# 9. FIGURAS DE DIVERSIDAD
#-------------------------------------------------------------------------------

# Orden de sitios según el gradiente de antropización
site_levels <- tibble(
  # Final gAT order: sites are grouped by final quintile and ordered by
  # increasing site-level gAT within each quintile.
  site = c(
    "SJDP", "TDP", "EPR",
    "CH",
    "QDM", "BDT", "PILC", "LP",
    "ACOG", "EMV",
    "CLVR", "CVLL"
  ),
  anthro_level = factor(
    c(
      1, 1, 1,
      2,
      3, 3, 3, 3,
      4, 4,
      5, 5
    ),
    levels = 1:5,
    labels = paste0("Q", 1:5)
  )
)

# 9.1 Diversidad total
plot_total <- inext_total %>%
  transmute(
    site = Assemblage,
    Order.q,
    qD,
    qD.LCL,
    qD.UCL
  ) %>%
  left_join(
    site_levels,
    by = "site"
  ) %>%
  mutate(
    site = factor(
      site,
      levels = site_levels$site
    ),
    
    hill_order = factor(
      Order.q,
      levels = c(0, 1, 2),
      labels = c(
        "q = 0 · Species richness",
        "q = 1 · Shannon diversity",
        "q = 2 · Simpson diversity"
      )
    )
  )


figure_diversity_total <- ggplot(
  plot_total,
  aes(
    x = site,
    y = qD,
    color = anthro_level
  )
) +
  
  geom_errorbar(
    aes(
      ymin = qD.LCL,
      ymax = qD.UCL
    ),
    width = 0.15,
    color = "grey40",
    linewidth = 0.6
  ) +
  
  geom_point(
    size = 3
  ) +
  
  facet_wrap(
    ~ hill_order,
    scales = "free_y",
    ncol = 1
  ) +
  
  labs(
    x = "Site",
    y = "Hill diversity",
    color = "Anthropization level",
    title = "Plant diversity",
    subtitle = paste0(
      "Hill numbers standardized at ",
      coverage_level * 100,
      "% sample coverage"
    )
  ) +
  
  theme_classic(
    base_size = 11
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    strip.text = element_text(
      face = "bold"
    ),
    
    legend.position = "right"
  )


figure_diversity_total

# ggsave(
#   filename = file.path(
#     out_dir,
#     "Figure_diversity_total.png"
#   ),
#   plot = figure_diversity_total,
#   width = 8,
#   height = 8,
#   dpi = 600,
#   bg = "white"
# )

# 9.2 Diversidad nativa
plot_native <- inext_native %>%
  transmute(
    site = Assemblage,
    Order.q,
    qD,
    qD.LCL,
    qD.UCL
  ) %>%
  left_join(
    site_levels,
    by = "site"
  ) %>%
  mutate(
    site = factor(
      site,
      levels = site_levels$site
    ),
    
    hill_order = factor(
      Order.q,
      levels = c(0, 1, 2),
      labels = c(
        "q = 0 · Native species richness",
        "q = 1 · Native Shannon diversity",
        "q = 2 · Native Simpson diversity"
      )
    )
  )


figure_diversity_native <- ggplot(
  plot_native,
  aes(
    x = site,
    y = qD,
    color = anthro_level
  )
) +
  
  geom_errorbar(
    aes(
      ymin = qD.LCL,
      ymax = qD.UCL
    ),
    width = 0.15,
    color = "grey40",
    linewidth = 0.6
  ) +
  
  geom_point(
    size = 3
  ) +
  
  facet_wrap(
    ~ hill_order,
    scales = "free_y",
    ncol = 1
  ) +
  
  labs(
    x = "Site",
    y = "Hill diversity",
    color = "Anthropization level",
    title = "Native plant diversity",
    subtitle = paste0(
      "Hill numbers standardized at ",
      round(
        coverage_level_native * 100,
        1
      ),
      "% sample coverage"
    )
  ) +
  
  theme_classic(
    base_size = 11
  ) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    strip.text = element_text(
      face = "bold"
    ),
    
    legend.position = "right"
  )


figure_diversity_native

# ggsave(
#   filename = file.path(
#     out_dir,
#     "Figure_diversity_native.png"
#   ),
#   plot = figure_diversity_native,
#   width = 8,
#   height = 8,
#   dpi = 600,
#   bg = "white"
# )

# 10. RESUMEN FINAL
#-------------------------------------------------------------------------------

cat(
  "\n==================================================\n",
  "ANÁLISIS DE BIODIVERSIDAD COMPLETADO\n",
  "==================================================\n"
)

cat(
  "Umbral de DAP:",
  dap_min_cm,
  "cm\n"
)

cat(
  "Individuos únicos:",
  nrow(individuals),
  "\n"
)

cat(
  "Tallos utilizados en iNEXT:",
  nrow(stems),
  "\n"
)

cat(
  "Cobertura biodiversidad total:",
  coverage_level,
  "\n"
)

cat(
  "Cobertura biodiversidad nativa:",
  round(
    coverage_level_native,
    4
  ),
  "\n"
)

cat(
  "Cobertura biodiversidad exótica:",
  round(
    coverage_level_exotic,
    4
  ),
  "\n"
)

cat(
  "\nArchivos principales generados en:\n",
  out_dir,
  "\n"
)

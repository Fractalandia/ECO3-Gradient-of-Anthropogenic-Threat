#===============================================================================
# QUESTIONNAIRE THREAT ANALYSIS – ECO3
#===============================================================================
#
# Input:
#   questionnaire_anonymized_raw.csv
#
# Analytical calculations are unchanged from the verified questionnaire workflow.
# Final ECO3 quintiles are used only as graphical metadata.
#===============================================================================

library(tidyverse)
library(lubridate)
library(janitor)

#PATHS
desktop <- "C:/Users/Dell/Desktop"

questionnaire_out_dir <- file.path(
  desktop,
  "Patricio_clean_data",
  "09_Questionnaire_ANALYSIS"
)

dir.create(
  questionnaire_out_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

raw_file <- file.path(
  questionnaire_out_dir,
  "questionnaire_anonymized_raw.csv"
)

raw <- read.csv(
  raw_file,
  sep = ";",
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# Kobo exports contain repeated question labels.
# Make column names unique without changing questionnaire values.
names(raw) <- make.unique(names(raw))

# 2. SITIOS VÁLIDOS: SOLO CON 3 PARCELAS EFECTIVAS
#-------------------------------------------------------------------------------

valid_sites <- c(
  "CH", "LP", "TDP", "BDT", "EPr", "SJdP",
  "CVLL", "PILC", "ACog", "CLVR", "QdM", "EMV"
)

# Final ECO3 anthropization quintiles.
# Used only for figure metadata; they do not enter questionnaire calculations.
site_quintiles <- tibble(
  site_code = c(
    "SJdP", "TDP", "EPr",
    "CH",
    "QdM", "BDT", "PILC", "LP",
    "ACog", "EMV",
    "CLVR", "CVLL"
  ),
  SIG_quintile = factor(
    c(
      "Q1", "Q1", "Q1",
      "Q2",
      "Q3", "Q3", "Q3", "Q3",
      "Q4", "Q4",
      "Q5", "Q5"
    ),
    levels = c("Q1", "Q2", "Q3", "Q4", "Q5")
  )
)

# 3. EXTRAER COLUMNAS CUANTITATIVAS
#-------------------------------------------------------------------------------

quant <- raw %>%
  transmute(
    start,
    end,
    submission_time = `_submission_time`,
    
    site_name_raw = `Nombre del sitio: `,
    site_id_raw   = `ID del sitio: `,
    date_eval     = `Fecha de evaluación:`,
    evaluator_raw = `Tipo de evaluador`,
    
    prod_present = `¿Se desarrollan actividades productivas en este sitio?`,
    prod_ext     = `Extensión espacial: ¿Qué proporción del sitio se ve afectada por actividades productivas?`,
    prod_sev     = `Severidad del impacto: ¿En qué medida esta amenaza afecta la condición del sitio (por ejemplo, suelo, vegetación o procesos naturales de regeneración)? `,
    prod_time    = `Estado temporal: ¿Esta amenaza se encuentra actualmente activa en el sitio? ¿O en qué periodo fue relevante?`,
    
    extract_present = `¿Se realizan actividades de extracción de recursos en este sitio?`,
    extract_ext     = `Extensión espacial: ¿Qué proporción del sitio se ve afectada por actividades de extracción de recursos?`,
    extract_sev     = `Severidad del impacto: ¿En qué medida esta amenaza afecta la condición del sitio (por ejemplo, suelo, vegetación o procesos naturales de regeneración)? .1`,
    extract_time    = `Estado temporal: ¿Esta amenaza se encuentra actualmente activa en el sitio? ¿O en qué periodo fue relevante?.1`,
    
    fire_present = `¿Los incendios representan una amenaza en este sitio?`,
    fire_ext     = `Extensión espacial:¿Qué proporción del sitio está o ha sido afectada por incendios?`,
    fire_sev     = `Severidad del impacto: ¿Qué tan severos han sido los impactos del fuego (por ejemplo, pérdida de cobertura vegetal, suelo expuesto, etc.)?`,
    fire_time    = `Estado temporal: ¿Cuándo fue la última temporada de incendios que afectó al sitio?`,
    
    invasive_present = `¿Existen especies exóticas o invasoras (no nativas) presentes en este sitio?`,
    invasive_ext     = `Extensión espacial: ¿Qué proporción del sitio se ve afectada por especies invasoras?`,
    invasive_sev     = `Severidad del impacto: ¿Qué tan severo ha sido el impacto de las especies exóticas o invasoras en el sitio (por ejemplo, desplazamiento de vegetación nativa)? `,
    invasive_time    = `Estado temporal: ¿Esta amenaza se encuentra actualmente activa en el sitio? ¿O en qué periodo fue relevante?.2`,
    
    rec_present = `¿Este sitio es utilizado para actividades recreativas humanas?`,
    rec_ext     = `Extensión espacial: ¿Este sitio es utilizado para actividades recreativas humanas?`,
    rec_sev     = `Severidad del impacto: ¿En qué medida esta amenaza afecta la condición del sitio (por ejemplo, erosión, acumulación de residuos, etc.)?`,
    rec_time    = `Estado temporal: ¿Esta amenaza se encuentra actualmente activa en el sitio? ¿O en qué periodo fue relevante?.3`
  )

# 4. HOMOGENEIZAR ETIQUETAS DE SITIO Y EVALUADOR
#-------------------------------------------------------------------------------

quant <- quant %>%
  mutate(
    site_id_clean   = str_squish(site_id_raw),
    site_name_clean = str_squish(site_name_raw),
    
    site_code = case_when(
      str_detect(site_id_clean, regex("^CH", ignore_case = TRUE)) ~ "CH",
      str_detect(site_id_clean, regex("LP", ignore_case = TRUE)) ~ "LP",
      str_detect(site_id_clean, regex("TDP|TP", ignore_case = TRUE)) ~ "TDP",
      str_detect(site_id_clean, regex("BDT|BT", ignore_case = TRUE)) ~ "BDT",
      str_detect(site_id_clean, regex("EPR", ignore_case = TRUE)) ~ "EPr",
      str_detect(site_id_clean, regex("SJDP", ignore_case = TRUE)) ~ "SJdP",
      str_detect(site_id_clean, regex("CVLL", ignore_case = TRUE)) ~ "CVLL",
      str_detect(site_id_clean, regex("PILC", ignore_case = TRUE)) ~ "PILC",
      
      # En la encuesta del administrador de PILC el ID figura como "Na".
      # Se recupera el sitio mediante su nombre.
      str_detect(
        site_name_clean,
        regex("LOS CRUCEROS|PIRAGUA", ignore_case = TRUE)
      ) ~ "PILC",
      
      str_detect(site_id_clean, regex("ACOG", ignore_case = TRUE)) ~ "ACog",
      str_detect(site_id_clean, regex("CLVR", ignore_case = TRUE)) ~ "CLVR",
      str_detect(site_id_clean, regex("QDM", ignore_case = TRUE)) ~ "QdM",
      str_detect(site_id_clean, regex("EMV", ignore_case = TRUE)) ~ "EMV",
      TRUE ~ NA_character_
    ),
    
    evaluator = case_when(
      str_detect(site_id_clean, regex("T1", ignore_case = TRUE)) ~ "Administrador",
      str_detect(site_id_clean, regex("T2", ignore_case = TRUE)) ~ "Investigador",
      str_detect(
        evaluator_raw,
        regex("gestor|administrador", ignore_case = TRUE)
      ) ~ "Administrador",
      str_detect(
        evaluator_raw,
        regex("equipo|investigacion|investigación", ignore_case = TRUE)
      ) ~ "Investigador",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(site_code %in% valid_sites) %>%
  arrange(site_code, date_eval, submission_time) %>%
  group_by(site_code) %>%
  mutate(
    # En BDT ambos IDs fueron registrados como T1.
    evaluator = if_else(
      site_code == "BDT" & row_number() == 2,
      "Investigador",
      evaluator
    )
  ) %>%
  ungroup()

# 5. CONTROL DE CALIDAD INICIAL
#-------------------------------------------------------------------------------

qc_interviews <- quant %>%
  count(site_code, evaluator) %>%
  pivot_wider(
    names_from = evaluator,
    values_from = n,
    values_fill = 0
  ) %>%
  arrange(site_code)

print(qc_interviews)

qc_labels <- quant %>%
  arrange(site_code, date_eval, submission_time) %>%
  select(
    site_code,
    site_name_raw,
    site_id_raw,
    date_eval,
    submission_time,
    evaluator
  )

print(qc_labels)

# Comprobar que ningún sitio válido quedó sin clasificar
missing_valid_labels <- raw %>%
  transmute(
    site_name_raw = `Nombre del sitio: `,
    site_id_raw   = `ID del sitio: `
  ) %>%
  filter(!is.na(site_name_raw) | !is.na(site_id_raw)) %>%
  anti_join(
    quant %>% distinct(site_name_raw, site_id_raw),
    by = c("site_name_raw", "site_id_raw")
  )

print(missing_valid_labels)

# 6. LIMPIEZA Y NORMALIZACIÓN
#-------------------------------------------------------------------------------

to_score <- function(x) {
  as.numeric(str_extract(as.character(x), "^[0-9]+"))
}

is_absent <- function(x) {
  str_detect(
    str_squish(as.character(x)),
    regex("^no$|no aplica|ausente|absent", ignore_case = TRUE)
  )
}

norm_5 <- function(x) {
  (x - 1) / 4
}

norm_4 <- function(x) {
  (x - 1) / 3
}

# 7. PASAR A FORMATO LARGO
#-------------------------------------------------------------------------------

threat_long <- quant %>%
  select(
    site_code,
    site_name_raw,
    site_id_raw,
    date_eval,
    submission_time,
    evaluator,
    matches("^(prod|extract|fire|invasive|rec)_(present|ext|sev|time)$")
  ) %>%
  pivot_longer(
    cols = matches("^(prod|extract|fire|invasive|rec)_(present|ext|sev|time)$"),
    names_to = c("threat", ".value"),
    names_pattern = "(.+)_(present|ext|sev|time)"
  )

# 8. NORMALIZAR PUNTAJES
#    Amenaza ausente = 0
#    Temporalidad 5 = desconocido = NA
#-------------------------------------------------------------------------------

threat_scored <- threat_long %>%
  mutate(
    present_absent = is_absent(present),
    
    ext_num  = to_score(ext),
    sev_num  = to_score(sev),
    time_num = to_score(time),
    
    time_num = if_else(time_num == 5, NA_real_, time_num),
    
    ext_norm = case_when(
      present_absent ~ 0,
      TRUE ~ norm_5(ext_num)
    ),
    
    sev_norm = case_when(
      present_absent ~ 0,
      TRUE ~ norm_5(sev_num)
    ),
    
    time_norm = case_when(
      present_absent ~ 0,
      TRUE ~ norm_4(time_num)
    ),
    
    threat_impact = rowMeans(
      cbind(ext_norm, sev_norm, time_norm),
      na.rm = TRUE
    ),
    
    # Evita NaN si las tres dimensiones están ausentes
    threat_impact = if_else(
      is.nan(threat_impact),
      NA_real_,
      threat_impact
    )
  )

# 8.1 PREPARAR DATOS PARA RED DE CORRELACIONES
#-------------------------------------------------------------------------------
# Objetivo:
# Analizar las relaciones internas entre las cinco categorías de amenazas
# y sus tres dimensiones:
#
#   - extensión espacial
#   - severidad
#   - estado temporal
#
# Unidad de análisis:
# Un valor promedio por sitio, categoría de amenaza y dimensión.
#
# Los evaluadores disponibles para cada sitio se promedian para evitar que
# sitios con más entrevistas tengan mayor peso en las correlaciones.

# 8.1.1 Promedio por sitio y categoría de amenaza
questionnaire_network_long <- threat_scored %>%
  group_by(
    site_code,
    threat
  ) %>%
  summarise(
    extent = mean(
      ext_norm,
      na.rm = TRUE
    ),
    
    severity = mean(
      sev_norm,
      na.rm = TRUE
    ),
    
    temporal = mean(
      time_norm,
      na.rm = TRUE
    ),
    
    .groups = "drop"
  ) %>%
  mutate(
    across(
      c(
        extent,
        severity,
        temporal
      ),
      ~ if_else(
        is.nan(.x),
        NA_real_,
        .x
      )
    )
  )

cat(
  "\n--- Questionnaire dimensions by site and threat ---\n"
)

print(
  questionnaire_network_long,
  n = Inf,
  width = Inf
)

# 8.1.2 Convertir a una fila por sitio
# Se generan 15 variables:
#   5 categorías de amenaza × 3 dimensiones.

questionnaire_network_data <- questionnaire_network_long %>%
  pivot_longer(
    cols = c(
      extent,
      severity,
      temporal
    ),
    
    names_to = "dimension",
    
    values_to = "value"
  ) %>%
  mutate(
    variable = paste(
      threat,
      dimension,
      sep = "_"
    )
  ) %>%
  select(
    site_code,
    variable,
    value
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  ) %>%
  arrange(site_code)

cat(
  "\n--- Questionnaire network dataset ---\n"
)

print(
  questionnaire_network_data,
  n = Inf,
  width = Inf
)

cat(
  "\n--- Dimensions of questionnaire network dataset ---\n"
)

print(
  dim(questionnaire_network_data)
)

# 8.1.3 Control de calidad
expected_network_variables <- c(
  "prod_extent",
  "prod_severity",
  "prod_temporal",
  
  "extract_extent",
  "extract_severity",
  "extract_temporal",
  
  "fire_extent",
  "fire_severity",
  "fire_temporal",
  
  "invasive_extent",
  "invasive_severity",
  "invasive_temporal",
  
  "rec_extent",
  "rec_severity",
  "rec_temporal"
)

missing_network_variables <- setdiff(
  expected_network_variables,
  names(questionnaire_network_data)
)

if (length(missing_network_variables) > 0) {
  stop(
    "Missing questionnaire network variables: ",
    paste(
      missing_network_variables,
      collapse = ", "
    )
  )
}

if (nrow(questionnaire_network_data) != 12) {
  warning(
    "Expected 12 sites in the questionnaire network dataset, but found ",
    nrow(questionnaire_network_data),
    "."
  )
}

# 8.1.4 Matriz de correlaciones de Spearman
questionnaire_cor_data <- questionnaire_network_data %>%
  select(
    all_of(expected_network_variables)
  )

questionnaire_cor <- cor(
  questionnaire_cor_data,
  method = "spearman",
  use = "pairwise.complete.obs"
)

cat(
  "\n--- Questionnaire Spearman correlation matrix ---\n"
)

print(
  round(
    questionnaire_cor,
    2
  )
)

# 8.1.5 Resumen de correlaciones según umbral
questionnaire_cor_pairs <- as.data.frame(
  as.table(questionnaire_cor)
) %>%
  rename(
    variable_1 = Var1,
    variable_2 = Var2,
    rho = Freq
  ) %>%
  mutate(
    variable_1 = as.character(variable_1),
    variable_2 = as.character(variable_2)
  ) %>%
  filter(
    variable_1 != variable_2
  ) %>%
  rowwise() %>%
  mutate(
    pair_id = paste(
      sort(
        c(
          variable_1,
          variable_2
        )
      ),
      collapse = " | "
    )
  ) %>%
  ungroup() %>%
  distinct(
    pair_id,
    .keep_all = TRUE
  ) %>%
  arrange(
    desc(abs(rho))
  )

cat(
  "\n--- Strongest questionnaire correlations ---\n"
)

questionnaire_cor_pairs %>%
  slice_head(n = 20) %>%
  print(
    n = 20,
    width = Inf
  )

correlation_threshold_questionnaire <- 0.25

cat(
  "\n--- Number of correlations with |rho| >= ",
  correlation_threshold_questionnaire,
  " ---\n",
  sep = ""
)

questionnaire_cor_pairs %>%
  summarise(
    n_correlations = sum(
      abs(rho) >= correlation_threshold_questionnaire,
      na.rm = TRUE
    )
  ) %>%
  print()

# 8.1.6 Compare possible thresholds
questionnaire_cor_pairs %>%
  summarise(
    threshold_025 = sum(abs(rho) >= 0.25, na.rm = TRUE),
    threshold_040 = sum(abs(rho) >= 0.40, na.rm = TRUE),
    threshold_050 = sum(abs(rho) >= 0.50, na.rm = TRUE),
    threshold_060 = sum(abs(rho) >= 0.60, na.rm = TRUE),
    threshold_070 = sum(abs(rho) >= 0.70, na.rm = TRUE)
  ) %>%
  print(width = Inf)

# 8.2 BUILD NETWORK OBJECTS
#-------------------------------------------------------------------------------

correlation_threshold_questionnaire <- 0.50

questionnaire_edges <- questionnaire_cor_pairs %>%
  filter(
    abs(rho) >= correlation_threshold_questionnaire
  ) %>%
  mutate(
    
    sign = ifelse(
      rho > 0,
      "positive",
      "negative"
    ),
    
    strength = abs(rho)
    
  )

cat("\n--- Network edges retained ---\n")

print(
  questionnaire_edges,
  n = Inf,
  width = Inf
)

cat(
  "\nNumber of edges:",
  nrow(questionnaire_edges),
  "\n"
)

questionnaire_nodes <- tibble(
  
  variable = colnames(questionnaire_cor),
  
  threat = case_when(
    
    str_detect(variable, "^prod") ~ "Productive",
    
    str_detect(variable, "^extract") ~ "Extraction",
    
    str_detect(variable, "^fire") ~ "Fire",
    
    str_detect(variable, "^invasive") ~ "Invasive",
    
    str_detect(variable, "^rec") ~ "Recreation"
    
  ),
  
  dimension = case_when(
    
    str_detect(variable, "extent") ~ "Extent",
    
    str_detect(variable, "severity") ~ "Severity",
    
    str_detect(variable, "temporal") ~ "Temporal"
    
  )
  
)

print(questionnaire_nodes)

# 8.3 QUESTIONNAIRE CORRELATION NETWORK
#-------------------------------------------------------------------------------

# 8.3.1 Manual circular layout
# Each threat category occupies one clearly separated sector.
# Within each sector, nodes are ordered as:
# Extent -> Severity -> Temporal.

questionnaire_nodes_plot <- questionnaire_nodes %>%
  mutate(
    angle_deg = case_when(
      
      # Productive: upper sector
      variable == "prod_extent"   ~ 110,
      variable == "prod_severity" ~ 86,
      variable == "prod_temporal" ~ 62,
      
      # Extraction: upper-right sector
      variable == "extract_extent"   ~ 44,
      variable == "extract_severity" ~ 20,
      variable == "extract_temporal" ~ -4,
      
      # Fire: lower-right sector
      variable == "fire_extent"   ~ -32,
      variable == "fire_severity" ~ -56,
      variable == "fire_temporal" ~ -80,
      
      # Invasive species: lower-left sector
      variable == "invasive_extent"   ~ -108,
      variable == "invasive_severity" ~ -132,
      variable == "invasive_temporal" ~ -156,
      
      # Recreation: upper-left sector
      variable == "rec_temporal" ~ 182,
      variable == "rec_extent"   ~ 158,
      variable == "rec_severity" ~ 134
    ),
    
    angle = angle_deg * pi / 180,
    
    x = cos(angle),
    y = sin(angle),
    
    label_radius = 1.27,
    
    label_x = label_radius * cos(angle),
    label_y = label_radius * sin(angle),
    
    label_hjust = case_when(
      label_x >  0.15 ~ 0,
      label_x < -0.15 ~ 1,
      TRUE ~ 0.5
    )
  )

# 8.3.2 Prepare retained edges
questionnaire_edges_plot <- questionnaire_edges %>%
  transmute(
    from = variable_1,
    to = variable_2,
    rho = rho,
    
    sign = if_else(
      rho >= 0,
      "Positive",
      "Negative"
    ),
    
    strength = abs(rho),
    
    dominant_label = if_else(
      abs(rho) >= 0.70,
      sprintf("%.2f", rho),
      NA_character_
    )
  ) %>%
  
  left_join(
    questionnaire_nodes_plot %>%
      select(
        variable,
        x,
        y
      ),
    by = c("from" = "variable")
  ) %>%
  
  rename(
    x_from = x,
    y_from = y
  ) %>%
  
  left_join(
    questionnaire_nodes_plot %>%
      select(
        variable,
        x,
        y
      ),
    by = c("to" = "variable")
  ) %>%
  
  rename(
    x_to = x,
    y_to = y
  ) %>%
  
  mutate(
    midpoint_x = (x_from + x_to) / 2,
    midpoint_y = (y_from + y_to) / 2,
    
    dx = x_to - x_from,
    dy = y_to - y_from,
    
    edge_length = sqrt(
      dx^2 + dy^2
    ),
    
    perpendicular_x = -dy / edge_length,
    perpendicular_y = dx / edge_length,
    
    label_direction = rep(
      c(-1, 1),
      length.out = n()
    ),
    
    label_offset = 0.055,
    
    label_x = midpoint_x +
      perpendicular_x *
      label_offset *
      label_direction,
    
    label_y = midpoint_y +
      perpendicular_y *
      label_offset *
      label_direction
  )

# 8.3.3 Color palettes
questionnaire_block_colors <- c(
  "Productive" = "#6BAED6",
  "Extraction" = "#F4A261",
  "Fire"       = "#E76F51",
  "Invasive"   = "#8E77B8",
  "Recreation" = "#74C69D"
)

questionnaire_edge_colors <- c(
  "Positive" = "#5796C7",
  "Negative" = "#D6819B"
)

# 8.3.4 Plot
fig_questionnaire_network <- ggplot() +
  
  # Subtle outer circle
  
  annotate(
    "path",
    x = cos(
      seq(
        0,
        2 * pi,
        length.out = 300
      )
    ),
    y = sin(
      seq(
        0,
        2 * pi,
        length.out = 300
      )
    ),
    colour = "grey88",
    linewidth = 0.5
  ) +
  
  # Correlation edges
  
  geom_segment(
    data = questionnaire_edges_plot,
    aes(
      x = x_from,
      y = y_from,
      xend = x_to,
      yend = y_to,
      colour = sign,
      linewidth = strength
    ),
    alpha = 0.68,
    lineend = "round"
  ) +
  
  # Numeric labels for strongest correlations
  
  geom_label(
    data = questionnaire_edges_plot %>%
      filter(
        !is.na(dominant_label)
      ),
    aes(
      x = label_x,
      y = label_y,
      label = dominant_label
    ),
    size = 3,
    colour = "grey20",
    fill = scales::alpha(
      "white",
      0.92
    ),
    label.size = 0,
    label.padding = unit(
      0.08,
      "lines"
    )
  ) +
  
  # Nodes
  
  geom_point(
    data = questionnaire_nodes_plot,
    aes(
      x = x,
      y = y,
      fill = threat
    ),
    shape = 21,
    size = 8.5,
    colour = "white",
    stroke = 1.2
  ) +
  
  # Labels for each node:
  # Extent, Severity or Temporal
  
  geom_text(
    data = questionnaire_nodes_plot,
    aes(
      x = label_x,
      y = label_y,
      label = dimension,
      hjust = label_hjust
    ),
    size = 3.5,
    fontface = "bold",
    colour = "grey18"
  ) +
  
  scale_fill_manual(
    values = questionnaire_block_colors,
    name = "Threat category"
  ) +
  
  scale_colour_manual(
    values = questionnaire_edge_colors,
    guide = "none"
  ) +
  
  scale_linewidth_continuous(
    range = c(
      0.7,
      4.1
    ),
    guide = "none"
  ) +
  
  coord_equal(
    xlim = c(
      -1.52,
      1.52
    ),
    ylim = c(
      -1.48,
      1.48
    ),
    clip = "off"
  ) +
  
  labs(
    title = "Relationships among questionnaire threat dimensions",
    
    subtitle = paste0(
      "Spearman correlations; only |ρ| ≥ ",
      correlation_threshold_questionnaire,
      " are shown"
    )
  ) +
  
  theme_void() +
  
  theme(
    plot.title = element_text(
      size = 15,
      face = "bold",
      hjust = 0.5
    ),
    
    plot.subtitle = element_text(
      size = 10,
      hjust = 0.5,
      colour = "grey35"
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    legend.text = element_text(
      size = 9
    ),
    
    legend.key.width = unit(
      0.8,
      "cm"
    ),
    
    plot.margin = margin(
      t = 15,
      r = 55,
      b = 15,
      l = 55
    )
  )

fig_questionnaire_network

# 8.3.5 Export questionnaire correlation network
questionnaire_network_output <- file.path(
  questionnaire_out_dir,
  "Figure_questionnaire_network_ECO3.png"
)

ggsave(
  filename = questionnaire_network_output,
  plot = fig_questionnaire_network,
  width = 9,
  height = 8.2,
  units = "in",
  dpi = 600,
  bg = "white"
)

cat(
  "\nQuestionnaire correlation network exported successfully:\n",
  questionnaire_network_output,
  "\n"
)

# 9. ÍNDICE POR ENCUESTA / EVALUADOR
#-------------------------------------------------------------------------------

interview_index <- threat_scored %>%
  group_by(
    site_code,
    site_id_raw,
    date_eval,
    submission_time,
    evaluator
  ) %>%
  summarise(
    n_threats = n(),
    threat_index = mean(threat_impact, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    threat_index = if_else(
      is.nan(threat_index),
      NA_real_,
      threat_index
    )
  ) %>%
  arrange(site_code, evaluator, date_eval, submission_time)

print(interview_index)

# 10. RESOLVER DUPLICADOS POR SITIO + EVALUADOR
#-------------------------------------------------------------------------------

evaluator_index <- interview_index %>%
  group_by(site_code, evaluator) %>%
  summarise(
    n_interviews = n(),
    threat_index_eval = mean(threat_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    threat_index_eval = if_else(
      is.nan(threat_index_eval),
      NA_real_,
      threat_index_eval
    )
  ) %>%
  arrange(site_code, evaluator)

print(evaluator_index)

# 11. FINAL THREAT INDEX BY SITE
#-------------------------------------------------------------------------------

site_index <- evaluator_index %>%
  group_by(site_code) %>%
  summarise(
    n_evaluator_types = n_distinct(
      evaluator[!is.na(evaluator)]
    ),
    threat_index_site = mean(
      threat_index_eval,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    threat_index_site = if_else(
      is.nan(threat_index_site),
      NA_real_,
      threat_index_site
    )
  ) %>%
  arrange(site_code)

cat("\nFinal threat index by site:\n")
print(site_index)

# 11.1 FIGURE 1: THREAT INDEX BY SITE
#-------------------------------------------------------------------------------
# Final ECO3 quintiles are used only to color sites consistently with the
# anthropization gradient.

site_plot <- site_index %>%
  left_join(
    site_quintiles,
    by = "site_code"
  ) %>%
  filter(!is.na(threat_index_site)) %>%
  arrange(threat_index_site) %>%
  mutate(
    site_code = factor(
      site_code,
      levels = site_code
    ),
    SIG_quintile = factor(
      SIG_quintile,
      levels = c("Q1", "Q2", "Q3", "Q4", "Q5")
    )
  )

quintile_colors <- c(
  "Q1" = "#F8766D",
  "Q2" = "#A3A500",
  "Q3" = "#00BF7D",
  "Q4" = "#00B0F6",
  "Q5" = "#E76BF3"
)

fig1 <- ggplot(
  site_plot,
  aes(
    x = site_code,
    y = threat_index_site,
    fill = SIG_quintile
  )
) +
  geom_col(
    width = 0.72
  ) +
  geom_text(
    aes(
      label = sprintf(
        "%.2f",
        threat_index_site
      )
    ),
    hjust = -0.15,
    size = 3.4
  ) +
  coord_flip(
    clip = "off"
  ) +
  scale_fill_manual(
    values = quintile_colors,
    name = "Anthropization\nlevel",
    drop = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, 0.90),
    breaks = seq(
      0,
      0.9,
      0.1
    ),
    expand = expansion(
      mult = c(
        0,
        0.05
      )
    )
  ) +
  labs(
    title = "Threat index by site",
    x = NULL,
    y = "Threat index"
  ) +
  theme_bw(
    base_size = 12
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    legend.position = "right",
    legend.title = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(
      t = 10,
      r = 25,
      b = 10,
      l = 10
    )
  )

fig1

# 11.2 FIGURE 2: THREAT PROFILE BY SITE
#-------------------------------------------------------------------------------

threat_summary <- threat_scored %>%
  group_by(
    site_code,
    threat
  ) %>%
  summarise(
    mean_impact = mean(
      threat_impact,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    mean_impact = if_else(
      is.nan(mean_impact),
      NA_real_,
      mean_impact
    )
  ) %>%
  group_by(site_code) %>%
  mutate(
    total_impact = sum(
      mean_impact,
      na.rm = TRUE
    ),
    contribution = if_else(
      total_impact > 0,
      mean_impact / total_impact,
      0
    )
  ) %>%
  ungroup() %>%
  left_join(
    site_index %>%
      select(
        site_code,
        threat_index_site
      ),
    by = "site_code"
  ) %>%
  mutate(
    contribution_scaled =
      contribution *
      threat_index_site,
    
    threat = factor(
      threat,
      levels = c(
        "prod",
        "extract",
        "fire",
        "invasive",
        "rec"
      ),
      labels = c(
        "Production",
        "Extraction",
        "Fire",
        "Invasive species",
        "Recreation"
      )
    )
  ) %>%
  arrange(
    threat_index_site
  ) %>%
  mutate(
    site_code = factor(
      site_code,
      levels = unique(site_code)
    )
  )

threat_colors <- c(
  "Production" = "#2EA8DF",
  "Extraction" = "#F8766D",
  "Fire" = "#A3A500",
  "Invasive species" = "#00BF7D",
  "Recreation" = "#D65FE8"
)

fig2 <- ggplot(
  threat_summary,
  aes(
    x = site_code,
    y = contribution_scaled,
    fill = threat
  )
) +
  geom_col(
    width = 0.74
  ) +
  coord_flip() +
  scale_fill_manual(
    values = threat_colors,
    name = "Threat type",
    drop = FALSE
  ) +
  scale_y_continuous(
    limits = c(0, 0.70),
    breaks = seq(
      0,
      0.7,
      0.1
    ),
    expand = expansion(
      mult = c(
        0,
        0.02
      )
    )
  ) +
  labs(
    title = "Threat profile by site",
    x = NULL,
    y = "Threat index"
  ) +
  theme_bw(
    base_size = 12
  ) +
  theme(
    plot.title = element_text(
      face = "bold",
      hjust = 0.5
    ),
    legend.position = "right",
    legend.title = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank()
  )

fig2

ggsave(
  filename = file.path(
    questionnaire_out_dir,
    "Figure_threat_index_ECO3.png"
  ),
  plot = fig1,
  width = 7.5,
  height = 5.5,
  units = "in",
  dpi = 600,
  bg = "white"
)

ggsave(
  filename = file.path(
    questionnaire_out_dir,
    "Figure_threat_profile_ECO3.png"
  ),
  plot = fig2,
  width = 8.5,
  height = 6,
  units = "in",
  dpi = 600,
  bg = "white"
)

cat(
  "\nFigures exported successfully:\n",
  file.path(
    questionnaire_out_dir,
    "Figure_threat_index_ECO3.png"
  ),
  "\n",
  file.path(
    questionnaire_out_dir,
    "Figure_threat_profile_ECO3.png"
  ),
  "\n"
)

# 12. CONCORDANCIA ADMINISTRADOR VS INVESTIGADOR
#-------------------------------------------------------------------------------

agreement <- evaluator_index %>%
  filter(!is.na(evaluator)) %>%
  select(site_code, evaluator, threat_index_eval) %>%
  pivot_wider(
    names_from = evaluator,
    values_from = threat_index_eval
  ) %>%
  mutate(
    diff_admin_invest = Administrador - Investigador,
    abs_diff = abs(diff_admin_invest)
  ) %>%
  arrange(site_code)

print(agreement)

agreement_complete <- agreement %>%
  filter(
    !is.na(Administrador),
    !is.na(Investigador)
  )

pearson_agreement <- cor(
  agreement_complete$Administrador,
  agreement_complete$Investigador,
  method = "pearson"
)

spearman_agreement <- cor(
  agreement_complete$Administrador,
  agreement_complete$Investigador,
  method = "spearman"
)

mean_abs_diff <- mean(
  agreement_complete$abs_diff,
  na.rm = TRUE
)

n_complete_sites <- nrow(agreement_complete)

# Tabla resumen
agreement_summary <- tibble(
  Metric = c(
    "Pearson (r)",
    "Spearman (rho)",
    "Mean absolute difference",
    "Number of complete sites"
  ),
  Value = c(
    round(pearson_agreement, 3),
    round(spearman_agreement, 3),
    round(mean_abs_diff, 3),
    n_complete_sites
  )
)

print(agreement_summary)

cat("\n")
cat("=========================================\n")
cat("CONCORDANCIA ENTRE EVALUADORES\n")
cat("=========================================\n")
cat("Pearson (r): ", round(pearson_agreement, 3), "\n")
cat("Spearman (rho): ", round(spearman_agreement, 3), "\n")
cat("Diferencia absoluta media: ", round(mean_abs_diff, 3), "\n")
cat("Número de sitios completos: ", n_complete_sites, "\n")
cat("=========================================\n")

# 13. GRÁFICO DE CONCORDANCIA
#-------------------------------------------------------------------------------

fig3 <- ggplot(
  agreement_complete,
  aes(
    x = Administrador,
    y = Investigador,
    label = site_code
  )
) +
  geom_point(size = 3) +
  geom_text(nudge_y = 0.03, size = 3) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed"
  ) +
  coord_equal(
    xlim = c(-1.75, 1.75),
    ylim = c(-1.65, 1.65),
    clip = "off"
  ) +
  labs(
    x = "Índice de amenazas - Administrador local",
    y = "Índice de amenazas - Equipo de investigación",
    title = "Concordancia entre tipos de evaluador"
  ) +
  theme_minimal()

fig3

ggsave(
  file.path(
    questionnaire_out_dir,
    "Figure_evaluator_agreement_ECO3.png"
  ),
  fig3,
  width = 8,
  height = 6,
  dpi = 300
)

# 14. EXPORTAR TABLAS
#-------------------------------------------------------------------------------

# write.csv(
#   threat_scored,
#   file.path(desktop, "survey_threats_long_scored.csv"),
#   row.names = FALSE
# )
#
# write.csv(
#   interview_index,
#   file.path(desktop, "survey_index_by_interview.csv"),
#   row.names = FALSE
# )
#
# write.csv(
#   evaluator_index,
#   file.path(desktop, "survey_index_by_evaluator.csv"),
#   row.names = FALSE
# )
#
# write.csv(
#   site_index,
#   file.path(desktop, "survey_index_by_site.csv"),
#   row.names = FALSE
# )
#
# write.csv(
#   agreement,
#   file.path(desktop, "survey_agreement_admin_vs_researcher.csv"),
#   row.names = FALSE
# )

# FINAL QUESTIONNAIRE SUMMARY BY SITE
#-------------------------------------------------------------------------------

site_questionnaire_summary <- agreement %>%
  select(
    site_code,
    Administrador,
    Investigador
  ) %>%
  left_join(
    site_index %>%
      select(
        site_code,
        threat_index_site
      ),
    by = "site_code"
  ) %>%
  transmute(
    ID_site = str_to_upper(site_code),
    threat_admin = Administrador,
    threat_researcher = Investigador,
    threat_mean = threat_index_site
  ) %>%
  arrange(ID_site)

# Quality control
cat("\nFinal questionnaire summary by site:\n")
print(site_questionnaire_summary, row.names = FALSE)

if (nrow(site_questionnaire_summary) != 12) {
  warning(
    "Expected 12 sites, but found ",
    nrow(site_questionnaire_summary),
    "."
  )
}

if (any(is.na(site_questionnaire_summary$threat_mean))) {
  warning("At least one site has no final questionnaire index.")
}

# Export final questionnaire summary
questionnaire_output <- file.path(
  questionnaire_out_dir,
  "Questionnaire_summary_ECO3.csv"
)

write_csv(
  site_questionnaire_summary,
  questionnaire_output
)
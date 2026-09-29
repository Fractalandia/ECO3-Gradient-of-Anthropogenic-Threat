#===============================================================================
# BIO3 – INTEGRACIÓN FINAL DE RESULTADOS
#===============================================================================

library(tidyverse)
library(terra)
library(patchwork)
library(ggrepel)
library(scales)

# Archivos principales

master_path <- "C:/Users/Dell/Desktop/master_raw_FINAL.csv"

index_path <- "C:/Users/Dell/Desktop/revision_index/gAT_ECO3.tif"

quintile_path <- "C:/Users/Dell/Desktop/revision_index/gAT_quintiles_ECO3.tif"

loss_path <- "C:/Users/Dell/Desktop/revision_index/Loss_Nature_ECO3.tif"

roads_path <- "D:/Mapas_R/OUT/03_INTERMEDIATE/RoadDensity/Road_density_ECO3.tif"

ghm_path <- "C:/Users/Dell/Desktop/revision_index/GHM_ECO3.tif"

# datos
master_raw <- read_csv(
  master_path,
  show_col_types = FALSE
)

terraOptions(
  progress = 1,
  memfrac = 0.8
)

index     <- rast(index_path)
quintiles <- rast(quintile_path)
loss      <- rast(loss_path)
roads     <- rast(roads_path)
ghm       <- rast(ghm_path)

# 1. CARACTERIZACIÓN ESPACIAL DEL GRADIENTE DE ANTROPIZACIÓN
#-------------------------------------------------------------------------------
# 1.1 Capas espaciales
# Se usa la huella espacial del índice final para comparar el mismo territorio en las cuatro capas.

loss_map  <- mask(loss, index)
roads_map <- mask(roads, index)
ghm_map   <- mask(ghm, index)
index_map <- index

names(index_map) <- "Integrated index"
names(loss_map)  <- "Loss of nature"
names(roads_map) <- "Road density"
names(ghm_map)   <- "Human modification"

# 1.2 Correlaciones espaciales entre capas
#-------------------------------------------------------------------------------

map_stack <- c(
  index_map,
  loss_map,
  roads_map,
  ghm_map
)

map_values <- as.data.frame(
  map_stack,
  na.rm = TRUE
)

# Matriz de correlaciones de Pearson
spatial_correlations <- cor(
  map_values,
  method = "pearson",
  use = "complete.obs"
)

cat("\n--- Correlaciones espaciales entre capas ---\n")

print(
  round(
    spatial_correlations,
    3
  )
)

# Correlaciones + p-values
#-------------------------------------------------------------------------------
spatial_correlation_tests <- combn(
  names(map_values),
  2,
  simplify = FALSE
) %>%
  map_dfr(
    function(vars) {
      
      x <- map_values[[vars[1]]]
      y <- map_values[[vars[2]]]
      
      complete <- complete.cases(x, y)
      
      test <- cor.test(
        x[complete],
        y[complete],
        method = "pearson"
      )
      
      tibble(
        variable_1 = vars[1],
        variable_2 = vars[2],
        r = unname(test$estimate),
        p_value = test$p.value,
        n = sum(complete)
      )
    }
  ) %>%
  mutate(
    r = round(r, 3),
    p_value_formatted = format.pval(
      p_value,
      digits = 3,
      eps = 0.001
    )
  )


cat("\n--- Correlaciones espaciales de Pearson con p-values ---\n")

spatial_correlation_tests %>%
  select(
    variable_1,
    variable_2,
    r,
    p_value_formatted,
    n
  ) %>%
  print(
    n = Inf,
    width = Inf
  )

# 1.3 FIGURE 1 – SPATIAL COMPONENTS OF THE ANTHROPIZATION GRADIENT
#-------------------------------------------------------------------------------

raster_to_df <- function(x) {
  
  out <- as.data.frame(
    x,
    xy = TRUE,
    na.rm = TRUE
  )
  
  names(out)[3] <- "value"
  
  out
}


df_loss  <- raster_to_df(loss_map)
df_roads <- raster_to_df(roads_map)
df_ghm   <- raster_to_df(ghm_map)
df_index <- raster_to_df(index_map)

# Tema común
map_theme <- theme_void() +
  theme(
    
    plot.title = element_text(
      size = 11,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 3)
    ),
    
    legend.position = "bottom",
    
    legend.title = element_text(
      size = 8,
      face = "bold",
      hjust = 0.5
    ),
    
    legend.text = element_text(
      size = 7
    ),
    
    legend.box.spacing = unit(
      0.05,
      "cm"
    ),
    
    legend.key.width = unit(
      1.8,
      "cm"
    ),
    
    legend.key.height = unit(
      0.25,
      "cm"
    ),
    
    plot.margin = margin(
      2,
      2,
      2,
      2
    )
  )

# A. Loss of nature
p_loss <- ggplot(
  df_loss,
  aes(
    x = x,
    y = y,
    fill = value
  )
) +
  geom_raster() +
  coord_equal(
    expand = FALSE
  ) +
  scale_fill_viridis_c(
    option = "magma",
    direction = 1,
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    labels = c("0.0", "0.5", "1.0"),
    name = "",
    na.value = "transparent",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(2.0, "cm"),
      barheight = unit(0.32, "cm"),
      ticks = TRUE
    )
  ) +
  labs(
    title = "A. Loss of natural ecosystems"
  ) +
  map_theme

# B. Road density
p_roads <- ggplot(
  df_roads,
  aes(
    x = x,
    y = y,
    fill = value
  )
) +
  geom_raster() +
  coord_equal(
    expand = FALSE
  ) +
  scale_fill_gradientn(
    colours = c(
      "#FFFFCC",
      "#FED976",
      "#FD8D3C",
      "#E31A1C",
      "#800026"
    ),
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    labels = c("0.0", "0.5", "1.0"),
    name = "",
    na.value = "transparent",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(2.0, "cm"),
      barheight = unit(0.32, "cm"),
      ticks = TRUE
    )
  ) +
  labs(
    title = "B. Road density"
  ) +
  map_theme

# C. Human modification
p_ghm <- ggplot(
  df_ghm,
  aes(
    x = x,
    y = y,
    fill = value
  )
) +
  geom_raster() +
  coord_equal(
    expand = FALSE
  ) +
  scale_fill_gradientn(
    colours = c(
      "#264653",
      "#2A9D8F",
      "#E9C46A",
      "#F4A261",
      "#E76F51"
    ),
    limits = c(0, 1),
    breaks = c(0, 0.5, 1),
    labels = c("0.0", "0.5", "1.0"),
    name = "",
    na.value = "transparent",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(2.0, "cm"),
      barheight = unit(0.32, "cm"),
      ticks = TRUE
    )
  ) +
  labs(
    title = "C. Global Human modification"
  ) +
  map_theme

# D. Integrated anthropization index
p_index <- ggplot(
  df_index,
  aes(
    x = x,
    y = y,
    fill = value
  )
) +
  geom_raster() +
  coord_equal(
    expand = FALSE
  ) +
  scale_fill_gradientn(
    colours = c(
      "#2B83BA",
      "#ABDDA4",
      "#FFFFBF",
      "#FDAE61",
      "#D7191C"
    ),
    limits = c(0, 0.779),
    breaks = c(0, 0.2, 0.4, 0.6, 0.8),
    labels = c("0.0", "0.2", "0.4", "0.6", "0.8"),
    name = "",
    na.value = "transparent",
    guide = guide_colorbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = unit(2.0, "cm"),
      barheight = unit(0.32, "cm"),
      ticks = TRUE
    )
  ) +
  labs(
    title = "D. Gradient of anthropogenic threats"
  ) +
  map_theme

# Combinar paneles
figure_1 <- wrap_plots(
  p_loss,
  p_roads,
  p_ghm,
  p_index,
  ncol = 4
) +
  plot_layout(
    widths = c(1, 1, 1, 1)
  ) &
  theme(
    plot.margin = margin(
      t = 2,
      r = -8,
      b = 2,
      l = -8
    )
  )

figure_1

ggsave(
  filename = "C:/Users/Dell/Desktop/Figure_spatial_components_ECO3.png",
  plot = figure_1,
  width = 10.5,
  height = 6.2,
  dpi = 600,
  bg = "white"
)

# 2. REPRESENTATIVIDAD DE LOS SITIOS EN EL GRADIENTE DE ANTROPIZACIÓN
#-------------------------------------------------------------------------------
# 2.1 Valores regionales
# Cada celda se asigna a uno de los cinco quintiles oficiales del índice.

names(index)     <- "SIG"
names(quintiles) <- "SIG_quintile"
names(loss)      <- "loss"
names(roads)     <- "roads"
names(ghm)       <- "ghm"

# Usar directamente el raster oficial de quintiles del gAT.
quintiles_map <- mask(quintiles, index)

regional_stack <- c(
  index,
  quintiles_map,
  mask(loss, index),
  mask(roads, index),
  mask(ghm, index)
)

regional_values <- as.data.frame(
  regional_stack,
  na.rm = TRUE
) %>%
  mutate(
    SIG_quintile = factor(
      paste0("Q", as.integer(SIG_quintile)),
      levels = paste0("Q", 1:5)
    )
  ) %>%
  filter(!is.na(SIG_quintile))

# Límites observados de cada clase, derivados del raster oficial de quintiles.
quintile_limits <- regional_values %>%
  group_by(SIG_quintile) %>%
  summarise(
    SIG_lower = min(SIG, na.rm = TRUE),
    SIG_upper = max(SIG, na.rm = TRUE),
    .groups = "drop"
  )

# 2.2 Resumen regional por quintil
quintile_regional_summary <- regional_values %>%
  group_by(
    SIG_quintile
  ) %>%
  summarise(
    n_cells = n(),
    
    SIG_regional_mean = mean(SIG),
    SIG_regional_median = median(SIG),
    SIG_regional_sd = sd(SIG),
    SIG_regional_q25 = quantile(SIG, 0.25),
    SIG_regional_q75 = quantile(SIG, 0.75),
    
    loss_regional_mean = mean(loss),
    loss_regional_sd = sd(loss),
    
    roads_regional_mean = mean(roads),
    roads_regional_sd = sd(roads),
    
    ghm_regional_mean = mean(ghm),
    ghm_regional_sd = sd(ghm),
    
    .groups = "drop"
  ) %>%
  left_join(
    quintile_limits,
    by = "SIG_quintile"
  )

cat("\n--- Resumen regional por quintil ---\n")

print(
  quintile_regional_summary,
  n = Inf,
  width = Inf
)

# 2.3 Percentil regional de cada sitio
calculate_regional_percentile <- function(
    site_SIG,
    site_quintile,
    regional_data
) {
  
  regional_subset <- regional_data %>%
    filter(
      SIG_quintile == site_quintile
    ) %>%
    pull(SIG)
  
  as.numeric(
    ecdf(regional_subset)(
      site_SIG
    )
  )
}

# 2.4 Representatividad de los candidatos
site_representativeness <- master_raw %>%
  select(
    ID_site,
    SIG_quintile,
    SIG_mean,
    loss_mean,
    roads_mean,
    ghm_mean
  ) %>%
  
  mutate(
    SIG_quintile = factor(
      SIG_quintile,
      levels = paste0("Q", 1:5)
    )
  ) %>%
  
  left_join(
    quintile_regional_summary,
    by = "SIG_quintile"
  ) %>%
  
  rowwise() %>%
  
  mutate(
    
    SIG_regional_percentile =
      calculate_regional_percentile(
        site_SIG = SIG_mean,
        site_quintile = SIG_quintile,
        regional_data = regional_values
      ),
    
    # 1 = centro de la distribución regional; 0 = extremos
    SIG_typicality =
      1 - 2 * abs(
        SIG_regional_percentile - 0.5
      ),
    
    SIG_standardized_deviation =
      abs(
        SIG_mean - SIG_regional_median
      ) /
      SIG_regional_sd,
    
    # Distancia estandarizada al perfil medio de los tres componentes
    loss_z_within_quintile =
      (
        loss_mean -
          loss_regional_mean
      ) /
      loss_regional_sd,
    
    roads_z_within_quintile =
      (
        roads_mean -
          roads_regional_mean
      ) /
      roads_regional_sd,
    
    ghm_z_within_quintile =
      (
        ghm_mean -
          ghm_regional_mean
      ) /
      ghm_regional_sd,
    
    component_distance = sqrt(
      mean(
        c(
          loss_z_within_quintile^2,
          roads_z_within_quintile^2,
          ghm_z_within_quintile^2
        )
      )
    )
  ) %>%
  
  ungroup() %>%
  
  group_by(
    SIG_quintile
  ) %>%
  
  mutate(
    rank_SIG_typicality = min_rank(
      desc(SIG_typicality)
    ),
    
    rank_components = min_rank(
      component_distance
    )
  ) %>%
  
  ungroup() %>%
  
  arrange(
    SIG_quintile,
    rank_SIG_typicality
  )

cat("\n--- Representatividad de los candidatos ---\n")

site_representativeness %>%
  select(
    ID_site,
    SIG_quintile,
    SIG_mean,
    SIG_regional_percentile,
    SIG_typicality,
    component_distance,
    rank_SIG_typicality,
    rank_components
  ) %>%
  print(
    n = Inf,
    width = Inf
  )

# 2.5 FIGURE – POSITION OF CANDIDATE SITES ALONG THE GRADIENT
plot_quintile_regions <- quintile_regional_summary %>%
  transmute(
    SIG_quintile,
    quintile_y = as.numeric(
      SIG_quintile
    ),
    SIG_lower,
    SIG_upper,
    regional_median = SIG_regional_median,
    regional_q25 = SIG_regional_q25,
    regional_q75 = SIG_regional_q75
  )

# Los sitios se desplazan ligeramente en el eje vertical para evitar solaparlos.
# El desplazamiento es solamente gráfico.

site_plot_data <- site_representativeness %>%
  group_by(
    SIG_quintile
  ) %>%
  arrange(
    SIG_mean,
    .by_group = TRUE
  ) %>%
  mutate(
    site_y =
      as.numeric(SIG_quintile) +
      seq(
        from = -0.16,
        to = 0.16,
        length.out = n()
      )
  ) %>%
  ungroup()

figure_SIG_position <- ggplot() +
  
  # Rango oficial de cada quintil
  
  geom_rect(
    data = plot_quintile_regions,
    aes(
      xmin = SIG_lower,
      xmax = SIG_upper,
      ymin = 0.55,
      ymax = 5.45,
      fill = SIG_quintile
    ),
    alpha = 0.10,
    colour = NA
  ) +
  
  geom_hline(
    yintercept = 1:5,
    colour = "grey90",
    linewidth = 0.45
  ) +
  
  # Rango intercuartílico regional
  
  geom_segment(
    data = plot_quintile_regions,
    aes(
      x = regional_q25,
      xend = regional_q75,
      y = quintile_y,
      yend = quintile_y
    ),
    linewidth = 8,
    colour = "grey75",
    lineend = "round"
  ) +
  
  # Mediana regional
  
  geom_point(
    data = plot_quintile_regions,
    aes(
      x = regional_median,
      y = quintile_y
    ),
    shape = 21,
    size = 5,
    fill = "white",
    colour = "grey30",
    stroke = 1.2
  ) +
  
  # Candidatos
  
  geom_segment(
    data = site_plot_data,
    aes(
      x = SIG_mean,
      xend = SIG_mean,
      y = as.numeric(SIG_quintile),
      yend = site_y
    ),
    colour = "grey55",
    linewidth = 0.45
  ) +
  
  geom_point(
    data = site_plot_data,
    aes(
      x = SIG_mean,
      y = site_y,
      fill = SIG_quintile
    ),
    shape = 21,
    size = 4,
    colour = "black",
    stroke = 0.7
  ) +
  
  geom_text_repel(
    data = site_plot_data,
    aes(
      x = SIG_mean,
      y = site_y,
      label = ID_site
    ),
    seed = 123,
    size = 3.8,
    fontface = "bold",
    box.padding = 0.35,
    point.padding = 0.25,
    min.segment.length = 0,
    segment.colour = "grey50",
    segment.linewidth = 0.35,
    max.overlaps = Inf
  ) +
  
  scale_y_continuous(
    breaks = 1:5,
    labels = paste0("Q", 1:5),
    limits = c(0.55, 5.45),
    expand = expansion(
      mult = c(
        0.01,
        0.01
      )
    )
  ) +
  
  scale_x_continuous(
    limits = c(0, 0.779),
    breaks = seq(
      0,
      0.8,
      0.1
    ),
    expand = expansion(
      mult = c(
        0.01,
        0.03
      )
    )
  ) +
  
  scale_fill_brewer(
    palette = "RdYlBu",
    direction = -1,
    guide = "none"
  ) +
  
  labs(
    title = "Position of candidate sites along the anthropization gradient",
    x = "Continuous anthropization index",
    y = "Official anthropization quintile"
  ) +
  
  theme_classic(
    base_size = 12
  ) +
  
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    axis.text = element_text(
      colour = "grey25"
    ),
    
    plot.margin = margin(
      t = 12,
      r = 30,
      b = 12,
      l = 12
    )
  ) +
  
  coord_cartesian(
    clip = "off"
  )

figure_SIG_position

ggsave(
  filename = "C:/Users/Dell/Desktop/Figure_candidate_site_positions_ECO3.png",
  plot = figure_SIG_position,
  width = 10,
  height = 7.5,
  dpi = 600,
  bg = "white"
)

# 3. CONCORDANCIA ENTRE SIG, AMENAZAS Y BIODIVERSIDAD NATIVA
#-------------------------------------------------------------------------------
# Las tres variables se comparan mediante percentiles empíricos entre los
# 12 sitios candidatos.

# 3.1 SIG vs amenazas
sig_threat_validation <- master_raw %>%
  transmute(
    ID_site,
    
    SIG_quintile = factor(
      SIG_quintile,
      levels = paste0("Q", 1:5)
    ),
    
    SIG_mean,
    threat_mean,
    
    SIG_percentile = percent_rank(
      SIG_mean
    ),
    
    threat_percentile = percent_rank(
      threat_mean
    ),
    
    expected_threat_percentile =
      SIG_percentile,
    
    discordance = abs(
      threat_percentile -
        expected_threat_percentile
    ),
    
    concordance =
      1 - discordance
  ) %>%
  arrange(
    desc(concordance),
    ID_site
  )

cat("\n--- Concordancia SIG vs amenazas ---\n")

sig_threat_validation %>%
  select(
    ID_site,
    SIG_quintile,
    SIG_percentile,
    threat_percentile,
    discordance,
    concordance
  ) %>%
  print(
    n = Inf,
    width = Inf
  )

# Figura A
p_validation_threat <- ggplot(
  sig_threat_validation,
  aes(
    x = SIG_percentile,
    y = threat_percentile
  )
) +
  
  geom_abline(
    intercept = 0,
    slope = 1,
    linetype = 2,
    colour = "grey50",
    linewidth = 0.75
  ) +
  
  geom_segment(
    aes(
      xend = SIG_percentile,
      yend = expected_threat_percentile
    ),
    colour = "grey82",
    linewidth = 0.65
  ) +
  
  geom_point(
    aes(
      colour = SIG_quintile
    ),
    size = 4,
    alpha = 0.95
  ) +
  
  geom_text_repel(
    aes(
      label = ID_site
    ),
    size = 3.5,
    fontface = "bold",
    seed = 123,
    force = 1.2,
    box.padding = 0.35,
    point.padding = 0.25,
    min.segment.length = 0,
    segment.colour = "grey55",
    segment.linewidth = 0.35,
    max.overlaps = Inf
  ) +
  
  scale_x_continuous(
    limits = c(-0.03, 1.03),
    breaks = seq(0, 1, 0.2),
    labels = label_percent(
      accuracy = 1
    ),
    expand = expansion(
      mult = 0
    )
  ) +
  
  scale_y_continuous(
    limits = c(-0.03, 1.03),
    breaks = seq(0, 1, 0.2),
    labels = label_percent(
      accuracy = 1
    ),
    expand = expansion(
      mult = 0
    )
  ) +
  
  scale_colour_brewer(
    palette = "RdYlBu",
    direction = -1,
    name = "Anthropization\nquintile"
  ) +
  
  labs(
    title = "Concordance between anthropization and threats",
    subtitle = "Empirical percentile ranks: (rank − 1) / (n − 1)",
    x = "Anthropization percentile",
    y = "Threat percentile"
  ) +
  
  coord_equal(
    clip = "off"
  ) +
  
  theme_classic(
    base_size = 12
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      colour = "grey35",
      margin = margin(
        b = 10
      )
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    plot.margin = margin(
      t = 12,
      r = 25,
      b = 12,
      l = 12
    )
  )

# 3.2 SIG vs biodiversidad nativa
sig_native_validation <- master_raw %>%
  transmute(
    ID_site,
    
    SIG_quintile = factor(
      SIG_quintile,
      levels = paste0("Q", 1:5)
    ),
    
    SIG_mean,
    qD_q1_native,
    
    SIG_percentile = percent_rank(
      SIG_mean
    ),
    
    native_diversity_percentile = percent_rank(
      qD_q1_native
    ),
    
    expected_native_diversity_percentile =
      1 - SIG_percentile,
    
    discordance = abs(
      native_diversity_percentile -
        expected_native_diversity_percentile
    ),
    
    concordance =
      1 - discordance
  ) %>%
  arrange(
    desc(concordance),
    ID_site
  )

cat("\n--- Concordancia SIG vs biodiversidad nativa ---\n")

sig_native_validation %>%
  select(
    ID_site,
    SIG_quintile,
    SIG_percentile,
    native_diversity_percentile,
    discordance,
    concordance
  ) %>%
  print(
    n = Inf,
    width = Inf
  )

# Figura B
p_validation_native <- ggplot(
  sig_native_validation,
  aes(
    x = SIG_percentile,
    y = native_diversity_percentile
  )
) +
  
  geom_abline(
    intercept = 1,
    slope = -1,
    linetype = 2,
    colour = "grey50",
    linewidth = 0.75
  ) +
  
  geom_segment(
    aes(
      xend = SIG_percentile,
      yend = expected_native_diversity_percentile
    ),
    colour = "grey82",
    linewidth = 0.65
  ) +
  
  geom_point(
    aes(
      colour = SIG_quintile
    ),
    size = 4,
    alpha = 0.95
  ) +
  
  geom_text_repel(
    aes(
      label = ID_site
    ),
    size = 3.5,
    fontface = "bold",
    seed = 123,
    force = 1.2,
    box.padding = 0.35,
    point.padding = 0.25,
    min.segment.length = 0,
    segment.colour = "grey55",
    segment.linewidth = 0.35,
    max.overlaps = Inf
  ) +
  
  scale_x_continuous(
    limits = c(-0.03, 1.03),
    breaks = seq(0, 1, 0.2),
    labels = label_percent(
      accuracy = 1
    ),
    expand = expansion(
      mult = 0
    )
  ) +
  
  scale_y_continuous(
    limits = c(-0.03, 1.03),
    breaks = seq(0, 1, 0.2),
    labels = label_percent(
      accuracy = 1
    ),
    expand = expansion(
      mult = 0
    )
  ) +
  
  scale_colour_brewer(
    palette = "RdYlBu",
    direction = -1,
    name = "Anthropization\nquintile"
  ) +
  
  labs(
    title = "Concordance between anthropization and native diversity",
    subtitle = "Empirical percentile ranks: (rank − 1) / (n − 1)",
    x = "Anthropization percentile",
    y = "Native Shannon diversity percentile"
  ) +
  
  coord_equal(
    clip = "off"
  ) +
  
  theme_classic(
    base_size = 12
  ) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      colour = "grey35",
      margin = margin(
        b = 10
      )
    ),
    
    axis.title = element_text(
      face = "bold"
    ),
    
    legend.position = "right",
    
    legend.title = element_text(
      face = "bold"
    ),
    
    plot.margin = margin(
      t = 12,
      r = 25,
      b = 12,
      l = 12
    )
  )

# 3.3 Figura combinada
combined_concordance_figure <-
  p_validation_threat +
  p_validation_native +
  plot_layout(
    ncol = 2,
    widths = c(1, 1),
    guides = "collect"
  ) +
  plot_annotation(
    tag_levels = "A"
  ) &
  theme(
    legend.position = "right"
  )

combined_concordance_figure

ggsave(
  filename = paste0(
    "C:/Users/Dell/Desktop/",
    "Figure_SIG_threat_native_concordance_ECO3.png"
  ),
  plot = combined_concordance_figure,
  width = 15.5,
  height = 7.2,
  dpi = 600,
  bg = "white"
)

# 4. Correlation Network- RELACIONES ENTRE LOS COMPONENTES DE SELECCIÓN
#-------------------------------------------------------------------------------
# Se utilizan correlaciones de Spearman entre los componentes SIG,
# biodiversidad y encuestas.
#
# La red muestra asociaciones con |rho| >= 0.25.
# Los valores numéricos se muestran solo para |rho| >= 0.70.

# 4.1 Preparar los datos
network_data <- master_raw %>%
  select(
    ID_site,
    
    # GIS components
    SIG_mean,
    roads_mean,
    ghm_mean,
    loss_mean,
    
    # Biodiversity
    qD_q1_native,
    qD_q1_exotic,
    native_individuals_pct,
    
    # Questionnaire
    threat_admin,
    threat_mean,
    threat_researcher
  )

# 4.2 Correlaciones de Spearman
network_cor <- network_data %>%
  select(
    -ID_site
  ) %>%
  cor(
    method = "spearman",
    use = "complete.obs"
  )

cat("\n--- Correlaciones de Spearman ---\n")

print(
  round(
    network_cor,
    3
  )
)

# Convertir la matriz en pares únicos

network_edges <- as.data.frame(
  as.table(
    network_cor
  )
) %>%
  
  rename(
    from = Var1,
    to = Var2,
    rho = Freq
  ) %>%
  
  mutate(
    from = as.character(from),
    to = as.character(to)
  ) %>%
  
  filter(
    from != to
  ) %>%
  
  rowwise() %>%
  
  mutate(
    pair_id = paste(
      sort(
        c(
          from,
          to
        )
      ),
      collapse = " | "
    )
  ) %>%
  
  ungroup() %>%
  
  distinct(
    pair_id,
    .keep_all = TRUE
  )

# 4.3 Filtrar las relaciones mostradas
correlation_threshold <- 0.25

network_edges_plot <- network_edges %>%
  
  filter(
    abs(rho) >= correlation_threshold
  ) %>%
  
  mutate(
    sign = if_else(
      rho >= 0,
      "Positive",
      "Negative"
    ),
    
    strength = abs(rho),
    
    dominant_label = if_else(
      abs(rho) >= 0.70,
      sprintf(
        "%.2f",
        rho
      ),
      NA_character_
    )
  )

cat("\n--- Correlaciones incluidas en la red ---\n")

network_edges_plot %>%
  arrange(
    desc(
      abs(rho)
    )
  ) %>%
  select(
    from,
    to,
    rho
  ) %>%
  print(
    n = Inf,
    width = Inf
  )

# 4.4 Nodos y etiquetas
network_nodes <- tibble(
  
  name = c(
    
    # GIS components
    "SIG_mean",
    "roads_mean",
    "ghm_mean",
    "loss_mean",
    
    # Biodiversity
    "qD_q1_native",
    "qD_q1_exotic",
    "native_individuals_pct",
    
    # Questionnaire
    "threat_admin",
    "threat_mean",
    "threat_researcher"
  ),
  
  label = c(
    
    # GIS components
    "Integrated anthropization index",
    "Road density",
    "Global Human Modification",
    "Loss of nature",
    
    # Biodiversity
    "Native Shannon diversity",
    "Exotic Shannon diversity",
    "Native individuals (%)",
    
    # Questionnaire
    "Administrator",
    "Mean threat",
    "Researcher"
  ),
  
  block = c(
    rep(
      "GIS components",
      4
    ),
    
    rep(
      "Biodiversity",
      3
    ),
    
    rep(
      "Questionnaire",
      3
    )
  )
)

# 4.5 Posición de los nodos
network_nodes_plot <- network_nodes %>%
  mutate(
    
    angle_deg = case_when(
      
      # GIS components
      name == "SIG_mean"   ~ 82,
      name == "roads_mean" ~ 48,
      name == "ghm_mean"   ~ 16,
      name == "loss_mean"  ~ -16,
      
      # Biodiversity
      name == "qD_q1_native"           ~ -55,
      name == "qD_q1_exotic"           ~ -90,
      name == "native_individuals_pct" ~ -125,
      
      # Questionnaire
      name == "threat_admin"      ~ -160,
      name == "threat_mean"       ~ 165,
      name == "threat_researcher" ~ 130
    ),
    
    angle =
      angle_deg *
      pi /
      180,
    
    x = cos(angle),
    y = sin(angle),
    
    label_radius = 1.20,
    
    label_x =
      label_radius *
      cos(angle),
    
    label_y =
      label_radius *
      sin(angle),
    
    label_x = if_else(
      name == "SIG_mean",
      0,
      label_x
    ),
    
    label_y = if_else(
      name == "SIG_mean",
      y + 0.16,
      label_y
    ),
    
    label_hjust = case_when(
      name == "SIG_mean" ~ 0.5,
      label_x > 0.15 ~ 0,
      label_x < -0.15 ~ 1,
      TRUE ~ 0.5
    )
  )

# 4.6 Incorporar las coordenadas a las relaciones
network_edges_plot <- network_edges_plot %>%
  
  left_join(
    network_nodes_plot %>%
      select(
        name,
        x,
        y
      ),
    by = c(
      "from" = "name"
    )
  ) %>%
  
  rename(
    x_from = x,
    y_from = y
  ) %>%
  
  left_join(
    network_nodes_plot %>%
      select(
        name,
        x,
        y
      ),
    by = c(
      "to" = "name"
    )
  ) %>%
  
  rename(
    x_to = x,
    y_to = y
  )

# 4.7 Posición de las etiquetas de correlación
network_edges_plot <- network_edges_plot %>%
  mutate(
    
    midpoint_x =
      (
        x_from +
          x_to
      ) /
      2,
    
    midpoint_y =
      (
        y_from +
          y_to
      ) /
      2,
    
    dx =
      x_to -
      x_from,
    
    dy =
      y_to -
      y_from,
    
    edge_length = sqrt(
      dx^2 +
        dy^2
    ),
    
    perpendicular_x =
      -dy /
      edge_length,
    
    perpendicular_y =
      dx /
      edge_length,
    
    label_direction = rep(
      c(
        -1,
        1
      ),
      length.out = n()
    ),
    
    label_offset = 0.055,
    
    label_x =
      midpoint_x +
      perpendicular_x *
      label_offset *
      label_direction,
    
    label_y =
      midpoint_y +
      perpendicular_y *
      label_offset *
      label_direction
  )

# 4.8 Paleta
block_colors <- c(
  "GIS components" = "#78BCE8",
  "Biodiversity"   = "#A8DDB5",
  "Questionnaire"  = "#F4A261"
)

edge_colors <- c(
  "Positive" = "#5796C7",
  "Negative" = "#D6819B"
)

# 4.9 FIGURE – CORRELATION NETWORK
#-------------------------------------------------------------------------------

figure_correlation_network <- ggplot() +
  
  # Círculo de referencia
  
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
  
  # Relaciones
  
  geom_segment(
    data = network_edges_plot,
    
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
  
  # Valores de las correlaciones fuertes
  
  geom_label(
    data = network_edges_plot %>%
      filter(
        !is.na(
          dominant_label
        )
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
      0.94
    ),
    
    label.size = 0,
    
    label.padding = unit(
      0.08,
      "lines"
    )
  ) +
  
  # Nodos
  
  geom_point(
    data = network_nodes_plot,
    
    aes(
      x = x,
      y = y,
      fill = block
    ),
    
    shape = 21,
    size = 12,
    colour = "white",
    stroke = 1.4
  ) +
  
  # Etiquetas
  
  geom_text(
    data = network_nodes_plot,
    
    aes(
      x = label_x,
      y = label_y,
      label = label,
      hjust = label_hjust
    ),
    
    size = 3.7,
    fontface = "bold"
  ) +
  
  scale_fill_manual(
    values = block_colors,
    name = "Component"
  ) +
  
  scale_colour_manual(
    values = edge_colors,
    guide = "none"
  ) +
  
  scale_linewidth_continuous(
    range = c(
      0.6,
      4.2
    ),
    guide = "none"
  ) +
  
  coord_equal(
    xlim = c(
      -1.58,
      1.58
    ),
    ylim = c(
      -1.50,
      1.50
    ),
    clip = "off"
  ) +
  
  labs(
    title = "Relationships among site-selection components",
    
    subtitle = paste0(
      "Spearman correlations; only |ρ| ≥ ",
      correlation_threshold,
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
      hjust = 0.5
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
      r = 65,
      b = 15,
      l = 65
    )
  )


figure_correlation_network

ggsave(
  filename = "C:/Users/Dell/Desktop/Figure_correlation_network_ECO3.png",
  plot = figure_correlation_network,
  width = 9,
  height = 8,
  dpi = 600,
  bg = "white"
)

# 5. OPTIMIZACIÓN MULTIVARIADA DE LAS COMBINACIONES DE SITIOS
#-------------------------------------------------------------------------------
#
# Se evalúan las 48 combinaciones posibles con un sitio por quintil.
#
# Los tres bloques tienen el mismo peso:
#
#   SIG                    = 1/3
#   Amenazas               = 1/3
#   Condición biológica    = 1/3
#
# El bloque biológico combina:
#
#   Shannon nativa         = 1/6
#   Individuos nativos (%) = 1/6

# 5.1 Datos para la selección
selection_data <- master_raw %>%
  transmute(
    ID_site,
    
    SIG_quintile = factor(
      SIG_quintile,
      levels = paste0("Q", 1:5)
    ),
    
    SIG_mean,
    threat_mean,
    qD_q1_native,
    native_individuals_pct
  ) %>%
  
  arrange(
    SIG_quintile,
    SIG_mean
  )

# 5.2 Generar las 48 combinaciones
sites_by_quintile <- split(
  selection_data$ID_site,
  selection_data$SIG_quintile
)

site_combinations <- expand.grid(
  sites_by_quintile,
  stringsAsFactors = FALSE
)

names(site_combinations) <- paste0(
  "site_q",
  seq_len(
    ncol(site_combinations)
  )
)

site_combinations <- site_combinations %>%
  mutate(
    combination_id = row_number(),
    .before = 1
  )

cat(
  "\nNúmero de combinaciones evaluadas:",
  nrow(site_combinations),
  "\n"
)

# 5.3 Función para resumir las distancias
# Cinco sitios generan 10 distancias únicas.

calculate_distance_metrics <- function(
    selected_sites,
    distance_matrix
) {
  
  distance_submatrix <- distance_matrix[
    selected_sites,
    selected_sites,
    drop = FALSE
  ]
  
  pairwise_distances <- distance_submatrix[
    upper.tri(distance_submatrix)
  ]
  
  tibble(
    mean_distance = mean(
      pairwise_distances
    ),
    
    min_distance = min(
      pairwise_distances
    )
  )
}

# 5.4 Estandarizar las variables
selection_scaled <- selection_data %>%
  mutate(
    across(
      c(
        SIG_mean,
        threat_mean,
        qD_q1_native,
        native_individuals_pct
      ),
      ~ as.numeric(
        scale(.x)
      )
    )
  )

# 5.5 Aplicar los pesos por bloque
# Multiplicar cada variable por sqrt(peso) permite calcular directamente
# la distancia euclidiana ponderada.

selection_weighted <- selection_scaled %>%
  mutate(
    
    SIG_weighted =
      SIG_mean * sqrt(1 / 3),
    
    threat_weighted =
      threat_mean * sqrt(1 / 3),
    
    native_diversity_weighted =
      qD_q1_native * sqrt(1 / 6),
    
    native_pct_weighted =
      native_individuals_pct * sqrt(1 / 6)
  )

# 5.6 Matriz de distancias
distance_matrix <- selection_weighted %>%
  select(
    ID_site,
    SIG_weighted,
    threat_weighted,
    native_diversity_weighted,
    native_pct_weighted
  ) %>%
  column_to_rownames(
    "ID_site"
  ) %>%
  dist(
    method = "euclidean"
  ) %>%
  as.matrix()

# 5.7 Evaluar todas las combinaciones
multivariate_results <- site_combinations %>%
  
  rowwise() %>%
  
  mutate(
    metrics = list(
      calculate_distance_metrics(
        selected_sites = c_across(
          starts_with("site_q")
        ),
        distance_matrix = distance_matrix
      )
    )
  ) %>%
  
  ungroup() %>%
  
  unnest(
    metrics
  ) %>%
  
  mutate(
    
    rank_mean = min_rank(
      desc(mean_distance)
    ),
    
    rank_min = min_rank(
      desc(min_distance)
    ),
    
    rank_sum =
      rank_mean +
      rank_min
  ) %>%
  
  arrange(
    rank_sum,
    desc(mean_distance),
    desc(min_distance)
  )

# 5.8 Mejores combinaciones
top3_multivariate <- multivariate_results %>%
  slice_head(
    n = 3
  ) %>%
  mutate(
    option = as.character(
      row_number()
    )
  )

cat("\n--- Mejores combinaciones multivariadas ---\n")

top3_multivariate %>%
  select(
    option,
    site_q1,
    site_q2,
    site_q3,
    site_q4,
    site_q5,
    mean_distance,
    min_distance,
    rank_mean,
    rank_min,
    rank_sum
  ) %>%
  print(
    n = Inf,
    width = Inf
  )

# 5.9 FIGURE – MULTIVARIATE DIFFERENTIATION
#-------------------------------------------------------------------------------
# Etiqueta de las tres combinaciones obtenidas en el análisis actual

top3_label <- paste0(
  "Combinations\n",
  paste(
    paste0(
      top3_multivariate$option,
      "   ",
      top3_multivariate$site_q1,
      " · ",
      top3_multivariate$site_q2,
      " · ",
      top3_multivariate$site_q3,
      " · ",
      top3_multivariate$site_q4,
      " · ",
      top3_multivariate$site_q5
    ),
    collapse = "\n"
  )
)

# Medianas utilizadas como referencia visual

mean_distance_cut <- median(
  multivariate_results$mean_distance
)

min_distance_cut <- median(
  multivariate_results$min_distance
)

multivariate_plot_data <- multivariate_results %>%
  
  left_join(
    top3_multivariate %>%
      select(
        combination_id,
        option
      ),
    by = "combination_id"
  ) %>%
  
  mutate(
    highlighted = if_else(
      is.na(option),
      "Other combinations",
      "Top three combinations"
    )
  )

figure_multivariate_selection <- ggplot(
  multivariate_plot_data,
  aes(
    x = mean_distance,
    y = min_distance
  )
) +
  
  # Cuadrante superior derecho
  
  annotate(
    "rect",
    xmin = mean_distance_cut,
    xmax = Inf,
    ymin = min_distance_cut,
    ymax = Inf,
    fill = "grey90",
    alpha = 0.45
  ) +
  
  geom_vline(
    xintercept = mean_distance_cut,
    linetype = "dashed",
    colour = "grey45",
    linewidth = 0.65
  ) +
  
  geom_hline(
    yintercept = min_distance_cut,
    linetype = "dashed",
    colour = "grey45",
    linewidth = 0.65
  ) +
  
  # Otras combinaciones
  
  geom_point(
    data = multivariate_plot_data %>%
      filter(
        highlighted == "Other combinations"
      ),
    colour = "grey70",
    size = 2.8,
    alpha = 0.80
  ) +
  
  # Tres mejores combinaciones
  
  geom_point(
    data = multivariate_plot_data %>%
      filter(
        highlighted == "Top three combinations"
      ),
    shape = 21,
    size = 5.5,
    stroke = 1.1,
    fill = "white",
    colour = "#2C7FB8"
  ) +
  
  geom_point(
    data = multivariate_plot_data %>%
      filter(
        highlighted == "Top three combinations"
      ),
    size = 2.8,
    colour = "#2C7FB8"
  ) +
  
  geom_label_repel(
    data = multivariate_plot_data %>%
      filter(
        highlighted == "Top three combinations"
      ),
    aes(
      label = option
    ),
    seed = 123,
    size = 4,
    fontface = "bold",
    colour = "#2C7FB8",
    fill = "white",
    label.size = 0,
    box.padding = 0.45,
    point.padding = 0.35,
    min.segment.length = 0,
    segment.colour = "#2C7FB8",
    segment.linewidth = 0.4,
    max.overlaps = Inf
  ) +
  
  # Identificación de las tres combinaciones
  
  annotate(
    "text",
    x = Inf,
    y = min_distance_cut + 0.03,
    label = top3_label,
    hjust = 1.05,
    vjust = 0,
    size = 3.5,
    lineheight = 1.25,
    colour = "grey30"
  ) +
  
  labs(
    title = "Multivariate differentiation among candidate site combinations",
    
    x = "Mean pairwise distance",
    y = "Minimum pairwise distance"
  ) +
  
  scale_x_continuous(
    expand = expansion(
      mult = c(
        0.04,
        0.10
      )
    )
  ) +
  
  scale_y_continuous(
    expand = expansion(
      mult = c(
        0.05,
        0.12
      )
    )
  ) +
  
  theme_classic(
    base_size = 12
  ) +
  
  theme(
    
    axis.title = element_text(
      face = "bold",
      size = 12
    ),
    
    axis.text = element_text(
      colour = "grey30"
    ),
    
    axis.line = element_line(
      colour = "grey35",
      linewidth = 0.45
    ),
    
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    
    plot.subtitle = element_text(
      size = 10,
      colour = "grey38",
      margin = margin(
        b = 10
      )
    ),
    
    plot.margin = margin(
      t = 12,
      r = 25,
      b = 12,
      l = 12
    )
  )

figure_multivariate_selection

ggsave(
  filename = paste0(
    "C:/Users/Dell/Desktop/",
    "Figure_multivariate_site_selection_ECO3.png"
  ),
  plot = figure_multivariate_selection,
  width = 8.5,
  height = 6.5,
  dpi = 600,
  bg = "white"
)

# 6. Versión pasadas/Otros- ANÁLISIS DE SENSIBILIDAD
#-------------------------------------------------------------------------------
#
# Estos análisis se conservaron como apoyo metodológico. No forman parte del
# modelo principal de selección, pero permiten comprobar cómo cambian los
# resultados bajo distintas representaciones de biodiversidad y del gradiente.

# 6.1 CORRELACIONES EXPLORATORIAS: PEARSON VS SPEARMAN

correlation_sensitivity_data <- master_raw %>%
  select(
    ID_site,
    SIG_mean,
    qD_q0,
    qD_q1,
    qD_q2,
    threat_mean
  )

cor_pearson <- correlation_sensitivity_data %>%
  select(
    -ID_site
  ) %>%
  cor(
    method = "pearson",
    use = "pairwise.complete.obs"
  )

cor_spearman <- correlation_sensitivity_data %>%
  select(
    -ID_site
  ) %>%
  cor(
    method = "spearman",
    use = "pairwise.complete.obs"
  )

cat("\n--- Pearson correlations ---\n")
print(
  round(
    cor_pearson,
    3
  )
)

cat("\n--- Spearman correlations ---\n")
print(
  round(
    cor_spearman,
    3
  )
)

# Comparación entre ambos métodos
pearson_table <- as.data.frame(
  as.table(cor_pearson)
) %>%
  rename(
    variable_1 = Var1,
    variable_2 = Var2,
    pearson_r = Freq
  )

spearman_table <- as.data.frame(
  as.table(cor_spearman)
) %>%
  rename(
    variable_1 = Var1,
    variable_2 = Var2,
    spearman_rho = Freq
  )

correlation_comparison <- pearson_table %>%
  left_join(
    spearman_table,
    by = c(
      "variable_1",
      "variable_2"
    )
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
  mutate(
    pearson_r = round(
      pearson_r,
      3
    ),
    
    spearman_rho = round(
      spearman_rho,
      3
    ),
    
    difference = round(
      abs(
        pearson_r -
          spearman_rho
      ),
      3
    )
  ) %>%
  arrange(
    desc(
      abs(spearman_rho)
    )
  )


cat("\n--- Comparison between Pearson and Spearman ---\n")

correlation_comparison %>%
  select(
    variable_1,
    variable_2,
    pearson_r,
    spearman_rho,
    difference
  ) %>%
  print(
    n = Inf,
    width = Inf
  )

# 6.2 ALTERNATIVE CORRELATION NETWORK
# Native q0, q1 and q2 + native individuals (%)
#-------------------------------------------------------------------------------
#
# Esta fue la versión anterior de la red. Se conserva para comparar la estructura
# de correlaciones al representar la biodiversidad solamente con especies nativas.

network_native_data <- master_raw %>%
  select(
    ID_site,
    
    SIG_mean,
    roads_mean,
    ghm_mean,
    loss_mean,
    
    qD_q0_native,
    qD_q1_native,
    qD_q2_native,
    native_individuals_pct,
    
    threat_admin,
    threat_mean,
    threat_researcher
  )

network_native_cor <- network_native_data %>%
  select(
    -ID_site
  ) %>%
  cor(
    method = "spearman",
    use = "complete.obs"
  )

network_native_edges <- as.data.frame(
  as.table(
    network_native_cor
  )
) %>%
  rename(
    from = Var1,
    to = Var2,
    rho = Freq
  ) %>%
  mutate(
    from = as.character(from),
    to = as.character(to)
  ) %>%
  filter(
    from != to
  ) %>%
  rowwise() %>%
  mutate(
    pair_id = paste(
      sort(
        c(
          from,
          to
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
  filter(
    abs(rho) >= 0.25
  )

cat("\n--- Alternative native-biodiversity network ---\n")

network_native_edges %>%
  arrange(
    desc(
      abs(rho)
    )
  ) %>%
  select(
    from,
    to,
    rho
  ) %>%
  print(
    n = Inf,
    width = Inf
  )

# 6.3 ORIGINAL MULTIVARIATE MODEL
# SIG + total Shannon diversity + threats
selection_original <- master_raw %>%
  transmute(
    ID_site,
    SIG_mean,
    qD_q1,
    threat_mean
  ) %>%
  mutate(
    across(
      c(
        SIG_mean,
        qD_q1,
        threat_mean
      ),
      ~ as.numeric(
        scale(.x)
      )
    )
  )

distance_matrix_original <- selection_original %>%
  column_to_rownames(
    "ID_site"
  ) %>%
  dist(
    method = "euclidean"
  ) %>%
  as.matrix()

original_multivariate_results <- site_combinations %>%
  rowwise() %>%
  mutate(
    metrics = list(
      calculate_distance_metrics(
        selected_sites = c_across(
          starts_with("site_q")
        ),
        distance_matrix = distance_matrix_original
      )
    )
  ) %>%
  ungroup() %>%
  unnest(
    metrics
  ) %>%
  mutate(
    rank_mean = min_rank(
      desc(mean_distance)
    ),
    
    rank_min = min_rank(
      desc(min_distance)
    ),
    
    rank_sum =
      rank_mean +
      rank_min
  ) %>%
  arrange(
    rank_sum,
    desc(mean_distance),
    desc(min_distance)
  )

cat("\n--- Original multivariate model: top combinations ---\n")

original_multivariate_results %>%
  slice_head(
    n = 3
  ) %>%
  select(
    site_q1,
    site_q2,
    site_q3,
    site_q4,
    site_q5,
    mean_distance,
    min_distance,
    rank_sum
  ) %>%
  print(
    n = Inf,
    width = Inf
  )


# 6.4 SIG-ONLY SELECTION
# Contraste para evaluar qué combinación representa mejor la separación interna
# del gradiente continuo de antropización.

calculate_SIG_metrics <- function(
    selected_sites,
    site_data
) {
  
  SIG_lookup <- setNames(
    site_data$SIG_mean,
    site_data$ID_site
  )
  
  SIG_values <- unname(
    SIG_lookup[
      selected_sites
    ]
  )
  
  SIG_gaps <- diff(
    SIG_values
  )
  
  tibble(
    SIG_range =
      SIG_values[5] -
      SIG_values[1],
    
    SIG_min_gap =
      min(SIG_gaps),
    
    SIG_gap_cv =
      sd(SIG_gaps) /
      mean(SIG_gaps)
  )
}

SIG_sensitivity_results <- site_combinations %>%
  rowwise() %>%
  mutate(
    metrics = list(
      calculate_SIG_metrics(
        selected_sites = c_across(
          starts_with("site_q")
        ),
        site_data = selection_data
      )
    )
  ) %>%
  ungroup() %>%
  unnest(
    metrics
  ) %>%
  mutate(
    rank_range = min_rank(
      desc(SIG_range)
    ),
    
    rank_min_gap = min_rank(
      desc(SIG_min_gap)
    ),
    
    rank_regularity = min_rank(
      SIG_gap_cv
    ),
    
    rank_sum =
      rank_range +
      rank_min_gap +
      rank_regularity
  ) %>%
  arrange(
    rank_sum,
    desc(SIG_range),
    desc(SIG_min_gap),
    SIG_gap_cv
  )

cat("\n--- Best SIG-only combination ---\n")

SIG_sensitivity_results %>%
  slice_head(
    n = 1
  ) %>%
  select(
    site_q1,
    site_q2,
    site_q3,
    site_q4,
    site_q5,
    SIG_range,
    SIG_min_gap,
    SIG_gap_cv,
    rank_sum
  ) %>%
  print(
    width = Inf
  )

# 6.5 BIODIVERSITY-ONLY SELECTION
# Contraste: selección basada únicamente en diferencias de diversidad
# total de Shannon.

biodiversity_only_data <- master_raw %>%
  transmute(
    ID_site,
    
    qD_q1_scaled =
      as.numeric(
        scale(qD_q1)
      )
  )


distance_matrix_biodiversity <- biodiversity_only_data %>%
  column_to_rownames(
    "ID_site"
  ) %>%
  dist(
    method = "euclidean"
  ) %>%
  as.matrix()


biodiversity_only_results <- site_combinations %>%
  rowwise() %>%
  mutate(
    metrics = list(
      calculate_distance_metrics(
        selected_sites = c_across(
          starts_with("site_q")
        ),
        distance_matrix = distance_matrix_biodiversity
      )
    )
  ) %>%
  ungroup() %>%
  unnest(
    metrics
  ) %>%
  mutate(
    rank_mean = min_rank(
      desc(mean_distance)
    ),
    
    rank_min = min_rank(
      desc(min_distance)
    ),
    
    rank_sum =
      rank_mean +
      rank_min
  ) %>%
  arrange(
    rank_sum,
    desc(mean_distance),
    desc(min_distance)
  )

cat("\n--- Best biodiversity-only combination ---\n")

biodiversity_only_results %>%
  slice_head(
    n = 1
  ) %>%
  select(
    site_q1,
    site_q2,
    site_q3,
    site_q4,
    site_q5,
    mean_distance,
    min_distance,
    rank_sum
  ) %>%
  print(
    width = Inf
  )

# 6.6 NATIVE BIODIVERSITY MODEL
# SIG + native Shannon diversity + threats
native_model_data <- master_raw %>%
  transmute(
    ID_site,
    SIG_mean,
    qD_q1_native,
    threat_mean
  ) %>%
  mutate(
    across(
      c(
        SIG_mean,
        qD_q1_native,
        threat_mean
      ),
      ~ as.numeric(
        scale(.x)
      )
    )
  )

distance_matrix_native <- native_model_data %>%
  column_to_rownames(
    "ID_site"
  ) %>%
  dist(
    method = "euclidean"
  ) %>%
  as.matrix()

native_model_results <- site_combinations %>%
  rowwise() %>%
  mutate(
    metrics = list(
      calculate_distance_metrics(
        selected_sites = c_across(
          starts_with("site_q")
        ),
        distance_matrix = distance_matrix_native
      )
    )
  ) %>%
  ungroup() %>%
  unnest(
    metrics
  ) %>%
  mutate(
    rank_mean = min_rank(
      desc(mean_distance)
    ),
    
    rank_min = min_rank(
      desc(min_distance)
    ),
    
    rank_sum =
      rank_mean +
      rank_min
  ) %>%
  arrange(
    rank_sum,
    desc(mean_distance),
    desc(min_distance)
  )

cat("\n--- Native biodiversity model: top combinations ---\n")

native_model_results %>%
  slice_head(
    n = 3
  ) %>%
  select(
    site_q1,
    site_q2,
    site_q3,
    site_q4,
    site_q5,
    mean_distance,
    min_distance,
    rank_sum
  ) %>%
  print(
    n = Inf,
    width = Inf
  )

# 6.7 BLOCK-WEIGHTED MODEL WITH TOTAL DIVERSITY
# SIG      = 1/3
# Threats  = 1/3
# Biology  = 1/3
#
# Biology:
#   total Shannon diversity = 1/6
#   native individuals (%)  = 1/6

total_weighted_data <- master_raw %>%
  transmute(
    ID_site,
    SIG_mean,
    threat_mean,
    qD_q1,
    native_individuals_pct
  ) %>%
  mutate(
    across(
      c(
        SIG_mean,
        threat_mean,
        qD_q1,
        native_individuals_pct
      ),
      ~ as.numeric(
        scale(.x)
      )
    ),
    
    SIG_weighted =
      SIG_mean *
      sqrt(1 / 3),
    
    threat_weighted =
      threat_mean *
      sqrt(1 / 3),
    
    total_diversity_weighted =
      qD_q1 *
      sqrt(1 / 6),
    
    native_pct_weighted =
      native_individuals_pct *
      sqrt(1 / 6)
  )

distance_matrix_total_weighted <- total_weighted_data %>%
  select(
    ID_site,
    SIG_weighted,
    threat_weighted,
    total_diversity_weighted,
    native_pct_weighted
  ) %>%
  column_to_rownames(
    "ID_site"
  ) %>%
  dist(
    method = "euclidean"
  ) %>%
  as.matrix()

total_weighted_results <- site_combinations %>%
  rowwise() %>%
  mutate(
    metrics = list(
      calculate_distance_metrics(
        selected_sites = c_across(
          starts_with("site_q")
        ),
        distance_matrix = distance_matrix_total_weighted
      )
    )
  ) %>%
  ungroup() %>%
  unnest(
    metrics
  ) %>%
  mutate(
    rank_mean = min_rank(
      desc(mean_distance)
    ),
    
    rank_min = min_rank(
      desc(min_distance)
    ),
    
    rank_sum =
      rank_mean +
      rank_min
  ) %>%
  arrange(
    rank_sum,
    desc(mean_distance),
    desc(min_distance)
  )

cat("\n--- Total diversity + native composition model: top combinations ---\n")

total_weighted_results %>%
  slice_head(
    n = 3
  ) %>%
  select(
    site_q1,
    site_q2,
    site_q3,
    site_q4,
    site_q5,
    mean_distance,
    min_distance,
    rank_sum
  ) %>%
  print(
    n = Inf,
    width = Inf
  )


# 7. CONTINUOUS-GRADIENT MULTIVARIATE SITE SELECTION
#-------------------------------------------------------------------------------
#
# Objetivo:
#
# Evaluar todas las combinaciones posibles de cinco sitios SIN imponer
# la restricción de un sitio por quintil.
#
# En lugar de quintiles, las combinaciones deben cumplir dos condiciones
# continuas mínimas:
#
#   1. Cubrir al menos 70% del gradiente regional
#   2. Mantener al menos 10 puntos percentiles entre sitios consecutivos
#
# Las combinaciones que cumplen estas condiciones se evalúan utilizando
# EXACTAMENTE el mismo modelo multivariado utilizado anteriormente:
#
#   SIG                       = 1/3
#   Amenazas                  = 1/3
#   Shannon nativa            = 1/6
#   Individuos nativos (%)    = 1/6
#-------------------------------------------------------------------------------

# 7.1 POSICIÓN CONTINUA DE LOS SITIOS EN EL GRADIENTE REGIONAL
regional_SIG_values <- as.numeric(
  terra::values(
    index,
    na.rm = TRUE
  )
)


regional_SIG_ecdf <- ecdf(
  regional_SIG_values
)


continuous_selection_data <- master_raw %>%
  
  transmute(
    
    ID_site,
    
    SIG_mean,
    threat_mean,
    qD_q1_native,
    native_individuals_pct,
    
    SIG_percentile =
      regional_SIG_ecdf(
        SIG_mean
      )
    
  ) %>%
  
  arrange(
    SIG_percentile
  )


cat(
  "\n--- Candidate sites along continuous anthropization gradient ---\n"
)


print(
  continuous_selection_data,
  n = Inf,
  width = Inf
)



# 7.2 GENERAR TODAS LAS COMBINACIONES DE CINCO SITIOS
all_combinations <- combn(
  continuous_selection_data$ID_site,
  5,
  simplify = FALSE
)


cat(
  "\nTotal combinations:",
  length(
    all_combinations
  ),
  "\n"
)


stopifnot(
  length(
    all_combinations
  ) == 792
)

# 7.3 CALCULAR COBERTURA DEL GRADIENTE PARA CADA COMBINACIÓN
calculate_gradient_metrics <- function(
    selected_sites,
    site_data
) {
  
  selected_data <- site_data %>%
    
    filter(
      ID_site %in% selected_sites
    ) %>%
    
    arrange(
      SIG_percentile
    )
  
  
  percentile_values <-
    selected_data$SIG_percentile
  
  
  percentile_gaps <- diff(
    percentile_values
  )
  
  
  tibble(
    
    site_1 =
      selected_data$ID_site[1],
    
    site_2 =
      selected_data$ID_site[2],
    
    site_3 =
      selected_data$ID_site[3],
    
    site_4 =
      selected_data$ID_site[4],
    
    site_5 =
      selected_data$ID_site[5],
    
    
    percentile_1 =
      percentile_values[1],
    
    percentile_2 =
      percentile_values[2],
    
    percentile_3 =
      percentile_values[3],
    
    percentile_4 =
      percentile_values[4],
    
    percentile_5 =
      percentile_values[5],
    
    
    gradient_range =
      max(
        percentile_values
      ) -
      min(
        percentile_values
      ),
    
    
    gradient_min_gap =
      min(
        percentile_gaps
      ),
    
    
    gradient_mean_gap =
      mean(
        percentile_gaps
      ),
    
    
    gradient_gap_cv =
      sd(
        percentile_gaps
      ) /
      mean(
        percentile_gaps
      )
  )
}

# 7.4 EVALUAR COBERTURA DEL GRADIENTE
gradient_combinations <- map_dfr(
  
  seq_along(
    all_combinations
  ),
  
  function(i) {
    
    calculate_gradient_metrics(
      
      selected_sites =
        all_combinations[[i]],
      
      site_data =
        continuous_selection_data
    )
  }
  
) %>%
  
  mutate(
    
    combination_id =
      row_number(),
    
    .before = 1
  )

# 7.5 RESTRICCIÓN CONTINUA DEL GRADIENTE
# Una combinación es válida si:
#
#   gradient_range   >= 0.70
#   gradient_min_gap >= 0.10

gradient_range_threshold <- 0.70

gradient_min_gap_threshold <- 0.10


valid_combinations <- gradient_combinations %>%
  
  filter(
    
    gradient_range >=
      gradient_range_threshold,
    
    gradient_min_gap >=
      gradient_min_gap_threshold
  )

cat(
  "\nTotal combinations:",
  nrow(
    gradient_combinations
  ),
  "\n"
)

cat(
  "Combinations retained after continuous-gradient constraint:",
  nrow(
    valid_combinations
  ),
  "\n"
)

cat(
  "Proportion retained:",
  round(
    nrow(
      valid_combinations
    ) /
      nrow(
        gradient_combinations
      ),
    3
  ),
  "\n"
)




# 7.6 ESTANDARIZAR VARIABLES
# EXACTAMENTE el mismo procedimiento del modelo principal.

continuous_scaled <- continuous_selection_data %>%
  
  mutate(
    
    across(
      
      c(
        SIG_mean,
        threat_mean,
        qD_q1_native,
        native_individuals_pct
      ),
      
      ~ as.numeric(
        scale(.x)
      )
    )
  )

# 7.7 APLICAR LOS MISMOS PESOS
continuous_weighted <- continuous_scaled %>%
  
  mutate(
    
    SIG_weighted =
      SIG_mean *
      sqrt(1 / 3),
    
    threat_weighted =
      threat_mean *
      sqrt(1 / 3),
    
    native_diversity_weighted =
      qD_q1_native *
      sqrt(1 / 6),
    
    native_pct_weighted =
      native_individuals_pct *
      sqrt(1 / 6)
  )

# 7.8 MATRIZ DE DISTANCIAS MULTIVARIADAS
continuous_distance_matrix <- continuous_weighted %>%
  
  select(
    
    ID_site,
    
    SIG_weighted,
    threat_weighted,
    native_diversity_weighted,
    native_pct_weighted
  ) %>%
  
  column_to_rownames(
    "ID_site"
  ) %>%
  
  dist(
    method = "euclidean"
  ) %>%
  
  as.matrix()

# 7.9 FUNCIÓN PARA DISTANCIAS
# Igual a la utilizada en el análisis original.

calculate_continuous_distance_metrics <- function(
    selected_sites,
    distance_matrix
) {
  
  distance_submatrix <- distance_matrix[
    selected_sites,
    selected_sites,
    drop = FALSE
  ]
  
  
  pairwise_distances <- distance_submatrix[
    upper.tri(
      distance_submatrix
    )
  ]
  
  
  tibble(
    
    mean_distance =
      mean(
        pairwise_distances
      ),
    
    min_distance =
      min(
        pairwise_distances
      )
  )
}

# 7.10 CALCULAR DISTANCIAS PARA LAS COMBINACIONES VÁLIDAS
continuous_multivariate_results <- valid_combinations %>%
  
  rowwise() %>%
  
  mutate(
    
    metrics = list(
      
      calculate_continuous_distance_metrics(
        
        selected_sites = c(
          site_1,
          site_2,
          site_3,
          site_4,
          site_5
        ),
        
        distance_matrix =
          continuous_distance_matrix
      )
    )
  ) %>%
  
  ungroup() %>%
  
  unnest(
    metrics
  )

# 7.11 RANKING#
# EXACTAMENTE la misma lógica del análisis original.

continuous_multivariate_results <- continuous_multivariate_results %>%
  
  mutate(
    
    rank_mean =
      min_rank(
        desc(
          mean_distance
        )
      ),
    
    
    rank_min =
      min_rank(
        desc(
          min_distance
        )
      ),
    
    
    rank_sum =
      rank_mean +
      rank_min
  ) %>%
  
  arrange(
    
    rank_sum,
    
    desc(
      mean_distance
    ),
    
    desc(
      min_distance
    )
  )

# 7.12 TOP 3
top3_continuous <- continuous_multivariate_results %>%
  
  slice_head(
    n = 3
  ) %>%
  
  mutate(
    
    option =
      as.character(
        row_number()
      )
  )

cat(
  "\n--- Best continuous-gradient multivariate combinations ---\n"
)

top3_continuous %>%
  
  select(
    
    option,
    
    site_1,
    site_2,
    site_3,
    site_4,
    site_5,
    
    percentile_1,
    percentile_2,
    percentile_3,
    percentile_4,
    percentile_5,
    
    gradient_range,
    gradient_min_gap,
    
    mean_distance,
    min_distance,
    
    rank_mean,
    rank_min,
    rank_sum
    
  ) %>%
  
  print(
    n = Inf,
    width = Inf
  )



# Identificación de las tres mejores combinaciones
top3_label <- paste0(
  "Combinations\n",
  paste(
    paste0(
      top3_continuous$option,
      "   ",
      top3_continuous$site_1,
      " · ",
      top3_continuous$site_2,
      " · ",
      top3_continuous$site_3,
      " · ",
      top3_continuous$site_4,
      " · ",
      top3_continuous$site_5
    ),
    collapse = "\n"
  )
)

# 7.13 FIGURE – MULTIVARIATE DIFFERENTIATION
# Misma lógica de la figura original:
#
# x = mean_distance
# y = min_distance
#
# Ambas deben ser altas.

mean_distance_cut <- median(
  continuous_multivariate_results$mean_distance
)


min_distance_cut <- median(
  continuous_multivariate_results$min_distance
)


continuous_plot_data <- continuous_multivariate_results %>%
  
  left_join(
    
    top3_continuous %>%
      select(
        combination_id,
        option
      ),
    
    by = "combination_id"
  ) %>%
  
  mutate(
    
    highlighted = if_else(
      
      is.na(
        option
      ),
      
      "Other combinations",
      
      "Top three combinations"
    )
  )

figure_continuous_selection <- ggplot(
  
  continuous_plot_data,
  
  aes(
    x = mean_distance,
    y = min_distance
  )
  
) +
  
  # Cuadrante superior derecho
  
  annotate(
    
    "rect",
    
    xmin =
      mean_distance_cut,
    
    xmax =
      Inf,
    
    ymin =
      min_distance_cut,
    
    ymax =
      Inf,
    
    fill =
      "grey90",
    
    alpha =
      0.45
  ) +
  
  
  geom_vline(
    
    xintercept =
      mean_distance_cut,
    
    linetype =
      "dashed",
    
    colour =
      "grey45",
    
    linewidth =
      0.65
  ) +
  
  
  geom_hline(
    
    yintercept =
      min_distance_cut,
    
    linetype =
      "dashed",
    
    colour =
      "grey45",
    
    linewidth =
      0.65
  ) +
  
  
  # Otras combinaciones
  
  geom_point(
    
    data =
      continuous_plot_data %>%
      filter(
        highlighted ==
          "Other combinations"
      ),
    
    colour =
      "grey70",
    
    size =
      2.8,
    
    alpha =
      0.80
  ) +
  
  
  # Top 3
  
  geom_point(
    
    data =
      continuous_plot_data %>%
      filter(
        highlighted ==
          "Top three combinations"
      ),
    
    shape =
      21,
    
    size =
      5.5,
    
    stroke =
      1.1,
    
    fill =
      "white",
    
    colour =
      "#2C7FB8"
  ) +
  
  
  geom_point(
    
    data =
      continuous_plot_data %>%
      filter(
        highlighted ==
          "Top three combinations"
      ),
    
    size =
      2.8,
    
    colour =
      "#2C7FB8"
  ) +
  
  
  geom_label_repel(
    
    data =
      continuous_plot_data %>%
      filter(
        highlighted ==
          "Top three combinations"
      ),
    
    aes(
      label =
        option
    ),
    
    seed =
      123,
    
    size =
      4,
    
    fontface =
      "bold",
    
    colour =
      "#2C7FB8",
    
    fill =
      "white",
    
    label.size =
      0,
    
    box.padding =
      0.45,
    
    point.padding =
      0.35,
    
    min.segment.length =
      0,
    
    segment.colour =
      "#2C7FB8",
    
    segment.linewidth =
      0.4,
    
    max.overlaps =
      Inf
  ) +
  
  # Identificación de las tres combinaciones
  
  annotate(
    "text",
    x = Inf,
    y = min_distance_cut + 0.03,
    label = top3_label,
    hjust = 1.05,
    vjust = 0,
    size = 3.5,
    lineheight = 1.25,
    colour = "grey30"
  ) +
  
  labs(
    
    title =
      "Multivariate differentiation among continuous-gradient combinations",
    
    subtitle =
      "",
    
    x =
      "Mean pairwise distance",
    
    y =
      "Minimum pairwise distance"
  ) +
  
  
  scale_x_continuous(
    
    expand =
      expansion(
        
        mult = c(
          0.04,
          0.10
        )
      )
  ) +
  
  
  scale_y_continuous(
    
    expand =
      expansion(
        
        mult = c(
          0.05,
          0.12
        )
      )
  ) +
  
  
  theme_classic(
    base_size = 12
  ) +
  
  
  theme(
    
    axis.title =
      element_text(
        
        face =
          "bold",
        
        size =
          12
      ),
    
    
    axis.text =
      element_text(
        
        colour =
          "grey30"
      ),
    
    
    axis.line =
      element_line(
        
        colour =
          "grey35",
        
        linewidth =
          0.45
      ),
    
    
    plot.title =
      element_text(
        
        face =
          "bold",
        
        size =
          15
      ),
    
    
    plot.subtitle =
      element_text(
        
        size =
          10,
        
        colour =
          "grey38",
        
        margin =
          margin(
            b = 10
          )
      ),
    
    
    plot.margin =
      margin(
        
        t = 12,
        r = 25,
        b = 12,
        l = 12
      )
  )

figure_continuous_selection

ggsave(
  filename = "C:/Users/Dell/Desktop/Figure_continuous_gradient_selection_ECO3.png",
  plot = figure_continuous_selection,
  width = 8.5,
  height = 6.5,
  dpi = 600,
  bg = "white"
)


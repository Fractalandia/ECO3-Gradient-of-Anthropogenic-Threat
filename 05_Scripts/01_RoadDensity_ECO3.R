#===============================================================================
# Road Density – ECO3
# FONDECYT Regular Grant No. 1251095
#
# Purpose:
# Calculate road density for ECO3 below 2000 m a.s.l. using the national road
# network ((2024). Road density is calculated on a 1 km grid, smoothed with a 5 x 5 km
# focal window, and normalized to 0–1 using the 2nd and 98th percentiles.
#
# Main outputs:
#   OUT/03_INTERMEDIATE/RoadDensity/Road_density_ECO3.tif
#   OUT/03_INTERMEDIATE/RoadDensity/Road_density_ECO3_trim.tif
#===============================================================================

library(terra)
library(sf)

terraOptions(progress = 1, memfrac = 0.8)

#Paths
base_dir <- "D:/Mapas_R"

path_vectors <- file.path(base_dir, "Vector")
out_dir      <- file.path(base_dir, "OUT")
out_roads    <- file.path(out_dir, "03_INTERMEDIATE", "RoadDensity")

if (!dir.exists(out_roads)) dir.create(out_roads, recursive = TRUE)

# 1. Input layers
#-------------------------------------------------------------------------------

mask_poly <- vect(file.path(out_dir, "03_INTERMEDIATE", "Masks",
                            "mask_bioregion3_lt2000_poly_preciso.gpkg"))

gdb_path <- file.path(path_vectors, "Red_Vial_Chile",
                      "Red_Vial_Chile_07_02_2024.gdb")

# 2. Load and prepare road network
#-------------------------------------------------------------------------------

roads <- st_read(gdb_path, layer = "Red_Vial_Chile", quiet = TRUE)

roads <- st_zm(roads)
roads <- st_transform(roads, crs(mask_poly))

# 3. Create 1 km grid for ECO3
#-------------------------------------------------------------------------------

grid_1km <- st_make_grid(
  mask_poly,
  cellsize = 1000,
  what = "polygons",
  square = TRUE
)

grid_1km <- st_sf(
  id = seq_along(grid_1km),
  geometry = grid_1km
)

# 4. Calculate km of roads per 1 km grid cell
#-------------------------------------------------------------------------------

roads_int <- st_intersection(roads, grid_1km)
roads_int$km <- as.numeric(st_length(roads_int)) / 1000

dens_table <- aggregate(
  roads_int["km"],
  grid_1km,
  sum,
  na.rm = TRUE
)

grid_1km$km <- dens_table$km
grid_1km$km[is.na(grid_1km$km)] <- 0

r_1km <- rast(
  ext(grid_1km),
  resolution = 1000,
  crs = crs(mask_poly)
)

dens_1km <- rasterize(
  vect(grid_1km),
  r_1km,
  field = "km",
  background = 0
)

# 5. Smooth with a 5x5 km focal window
#-------------------------------------------------------------------------------

w <- matrix(1, nrow = 5, ncol = 5)

dens_smooth <- focal(
  dens_1km,
  w = w,
  fun = mean,
  na.rm = TRUE
)

# 6. Normalize to 0-1 using 2nd and 98th percentiles
#-------------------------------------------------------------------------------

vals <- values(dens_smooth, na.rm = TRUE)
qs <- quantile(vals, c(0.02, 0.98), na.rm = TRUE)

if (qs[2] <= qs[1]) {
  stop("The 2nd and 98th percentiles are equal. Check road density distribution.")
}

dens_clamp <- clamp(dens_smooth, qs[1], qs[2], values = TRUE)
dens_norm <- (dens_clamp - qs[1]) / (qs[2] - qs[1])
dens_norm <- clamp(dens_norm, 0, 1, values = TRUE)

dens_final <- mask(dens_norm, mask_poly)
names(dens_final) <- "road_density_01"

# Trimmed version for visualization only
dens_final_trim <- trim(dens_final)

# 7. Outputs
#-------------------------------------------------------------------------------

writeRaster(
  dens_final,
  file.path(out_roads, "Road_density_ECO3.tif"),
  datatype = "FLT4S",
  overwrite = TRUE,
  wopt = list(gdal = "COMPRESS=LZW")
)

writeRaster(
  dens_final_trim,
  file.path(out_roads, "Road_density_ECO3_trim.tif"),
  datatype = "FLT4S",
  overwrite = TRUE,
  wopt = list(gdal = "COMPRESS=LZW")
)

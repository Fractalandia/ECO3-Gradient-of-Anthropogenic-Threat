#===============================================================================
# Global Human Modification (gHM) – ECO3
# FONDECYT Regular Grant No. 1251095
#
# Purpose:
# Prepare the continuous Global Human Modification (gHM) layer for ECO3.
# The original 0–1 raster is aligned to the 1 km Road Density master grid,
# restricted to ECO3 below 2000 m a.s.l., and kept on its original continuous
# scale.
#
# Main output:
#   OUT/03_INTERMEDIATE/gHM/GHM_ECO3.tif
#===============================================================================

library(terra)

terraOptions(progress = 1, memfrac = 0.8)

# Paths
base_dir <- "D:/Mapas_R"

out_dir  <- file.path(base_dir, "OUT")
mask_dir <- file.path(base_dir, "MASK")
out_ghm  <- file.path(out_dir, "03_INTERMEDIATE", "gHM")

if (!dir.exists(out_ghm)) dir.create(out_ghm, recursive = TRUE)

# 1. Input layers
#-------------------------------------------------------------------------------

# Original continuous gHM raster
ghm_raw <- rast(file.path(mask_dir, "nv_gHM.tif"))

# Road Density defines the master 1 km grid
roads01 <- rast(file.path(out_dir, "03_INTERMEDIATE", "RoadDensity",
                          "Road_density_ECO3.tif"))

# ECO3 <2000 m study-area mask
mask_poly <- vect(file.path(out_dir, "03_INTERMEDIATE", "Masks",
                            "mask_bioregion3_lt2000_poly_preciso.gpkg"))

# 2. Keep gHM on its continuous 0–1 scale
#-------------------------------------------------------------------------------

ghm_raw <- clamp(ghm_raw, 0, 1, values = TRUE)

# 3. Align gHM to the Road Density master grid
#-------------------------------------------------------------------------------

# gHM is continuous, so bilinear interpolation is used.
ghm_1km <- project(
  ghm_raw,
  roads01,
  method = "bilinear"
)

ghm_1km <- clamp(ghm_1km, 0, 1, values = TRUE)

# 4. Restrict to ECO3 below 2000 m
#-------------------------------------------------------------------------------

mask_poly_1km <- project(mask_poly, crs(roads01))
ghm_1km <- mask(ghm_1km, mask_poly_1km)

# Keep the same valid spatial footprint as Road Density
ghm_1km <- mask(ghm_1km, roads01)

names(ghm_1km) <- "gHM_cont"

stopifnot(compareGeom(ghm_1km, roads01, stopOnError = TRUE))

# 5. Output
#-------------------------------------------------------------------------------

writeRaster(
  ghm_1km,
  file.path(out_ghm, "GHM_ECO3.tif"),
  datatype = "FLT4S",
  overwrite = TRUE,
  wopt = list(gdal = "COMPRESS=LZW")
)

#===============================================================================
# Integrated Gradient of Anthropogenic Threats (gAT) – ECO3
# FONDECYT Regular Grant No. 1251095
#
# Purpose:
# Integrate the three final anthropogenic threat indicators for ECO3:
#   - Road Density
#   - Loss Nature
#   - Global Human Modification (gHM)
#
# The gAT is calculated as the arithmetic mean of the three 0–1 components.
# Urban areas plus a 1 km buffer are then excluded, and the resulting continuous
# gradient is classified into five empirical quintiles.
#
# Main outputs:
#   OUT/01_FINAL_INDEX/gAT_ECO3.tif
#   OUT/01_FINAL_INDEX/gAT_quintiles_ECO3.tif
#===============================================================================

library(terra)
library(sf)

terraOptions(progress = 1, memfrac = 0.8)

#Paths
base_dir <- "D:/Mapas_R"

out_dir    <- file.path(base_dir, "OUT")
vector_dir <- file.path(base_dir, "Vector")
dir_index  <- file.path(out_dir, "01_FINAL_INDEX")

if (!dir.exists(dir_index)) dir.create(dir_index, recursive = TRUE)

# 1. Input layers
#-------------------------------------------------------------------------------

path_roads <- file.path(out_dir, "03_INTERMEDIATE", "RoadDensity",
                        "Road_density_ECO3.tif")

path_loss <- file.path(out_dir, "03_INTERMEDIATE", "LossNature",
                       "Loss_Nature_ECO3.tif")

path_ghm <- file.path(out_dir, "03_INTERMEDIATE", "gHM",
                      "GHM_ECO3.tif")

path_urban <- file.path(vector_dir, "urbano_buffer1km.gpkg")

required_files <- c(path_roads, path_loss, path_ghm, path_urban)

if (!all(file.exists(required_files))) {
  stop(
    "One or more required input files are missing:\n",
    paste(required_files[!file.exists(required_files)], collapse = "\n")
  )
}

# 2. Load the three components
#-------------------------------------------------------------------------------

roads01 <- rast(path_roads)
loss01  <- rast(path_loss)
ghm01   <- rast(path_ghm)

stopifnot(
  compareGeom(roads01, loss01, stopOnError = TRUE),
  compareGeom(roads01, ghm01, stopOnError = TRUE)
)

roads01 <- clamp(roads01, 0, 1, values = TRUE)
loss01  <- clamp(loss01, 0, 1, values = TRUE)
ghm01   <- clamp(ghm01, 0, 1, values = TRUE)

names(roads01) <- "roads_01"
names(loss01)  <- "loss_01"
names(ghm01)   <- "ghm_01"

# 3. Calculate the integrated gAT
#-------------------------------------------------------------------------------

gAT <- (roads01 + loss01 + ghm01) / 3
gAT <- clamp(gAT, 0, 1, values = TRUE)

names(gAT) <- "gAT"

# 4. Exclude urban areas + 1 km buffer
#-------------------------------------------------------------------------------

urb <- st_read(path_urban, quiet = TRUE) |>
  st_make_valid() |>
  st_transform(st_crs(crs(roads01)))

urb_r <- rasterize(
  vect(urb),
  roads01,
  field = 1,
  background = 0
)

gAT_noUrban <- mask(gAT, urb_r, maskvalues = 1)
names(gAT_noUrban) <- "gAT"

# 5. Continuous output 
#-------------------------------------------------------------------------------

writeRaster(
  gAT_noUrban,
  file.path(dir_index, "gAT_ECO3.tif"),
  datatype = "FLT4S",
  overwrite = TRUE,
  wopt = list(gdal = "COMPRESS=LZW")
)

# 6. Empirical quintiles
#-------------------------------------------------------------------------------

qs <- as.numeric(
  global(
    gAT_noUrban,
    quantile,
    probs = seq(0, 1, 0.2),
    na.rm = TRUE
  )[1, ]
)

rcl <- cbind(
  qs[-length(qs)],
  qs[-1],
  1:5
)

# Include the absolute maximum in Q5
rcl[5, 2] <- rcl[5, 2] + 1e-9

gAT_quintiles <- classify(
  gAT_noUrban,
  rcl,
  include.lowest = TRUE,
  right = FALSE
)

names(gAT_quintiles) <- "gAT_level_1_5"

writeRaster(
  gAT_quintiles,
  file.path(dir_index, "gAT_quintiles_ECO3.tif"),
  datatype = "INT1U",
  overwrite = TRUE,
  wopt = list(gdal = "COMPRESS=LZW")
)

print(qs)

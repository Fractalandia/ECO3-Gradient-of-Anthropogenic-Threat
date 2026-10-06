#===============================================================================
# Loss Nature 
# FONDECYT Regular Grant No. 1251095
#
# Purpose:
# Calculate loss of natural ecosystems between 2000 and 2024 using MapBiomas
# Chile Collection 2. Loss is defined as natural cover in 2000 changing to
# anthropogenic cover in 2024.
#
# Processing:
#   1. Reproject MapBiomas 2000 and 2024 to the aligned 30 m working grid.
#   2. Identify natural cover in 2000 and anthropogenic cover in 2024.
#   3. Define natural-to-anthropogenic transitions as loss.
#   4. Apply the <2000 m mask.
#   5. Aggregate 33 x 33 cells (~1 km) using the mean.
#   6. Crop and mask to ECO3.
#   7. Align the continuous proportion layer to the Road Density master grid.
#
# Output:
#   OUT/03_INTERMEDIATE/LossNature/Loss_Nature.tif
#===============================================================================

library(terra)

terraOptions(progress = 1, memfrac = 0.8)

# Paths
base_dir <- "D:/Mapas_R"

path_rasters <- file.path(base_dir, "Raster")
path_masks   <- file.path(base_dir, "MASK")
out_dir      <- file.path(base_dir, "OUT")
out_loss     <- file.path(out_dir, "03_INTERMEDIATE", "LossNature")

if (!dir.exists(out_loss)) dir.create(out_loss, recursive = TRUE)

# 1. Input layers
#-------------------------------------------------------------------------------

# MapBiomas Chile Collection 2
cov_2000_raw <- rast(file.path(path_rasters, "MapBiomas_Chile_C2_2000.tif"))[[1]]
cov_2024_raw <- rast(file.path(path_rasters, "MapBiomas_Chile_C2_2024.tif"))[[1]]

# Original 30 m master grid / elevation mask
altmask_30m <- rast(file.path(path_masks, "mask2000_30m_102033.tif"))

# ECO3 <2000 m study-area mask
mask_poly <- vect(file.path(out_dir, "03_INTERMEDIATE", "Masks",
                            "mask_bioregion3_lt2000_poly_preciso.gpkg"))

# Road Density defines the master 1 km grid
roads_1km <- rast(file.path(out_dir, "03_INTERMEDIATE", "RoadDensity",
                            "Road_density.tif"))

# 2. Define the aligned ~1 km aggregation grid
#-------------------------------------------------------------------------------

res_x <- res(altmask_30m)[1]
res_y <- res(altmask_30m)[2]

fact_1km <- round(1000 / res_x)

block_x <- res_x * fact_1km
block_y <- res_y * fact_1km

mask_poly_30m <- project(mask_poly, crs(altmask_30m))
bio3_ext <- ext(mask_poly_30m)
full_ext <- ext(altmask_30m)

# Preserve the origin of the original 30 m grid so that 33 x 33 aggregation
# blocks remain identical to those of the full raster.
work_xmin <- xmin(full_ext) +
  floor((xmin(bio3_ext) - xmin(full_ext)) / block_x) * block_x

work_xmax <- xmin(full_ext) +
  ceiling((xmax(bio3_ext) - xmin(full_ext)) / block_x) * block_x

# Raster rows are referenced from the maximum Y coordinate downward.
work_ymax <- ymax(full_ext) -
  floor((ymax(full_ext) - ymax(bio3_ext)) / block_y) * block_y

work_ymin <- ymax(full_ext) -
  ceiling((ymax(full_ext) - ymin(bio3_ext)) / block_y) * block_y

work_xmin <- max(work_xmin, xmin(full_ext))
work_xmax <- min(work_xmax, xmax(full_ext))
work_ymin <- max(work_ymin, ymin(full_ext))
work_ymax <- min(work_ymax, ymax(full_ext))

work_ext <- ext(work_xmin, work_xmax, work_ymin, work_ymax)
altmask_work <- crop(altmask_30m, work_ext)

# Ensure complete 33 x 33 aggregation blocks
stopifnot(
  nrow(altmask_work) %% fact_1km == 0,
  ncol(altmask_work) %% fact_1km == 0
)

# 3. Crop and reproject MapBiomas to the 30 m working grid
#-------------------------------------------------------------------------------

work_poly_30m <- as.polygons(ext(altmask_work))
crs(work_poly_30m) <- crs(altmask_30m)

work_poly_wgs84 <- project(work_poly_30m, crs(cov_2000_raw))

cov_2000_crop <- crop(cov_2000_raw, work_poly_wgs84)
cov_2024_crop <- crop(cov_2024_raw, work_poly_wgs84)

# MapBiomas is categorical, so nearest-neighbour reprojection is used.
cov_2000 <- project(cov_2000_crop, altmask_work, method = "near")
cov_2024 <- project(cov_2024_crop, altmask_work, method = "near")

stopifnot(
  compareGeom(cov_2000, cov_2024, stopOnError = TRUE),
  compareGeom(cov_2000, altmask_work, stopOnError = TRUE)
)

# 4. MapBiomas classes
#-------------------------------------------------------------------------------

# Natural classes. Class 11 is included; classes 25 and 33 are not classified
# as natural or anthropogenic for the loss transition.
ids_natural <- c(
  3, 59, 60, 67,
  11, 12, 63, 66, 29,
  23, 61, 34
)

# Anthropogenic classes
ids_anthro <- c(9, 18, 15, 24)

# 5. Calculate nature loss from 2000 to 2024 at 30 m
#-------------------------------------------------------------------------------

nat_2000_log <- cov_2000 %in% ids_natural
anth_2024_log <- cov_2024 %in% ids_anthro

loss_nat_30m <- nat_2000_log & anth_2024_log
loss_nat_30m <- ifel(loss_nat_30m, 1, 0)

# Restrict to elevations below 2000 m
loss_nat_30m_mask <- mask(loss_nat_30m, altmask_work, maskvalues = 0)
names(loss_nat_30m_mask) <- "loss_nat_2000_2024"

# 6. Aggregate to ~1 km
#-------------------------------------------------------------------------------

# The mean represents the proportion of 30 m cells that experienced a
# natural-to-anthropogenic transition within each 33 x 33 block.
loss_nat_1km <- aggregate(
  loss_nat_30m_mask,
  fact = fact_1km,
  fun = mean,
  na.rm = TRUE
)

names(loss_nat_1km) <- "loss_01"

# 7. Crop and mask to ECO3
#-------------------------------------------------------------------------------

mask_poly_1km <- project(mask_poly, crs(loss_nat_1km))

loss_nat_1km_b3 <- crop(loss_nat_1km, mask_poly_1km)
loss_nat_1km_b3 <- mask(loss_nat_1km_b3, mask_poly_1km)

# 8. Align to the Road Density master grid
#-------------------------------------------------------------------------------

# Loss Nature is continuous after aggregation, so bilinear interpolation is used.
loss_aligned <- resample(loss_nat_1km_b3, roads_1km, method = "bilinear")
loss_aligned <- clamp(loss_aligned, 0, 1, values = TRUE)
names(loss_aligned) <- "loss_01"

stopifnot(compareGeom(loss_aligned, roads_1km, stopOnError = TRUE))

# 9. Output
#-------------------------------------------------------------------------------

writeRaster(
  loss_aligned,
  file.path(out_loss, "Loss_Nature.tif"),
  datatype = "FLT4S",
  overwrite = TRUE,
  wopt = list(gdal = "COMPRESS=LZW")
)

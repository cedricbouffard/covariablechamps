library(sf)
library(terra)
source("R/orientation.R")

# Long rectangle 100x20, orientation 0 (Math)
rect <- matrix(c(
  0, 0,
  100, 0,
  100, 20,
  0, 20,
  0, 0
), ncol = 2, byrow = TRUE)
champ <- st_sf(geometry = st_sfc(st_polygon(list(rect)), crs = 32618))

# Calculate distances
res <- calculer_distance_bordures_orientee(champ_poly = champ, resolution = 1)

# Inspect a point at (10, 10) - near the left end
# Nearest border is (0, 10) -> vector (-10, 0). Proj Long (1, 0) = 10.
val_10_10 <- terra::extract(res$distance_long, matrix(c(10, 10), ncol=2))[1,1]
cat("Distance Long at (10, 10):", val_10_10, "(Expected: ~10)\n")

# Inspect a point at (50, 5) - middle of the field
# Nearest border is (50, 0) -> vector (0, -5). Proj Long (1, 0) = 0.
val_50_5 <- terra::extract(res$distance_long, matrix(c(50, 5), ncol=2))[1,1]
cat("Distance Long at (50, 5):", val_50_5, "(Expected: ~0)\n")

# Inspect a point at (50, 5) for distance_large
val_large_50_5 <- terra::extract(res$distance_large, matrix(c(50, 5), ncol=2))[1,1]
cat("Distance Large at (50, 5):", val_large_50_5, "(Expected: ~5)\n")

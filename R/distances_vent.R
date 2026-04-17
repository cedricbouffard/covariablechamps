#' Calculer les distances aux arbres selon la direction du vent
#'
#' @param arbres_sf Objet sf POINT
#' @param angle_vent Angle du vent en degrés (0-360)
#' @param champ_bbox Objet sf avec le contour du champ
#' @param resolution Résolution du raster en mètres
#' @param max_distance Distance maximale à calculer
#' @param ouverture_angulaire Angle d'ouverture en degrés
#'
#' @return Liste avec les distances
#' @export
#'
calculer_distances_vent <- function(arbres_sf,
                                    angle_vent,
                                    champ_bbox,
                                    resolution = 2,
                                    max_distance = 100,
                                    ouverture_angulaire = 45) {

  if (!requireNamespace("terra", quietly = TRUE))
    stop("Le package 'terra' est requis.")
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Le package 'sf' est requis.")

  # Obtenir le CRS des arbres et du champ
  crs_arbres <- sf::st_crs(arbres_sf)
  crs_champ <- sf::st_crs(champ_bbox)

  message("CRS arbres: ", crs_arbres$input)
  message("CRS champ: ", crs_champ$input)

  # Utiliser le CRS des arbres comme référence
  crs_ref <- crs_arbres
  message("Utilisation du CRS des arbres comme référence")

  # S'assurer que les deux sont dans le même CRS
  if (is.na(crs_champ)) {
    message("Champ sans CRS, reprojection vers le CRS des arbres")
    champ_bbox <- sf::st_set_crs(champ_bbox, crs_arbres$input)
  } else if (crs_arbres$input != crs_champ$input) {
    message("Reprojection du champ vers le CRS des arbres")
    champ_bbox <- sf::st_transform(champ_bbox, crs_arbres)
  }

  # Obtenir la bounding box du champ
  bbox <- sf::st_bbox(champ_bbox)
  xmin <- as.numeric(bbox["xmin"])
  ymin <- as.numeric(bbox["ymin"])
  xmax <- as.numeric(bbox["xmax"])
  ymax <- as.numeric(bbox["ymax"])

  message(sprintf("Bounding box: [%.1f, %.1f] x [%.1f, %.1f]", xmin, xmax, ymin, ymax))

  # Vérifier que les coordonnées sont cohérentes
  if (xmax - xmin < 1 || ymax - ymin < 1) {
    stop("Bounding box trop petite - vérifiez le CRS des données")
  }

  # Étendre pour la zone de calcul
  xmin_et <- xmin - max_distance
  ymin_et <- ymin - max_distance
  xmax_et <- xmax + max_distance
  ymax_et <- ymax + max_distance

  message("Création du raster...")
  r_template <- terra::rast(
    ncols = max(1, ceiling((xmax_et - xmin_et) / resolution)),
    nrows = max(1, ceiling((ymax_et - ymin_et) / resolution)),
    xmin = xmin_et, xmax = xmax_et,
    ymin = ymin_et, ymax = ymax_et,
    crs = crs_ref$input
  )

  message(sprintf("Raster créé: %dx%d cellules", terra::ncol(r_template), terra::nrow(r_template)))

  # Rasteriser les arbres
  message("Rasterisation des arbres...")
  vect_arbres <- terra::vect(arbres_sf)
  arbres_raster <- terra::rasterize(vect_arbres, r_template, field = 1, background = NA)

  vals_arbres <- terra::values(arbres_raster)
  n_arbres <- sum(!is.na(vals_arbres))
  message(sprintf("Arbres rasterisés: %d", n_arbres))

  if (n_arbres == 0) {
    stop("Aucun arbre trouvé. Vérifiez que les arbres sont dans la zone du champ.")
  }

  # Distance
  message("Calcul des distances...")
  dist_raster <- terra::distance(arbres_raster, unit = "m")
  dist_raster <- terra::ifel(dist_raster > max_distance, max_distance, dist_raster)

  # Calcul de la direction
  message("Calcul des directions...")
  xy_arbres <- terra::xyFromCell(arbres_raster, which(!is.na(terra::values(arbres_raster))))
  xy_all <- terra::xyFromCell(dist_raster, 1:terra::ncell(dist_raster))
  angle_vent_rad <- (90 - angle_vent) * pi / 180
  n_cells <- nrow(xy_all)

  message(sprintf("Traitement de %d cellules...", n_cells))

  angles_arbre <- numeric(n_cells)

  for (i in seq_len(n_cells)) {
    dx <- xy_arbres[, 1] - xy_all[i, 1]
    dy <- xy_arbres[, 2] - xy_all[i, 2]
    dists <- sqrt(dx^2 + dy^2)
    idx_min <- which.min(dists)
    if (length(idx_min) > 0) {
      angles_arbre[i] <- atan2(dy[idx_min], dx[idx_min])
    }
  }

  diff_angles <- angles_arbre - angle_vent_rad
  diff_angles <- atan2(sin(diff_angles), cos(diff_angles))
  diff_deg <- diff_angles * 180 / pi

  seuil <- ouverture_angulaire / 2
  classification <- rep(0, n_cells)
  classification[abs(diff_deg) <= seuil] <- -1
  classification[abs(diff_deg) >= (180 - seuil)] <- 1

  # Créer les rasters
  dist_vals <- terra::values(dist_raster)

  dist_amont <- terra::rast(dist_raster)
  terra::values(dist_amont) <- ifelse(classification == -1, dist_vals, NA)

  dist_aval <- terra::rast(dist_raster)
  terra::values(dist_aval) <- ifelse(classification == 1, dist_vals, NA)

  dist_totale <- dist_raster

  # Crop final
  message("Crop final...")
  ext_crop <- terra::ext(xmin, xmax, ymin, ymax)
  dist_totale <- terra::crop(dist_totale, ext_crop)
  dist_amont <- terra::crop(dist_amont, ext_crop)
  dist_aval <- terra::crop(dist_aval, ext_crop)

  message("Terminé!")

  list(
    distance_totale = dist_totale,
    distance_amont = dist_amont,
    distance_aval = dist_aval,
    angle_vent = angle_vent,
    ouverture_angulaire = ouverture_angulaire,
    crs_projection = crs_ref$input
  )
}

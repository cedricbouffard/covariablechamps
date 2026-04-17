#' Calculer les distances amont/aval avec buffer et lissage
#'
#' @param arbres_sf Objet sf POINT avec les arbres
#' @param angle_vent Angle du vent en degrés (0=Nord, 90=Est)
#' @param champ_bbox Objet sf avec le contour du champ
#' @param resolution Résolution du raster en mètres
#' @param buffer_arbre Rayon du buffer autour des arbres (m)
#' @param angle_focal Angle focal en degrés
#' @param max_distance Distance maximale
#' @param taille_lissage Taille de la fenêtre de lissage (cellules)
#'
#' @return Liste avec les rasters amont et aval
#' @export
#'
calculer_distances_amont_aval <- function(arbres_sf,
                                         angle_vent,
                                         champ_bbox,
                                         resolution = 2,
                                         buffer_arbre = 3,
                                         angle_focal = 45,
                                         max_distance = 200,
                                         taille_lissage = 7) {

  if (!requireNamespace("terra", quietly = TRUE))
    stop("Le package 'terra' est requis.")
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Le package 'sf' est requis.")

  message(sprintf("Calcul distances amont/aval (buffer: %dm, angle focal: %d°)...", buffer_arbre, angle_focal))

  # CRS
  crs_arbres <- sf::st_crs(arbres_sf)
  crs_string <- crs_arbres$input

  if (is.na(crs_string)) stop("CRS manquant")

  # Reprojeter le champ
  crs_champ <- sf::st_crs(champ_bbox)
  if (!is.na(crs_champ) && crs_champ$input != crs_string) {
    champ_bbox <- sf::st_transform(champ_bbox, crs_arbres)
  }

  # Bounding box
  bbox <- sf::st_bbox(champ_bbox)
  xmin <- as.numeric(bbox["xmin"])
  ymin <- as.numeric(bbox["ymin"])
  xmax <- as.numeric(bbox["xmax"])
  ymax <- as.numeric(bbox["ymax"])

  # Étendre
  xmin_et <- xmin - max_distance
  ymin_et <- ymin - max_distance
  xmax_et <- xmax + max_distance
  ymax_et <- ymax + max_distance

  # Template raster
  r_template <- terra::rast(
    ncols = ceiling((xmax_et - xmin_et) / resolution),
    nrows = ceiling((ymax_et - ymin_et) / resolution),
    xmin = xmin_et, xmax = xmax_et,
    ymin = ymin_et, ymax = ymax_et
  )
  terra::crs(r_template) <- crs_string

  # Buffer et DISSOLVE (union de tous les buffers)
  message("Création du buffer dissous...")
  arbres_buffer <- sf::st_buffer(arbres_sf, dist = buffer_arbre)

  # Dissoudre tous les buffers en un seul polygone
  arbres_buffer_dissous <- sf::st_union(arbres_buffer)

  # Si union retourne une géométrie multi-polygone, la convertir
  if (inherits(arbres_buffer_dissous, "sfc_MULTIPOLYGON")) {
    arbres_buffer_dissous <- sf::st_cast(arbres_buffer_dissous, "POLYGON")
  }

  message(sprintf("Buffer dissous créé"))

  # Rasteriser le buffer dissous
  vect_buffer <- terra::vect(arbres_buffer_dissous)
  buffer_raster <- terra::rasterize(vect_buffer, r_template, field = 1, background = NA)

  # Coordonnées des centroïdes d'arbres (POINTS originaux)
  xy_arbres <- sf::st_coordinates(arbres_sf)
  n_arbres <- nrow(xy_arbres)
  message(sprintf("%d arbres", n_arbres))

  # Toutes les cellules
  xy_cells <- terra::xyFromCell(r_template, 1:terra::ncell(r_template))
  n_cells <- nrow(xy_cells)

  # Angles du vent
  theta_wind <- angle_vent * pi / 180
  half_angle <- (angle_focal / 2) * pi / 180

  message("Calcul des distances...")

  # Initialiser
  dist_amont <- numeric(n_cells)
  dist_aval <- numeric(n_cells)

  for (i in seq_len(n_cells)) {
    x <- xy_cells[i, 1]
    y <- xy_cells[i, 2]

    # Vecteur de la cellule vers chaque arbre
    dx <- xy_arbres[, 1] - x
    dy <- xy_arbres[, 2] - y
    dists <- sqrt(dx^2 + dy^2)

    # Angle de l'arbre par rapport à la cellule
    angles <- atan2(dy, dx)

    # Différence avec la direction du vent
    diff_angles <- angles - theta_wind
    diff_angles <- atan2(sin(diff_angles), cos(diff_angles))

    # AMONT: vent vient DE l'arbre (arbres dans la direction du vent)
    idx_amont <- which(abs(diff_angles) <= half_angle)
    if (length(idx_amont) > 0) {
      dist_amont[i] <- min(dists[idx_amont])
    }

    # AVAL: vent va VERS l'arbre (arbres dans la direction opposée)
    idx_aval <- which(abs(abs(diff_angles) - pi) <= half_angle)
    if (length(idx_aval) > 0) {
      dist_aval[i] <- min(dists[idx_aval])
    }

    if (i %% 20000 == 0) {
      message(sprintf("  %.0f%%", i / n_cells * 100))
    }
  }

  # Valeurs par défaut
  dist_amont[dist_amont == 0] <- max_distance
  dist_aval[dist_aval == 0] <- max_distance

  # Rasters bruts
  raster_amont <- terra::rast(r_template)
  terra::values(raster_amont) <- dist_amont

  raster_aval <- terra::rast(r_template)
  terra::values(raster_aval) <- dist_aval

  # Lissage (focal mean)
  message("Lissage...")
  w <- matrix(1, nrow = taille_lissage, ncol = taille_lissage)
  w <- w / sum(w)

  amont_smooth <- terra::focal(raster_amont, w = w, na.rm = TRUE)
  aval_smooth <- terra::focal(raster_aval, w = w, na.rm = TRUE)

  # Distance totale
  dist_totale <- terra::ifel(raster_amont < raster_aval, raster_amont, raster_aval)
  dist_totale_smooth <- terra::focal(dist_totale, w = w, na.rm = TRUE)

  # Limiter
  amont_smooth <- terra::ifel(amont_smooth > max_distance, max_distance, amont_smooth)
  aval_smooth <- terra::ifel(aval_smooth > max_distance, max_distance, aval_smooth)
  dist_totale_smooth <- terra::ifel(dist_totale_smooth > max_distance, max_distance, dist_totale_smooth)

  # Crop au champ
  ext_crop <- terra::ext(xmin, xmax, ymin, ymax)
  amont_crop <- terra::crop(amont_smooth, ext_crop)
  aval_crop <- terra::crop(aval_smooth, ext_crop)
  total_crop <- terra::crop(dist_totale_smooth, ext_crop)
  buffer_crop <- terra::crop(buffer_raster, ext_crop)

  message("Terminé!")

  list(
    amont = amont_crop,
    aval = aval_crop,
    totale = total_crop,
    buffer = buffer_crop,
    angle_vent = angle_vent,
    angle_focal = angle_focal,
    buffer_arbre = buffer_arbre,
    max_distance = max_distance
  )
}


#' Visualiser les distances amont/aval
#'
#' @param result Résultat de calculer_distances_amont_aval()
#' @param type "amont", "aval", "totale" ou "comparaison"
#'
#' @return ggplot2
#' @export
#'
visualiser_distances_vent <- function(result,
                                       type = c("comparaison", "amont", "aval", "totale")) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Le package 'ggplot2' requis")

  type <- match.arg(type)

  if (type == "comparaison") {
    p1 <- viz_distance_vent(result, result$amont, "AMONT: distance DEPUIS l'arbre",
               c("#fff7fb", "#deebf7", "#c6dbef", "#6baed6", "#2171b5"))
    p2 <- viz_distance_vent(result, result$aval, "AVAL: distance VERS l'arbre",
               c("#fff5f0", "#fee0d2", "#fcbba1", "#fb6a4a", "#cb181d"))
    return(list(amont = p1, aval = p2))
  } else if (type == "amont") {
    viz_distance_vent(result, result$amont, "Distance amont (m)", c("#fff7fb", "#deebf7", "#6baed6", "#08519c"))
  } else if (type == "aval") {
    viz_distance_vent(result, result$aval, "Distance aval (m)", c("#fff5f0", "#fee0d2", "#fb6a4a", "#67000d"))
  } else {
    viz_distance_vent(result, result$totale, "Distance totale (m)", c("#f7f7f7", "#636363", "#252525"))
  }
}

viz_distance_vent <- function(result, r, title, colors) {
  df <- as.data.frame(r, xy = TRUE)
  names(df)[3] <- "value"
  max_val <- result$max_distance

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = value)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(colors = colors, name = "Distance (m)",
                                  limits = c(0, max_val), na.value = "transparent") +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title,
                  subtitle = paste0("Vent: ", round(result$angle_vent, 0), "° | Buffer: ", result$buffer_arbre, "m"),
                  x = "X (m)", y = "Y (m)") +
    ggplot2::theme(legend.position = "right")
}


#' Simuler la vitesse du vent
#'
#' @param result Résultat de calculer_distances_amont_aval()
#' @param vitesse_ref Vitesse de référence (m/s)
#' @param coef_amont Effet ralentissement amont
#' @param coef_aval Effet accélération aval
#'
#' @return Raster de vitesse
#' @export
#'
simuler_vitesse_vent <- function(result,
                                  vitesse_ref = 5,
                                  coef_amont = 0.5,
                                  coef_aval = 0.3) {

  amont <- terra::values(result$amont)
  aval <- terra::values(result$aval)
  max_d <- result$max_distance

  amont[is.na(amont)] <- max_d
  aval[is.na(aval)] <- max_d

  # AMONT: proche d'un arbre = vent ralenti
  v_amont <- vitesse_ref * (1 - coef_amont * (1 - amont / max_d))

  # AVAL: proche d'un arbre = vent accéléré après
  v_aval <- vitesse_ref * (1 + coef_aval * (1 - aval / max_d))

  vitesse <- (v_amont + v_aval) / 2
  vitesse <- pmax(vitesse_ref * 0.5, pmin(vitesse_ref * 1.5, vitesse))

  r <- terra::rast(result$amont)
  terra::values(r) <- vitesse

  list(vitesse = r, vitesse_ref = vitesse_ref, coef_amont = coef_amont, coef_aval = coef_aval)
}

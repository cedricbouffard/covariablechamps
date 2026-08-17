#' Calculer les zones de vent avec une spline pour chaque haie
#' 
#' Crée des zones de vent (demi-lunes) derrière chaque haie. Les zones sont construites
#' par facteurs de la hauteur H de la haie (ex: 1H, 2H, ...).
#' 
#' @param haies_rectangles Objet sf avec les haies rectangulaires (colonnes: x_center, y_center, hauteur_p95, etc.)
#' @param direction_vent Direction du vent en degrés (degrés géographiques, 0=Nord, 90=Est) (défaut: 225)
#' @param facteur_hauteur Vecteur des facteurs H à calculer (défaut: 1:40)
#' @param champ_centroid Centroïde du champ pour déterminer le sens de protection (aval/amont) (défaut: NULL)
#' @param n_points Nombre de points pour le spline de l'apex (défaut: 50)
#'
#' @return Un objet sf avec les polygones des zones de vent. Attributs:
#'   \itemize{
#'     \item cluster, n_arbres, hauteur_p95, largeur, longueur, angle_haie_deg
#'     \item facteur_h: multiple de la hauteur H pour cette zone
#'     \item direction_vent: direction utilisée
#'     \item distance_H: distance H calculée pour ce facteur
#'     \item orientation_protection: "amont" ou "aval" selon la position du champ
#'   }
#' @export
calculer_zones_vent_spline <- function(haies_rectangles, 
                                          direction_vent = 225,
                                          facteur_hauteur = 1:40,
                                          champ_centroid = NULL,
                                          n_points = 50) {
   
   if (is.null(haies_rectangles) || nrow(haies_rectangles) == 0) {
     warning("Aucune haie à traiter")
     return(NULL)
   }
   
   if (!requireNamespace("sf", quietly = TRUE))
     stop("Le package 'sf' est requis.")
   
   crs_haies <- sf::st_crs(haies_rectangles)
   angle_vent_rad <- (90 - direction_vent) * pi / 180 + pi
   
   if (!is.null(champ_centroid)) {
     if (is.vector(champ_centroid) && length(champ_centroid) == 2) {
       champ_centroid <- sf::st_point(champ_centroid)
     }
     if (!inherits(champ_centroid, "sf")) {
       champ_centroid <- sf::st_sf(geometry = sf::st_sfc(champ_centroid), crs = crs_haies)
     }
     centroid_coords <- sf::st_coordinates(champ_centroid)
   } else {
     centroid_coords <- NULL
   }
   
   create_demilune_spline <- function(x1, y1, x2, y2, x_apex, y_apex, n = 50) {
     x_ctrl <- c(x1, x_apex, x2)
     y_ctrl <- c(y1, y_apex, y2)
     t_ctrl <- c(0, 0.5, 1)
     t_interp <- seq(0, 1, length.out = n)
     x_spline <- stats::spline(t_ctrl, x_ctrl, xout = t_interp, method = "natural")$y
     y_spline <- stats::spline(t_ctrl, y_ctrl, xout = t_interp, method = "natural")$y
     n_base <- round(n / 2)
     t_base <- seq(0, 1, length.out = n_base)
     x_base <- x2 + t_base * (x1 - x2)
     y_base <- y2 + t_base * (y1 - y2)
     x_poly <- c(x_base[-length(x_base)], x_spline[-1])
     y_poly <- c(y_base[-length(y_base)], y_spline[-1])
     coords <- matrix(c(x_poly, y_poly), ncol = 2)
     coords <- rbind(coords, coords[1, ])
     sf::st_polygon(list(coords))
   }
   
   clusters <- unique(haies_rectangles$cluster)
   all_zones_list <- list()
   all_zones_data <- list()
   zone_counter <- 0
   
for (c in clusters) {
      haie <- haies_rectangles[haies_rectangles$cluster == c, ]
      haie_row <- haie[1, ]
      x_center <- haie_row$x_center
      y_center <- haie_row$y_center
      hauteur <- haie_row$hauteur_p95
      angle_haie_rad <- haie_row$angle_deg * pi / 180
      L <- haie_row$longueur
      
      half_L <- L / 2
      cos_haie <- cos(angle_haie_rad); sin_haie <- sin(angle_haie_rad)
      x1 <- x_center - half_L * cos_haie; y1 <- y_center - half_L * sin_haie
      x2 <- x_center + half_L * cos_haie; y2 <- y_center + half_L * sin_haie
      
      angle_vent_calc <- angle_vent_rad
      orientation_protection <- "aval"
      if (!is.null(centroid_coords)) {
        dx_champ <- centroid_coords[1] - x_center
        dy_champ <- centroid_coords[2] - y_center
        angle_champ <- (atan2(dy_champ, dx_champ) * 180 / pi + 360) %% 360
        diff_angles <- abs(angle_champ - direction_vent)
        if (diff_angles > 180) diff_angles <- 360 - diff_angles
        if (diff_angles >= 90) {
          angle_vent_calc <- angle_vent_rad + pi
          orientation_protection <- "amont"
        }
      }
      
      cos_vent <- cos(angle_vent_calc); sin_vent <- sin(angle_vent_calc)
      
      for (facteur in facteur_hauteur) {
        distance_H <- hauteur * facteur
        x_apex <- x_center + distance_H * cos_vent
        y_apex <- y_center + distance_H * sin_vent
        zone_geom <- create_demilune_spline(x1, y1, x2, y2, x_apex, y_apex, n = n_points)
        zone_counter <- zone_counter + 1
        all_zones_list[[zone_counter]] <- zone_geom
        all_zones_data[[zone_counter]] <- data.frame(
          cluster = c,
          n_arbres = haie_row$n_arbres,
          hauteur_p95 = hauteur,
          largeur = haie_row$largeur,
          longueur = L,
          angle_haie_deg = haie_row$angle_deg,
          facteur_h = facteur,
          direction_vent = direction_vent,
          distance_H = distance_H,
          orientation_protection = orientation_protection,
          stringsAsFactors = FALSE
        )
      }
    }
   
   zones_sf <- sf::st_sf(dplyr::bind_rows(all_zones_data), 
                         geometry = sf::st_sfc(all_zones_list, crs = crs_haies))
   return(zones_sf)
}

#' Fusionner et disjoindre les zones de vent par facteur H
#' 
#' Prend les zones brutes et retourne une couche où chaque facteur H est fusionné
#' et rendu disjoint des facteurs plus petits (effet de couronnes).
#' 
#' @param zones_sf Résultat de calculer_zones_vent_spline
#' @return Un objet sf avec une ligne par facteur H, géométries fusionnées
#' @export
fusionner_zones_vent <- function(zones_sf) {
  if (is.null(zones_sf) || nrow(zones_sf) == 0) return(NULL)
  
  message("Fusion globale et disjonction par facteur H...")
  crs_orig <- sf::st_crs(zones_sf)
  h_vals <- sort(unique(zones_sf$facteur_h))
  
  result_list <- list()
  area_cumul <- NULL
  
  for (h in h_vals) {
    # 1. Union de toutes les haies pour ce facteur H
    h_union <- sf::st_union(zones_sf[zones_sf$facteur_h == h, ])
    
    # 2. Soustraire le cumul précédent
    if (!is.null(area_cumul)) {
      h_disjoint <- sf::st_difference(h_union, area_cumul)
    } else {
      h_disjoint <- h_union
    }
    
    # 3. Mettre à jour le cumul
    if (is.null(area_cumul)) {
      area_cumul <- h_union
    } else {
      area_cumul <- sf::st_union(area_cumul, h_union)
    }
    
    if (!sf::st_is_empty(h_disjoint)) {
      result_list[[length(result_list) + 1]] <- sf::st_sf(
        facteur_h = h,
        geometry = sf::st_sfc(h_disjoint, crs = crs_orig)
      )
    }
  }
  
  return(dplyr::bind_rows(result_list))
}

#' Calculer les distances amont/aval avec lissage
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
calculer_distances_vent <- function(arbres_sf,
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
  xmin <- as.numeric(bbox["xmin"]); ymin <- as.numeric(bbox["ymin"])
  xmax <- as.numeric(bbox["xmax"]); ymax <- as.numeric(bbox["ymax"])

  # Étendre
  xmin_et <- xmin - max_distance; ymin_et <- ymin - max_distance
  xmax_et <- xmax + max_distance; ymax_et <- ymax + max_distance

  # Template raster
  r_template <- terra::rast(
    ncols = ceiling((xmax_et - xmin_et) / resolution),
    nrows = ceiling((ymax_et - ymin_et) / resolution),
    xmin = xmin_et, xmax = xmax_et,
    ymin = ymin_et, ymax = ymax_et
  )
  terra::crs(r_template) <- crs_string

  # Buffer et DISSOLVE
  arbres_buffer <- sf::st_buffer(arbres_sf, dist = buffer_arbre)
  arbres_buffer_dissous <- sf::st_union(arbres_buffer)
  if (inherits(arbres_buffer_dissous, "sfc_MULTIPOLYGON")) {
    arbres_buffer_dissous <- sf::st_cast(arbres_buffer_dissous, "POLYGON")
  }

  # Rasteriser
  vect_buffer <- terra::vect(arbres_buffer_dissous)
  buffer_raster <- terra::rasterize(vect_buffer, r_template, field = 1, background = NA)

  # Coordonnées
  xy_arbres <- sf::st_coordinates(arbres_sf)
  xy_cells <- terra::xyFromCell(r_template, 1:terra::ncell(r_template))

  # Angles
  theta_wind <- angle_vent * pi / 180
  half_angle <- (angle_focal / 2) * pi / 180

  dist_amont <- numeric(nrow(xy_cells)); dist_aval <- numeric(nrow(xy_cells))

  for (i in seq_len(nrow(xy_cells))) {
    x <- xy_cells[i, 1]; y <- xy_cells[i, 2]
    dx <- xy_arbres[, 1] - x; dy <- xy_arbres[, 2] - y
    dists <- sqrt(dx^2 + dy^2)
    angles <- atan2(dy, dx)
    diff_angles <- angles - theta_wind
    diff_angles <- atan2(sin(diff_angles), cos(diff_angles))

    idx_amont <- which(abs(diff_angles) <= half_angle)
    if (length(idx_amont) > 0) dist_amont[i] <- min(dists[idx_amont])
    idx_aval <- which(abs(abs(diff_angles) - pi) <= half_angle)
    if (length(idx_aval) > 0) dist_aval[i] <- min(dists[idx_aval])
    
    if (i %% 100000 == 0) message(sprintf("  %.0f%%", i / nrow(xy_cells) * 100))
  }

  dist_amont[dist_amont == 0] <- max_distance
  dist_aval[dist_aval == 0] <- max_distance

  raster_amont <- terra::rast(r_template); terra::values(raster_amont) <- dist_amont
  raster_aval <- terra::rast(r_template); terra::values(raster_aval) <- dist_aval

  # Lissage
  w <- matrix(1, nrow = taille_lissage, ncol = taille_lissage); w <- w / sum(w)
  amont_smooth <- terra::focal(raster_amont, w = w, na.rm = TRUE)
  aval_smooth <- terra::focal(raster_aval, w = w, na.rm = TRUE)
  dist_totale <- terra::ifel(raster_amont < raster_aval, raster_amont, raster_aval)
  dist_totale_smooth <- terra::focal(dist_totale, w = w, na.rm = TRUE)

  # Crop
  ext_crop <- terra::ext(xmin, xmax, ymin, ymax)
  list(
    amont = terra::crop(amont_smooth, ext_crop),
    aval = terra::crop(aval_smooth, ext_crop),
    totale = terra::crop(dist_totale_smooth, ext_crop),
    buffer = terra::crop(buffer_raster, ext_crop),
    angle_vent = angle_vent,
    angle_focal = angle_focal,
    buffer_arbre = buffer_arbre,
    max_distance = max_distance
  )
}

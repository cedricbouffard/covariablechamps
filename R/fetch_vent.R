#' Créer une carte de fetch de vent avec effet directionnel (elliptique)
#'
#' Crée une carte montrant la distance aux arbres dans la direction du vent,
#' avec un effet de buffer elliptique aligné avec le vent.
#'
#' @param arbres_sf Objet sf POINT avec les arbres
#' @param angle_vent Angle du vent en degrés (0=Nord, 90=Est)
#' @param champ_bbox Objet sf avec le contour du champ
#' @param resolution Résolution du raster en mètres
#' @param max_fetch Distance maximale
#' @param coef_ellipse Ratio d'élongation (2 = 2x plus long dans la direction du vent)
#'
#' @return Liste avec les rasters
#' @export
#'
calculer_fetch_vent <- function(arbres_sf,
                                angle_vent,
                                champ_bbox,
                                resolution = 2,
                                max_fetch = 200,
                                coef_ellipse = 3) {

  if (!requireNamespace("terra", quietly = TRUE))
    stop("Le package 'terra' est requis.")
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Le package 'sf' est requis.")

  message("Calcul du fetch de vent (elliptique)...")

  # CRS
  crs_arbres <- sf::st_crs(arbres_sf)
  crs_string <- crs_arbres$input

  if (is.na(crs_string)) {
    stop("CRS manquant")
  }

  # Reprojeter le champ si nécessaire
  crs_champ <- sf::st_crs(champ_bbox)
  if (!is.na(crs_champ) && crs_champ$input != crs_string) {
    champ_bbox <- sf::st_transform(champ_bbox, crs_arbres)
  }

  # Buffer autour des arbres (effet de couronne)
  message("Création du buffer autour des arbres...")
  arbres_buffer <- sf::st_buffer(arbres_sf, dist = resolution * 1.5)

  # Rasteriser le buffer
  bbox <- sf::st_bbox(champ_bbox)
  xmin <- as.numeric(bbox["xmin"])
  ymin <- as.numeric(bbox["ymin"])
  xmax <- as.numeric(bbox["xmax"])
  ymax <- as.numeric(bbox["ymax"])

  # Étendre
  xmin_et <- xmin - max_fetch
  ymin_et <- ymin - max_fetch
  xmax_et <- xmax + max_fetch
  ymax_et <- ymax + max_fetch

  # Template raster
  r_template <- terra::rast(
    ncols = ceiling((xmax_et - xmin_et) / resolution),
    nrows = ceiling((ymax_et - ymin_et) / resolution),
    xmin = xmin_et, xmax = xmax_et,
    ymin = ymin_et, ymax = ymax_et
  )
  terra::crs(r_template) <- crs_string

  # Rasteriser les buffers d'arbres
  message("Rasterisation...")
  vect_buffer <- terra::vect(arbres_buffer)
  buffer_raster <- terra::rasterize(vect_buffer, r_template, field = 1, background = NA)

  # Distance euclidienne simple (pour référence)
  message("Calcul des distances...")
  dist_simple <- terra::distance(buffer_raster, unit = "m")
  dist_simple <- terra::ifel(dist_simple > max_fetch, max_fetch, dist_simple)

  # Calculer la distance directionnelle (elliptique)
  message("Calcul directionnel...")

  # Coordonnées de tous les centres d'arbres
  xy_centers <- sf::st_coordinates(arbres_sf)

  # Angle du vent en radians (direction vers laquelle souffle le vent)
  # 0 = Nord, 90 = Est
  theta <- angle_vent * pi / 180

  # Rotation: transformer les coordonnées pour aligner avec le vent
  cos_t <- cos(theta)
  sin_t <- sin(theta)

  # Pour chaque cellule du raster
  xy_cells <- terra::xyFromCell(r_template, 1:terra::ncell(r_template))
  n_cells <- nrow(xy_cells)

  # Initialiser les distances
  dist_along_wind <- numeric(n_cells)
  dist_across_wind <- numeric(n_cells)

  for (i in seq_len(n_cells)) {
    x <- xy_cells[i, 1]
    y <- xy_cells[i, 2]

    # Rotation: coordonnées dans le système aligné avec le vent
    x_rot <- (x - xy_centers[, 1]) * cos_t + (y - xy_centers[, 2]) * sin_t
    y_rot <- -(x - xy_centers[, 1]) * sin_t + (y - xy_centers[, 2]) * cos_t

    # Distance le long du vent (x_rot) et perpendiculaire (y_rot)
    # Distance "elliptique" : combine les deux avec le coef
    dists <- sqrt((x_rot / coef_ellipse)^2 + y_rot^2)

    # Prendre la distance minimale
    dist_along_wind[i] <- min(dists)
  }

  # Créer le raster de distance directionnelle
  dist_dir <- terra::rast(r_template)
  terra::values(dist_dir) <- dist_along_wind

  # Limiter
  dist_dir <- terra::ifel(dist_dir > max_fetch, max_fetch, dist_dir)

  # Crop au champ
  ext_crop <- terra::ext(xmin, xmax, ymin, ymax)
  dist_simple_crop <- terra::crop(dist_simple, ext_crop)
  dist_dir_crop <- terra::crop(dist_dir, ext_crop)
  buffer_crop <- terra::crop(buffer_raster, ext_crop)

  message("Terminé!")

  list(
    distance_simple = dist_simple_crop,
    distance_elliptique = dist_dir_crop,
    buffer_arbres = buffer_crop,
    angle_vent = angle_vent,
    coef_ellipse = coef_ellipse,
    max_fetch = max_fetch
  )
}


#' Visualiser le fetch de vent
#'
#' @param fetch Résultat de calculer_fetch_vent()
#' @param type "simple", "elliptique", ou "comparaison"
#' @param titre Titre optionnel
#'
#' @return ggplot2
#' @export
#'
visualiser_fetch <- function(fetch,
                             type = c("elliptique", "simple", "comparaison"),
                             titre = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Le package 'ggplot2' est requis.")

  type <- match.arg(type)

  if (type == "simple") {
    r <- fetch$distance_simple
    if (is.null(titre)) titre <- paste0("Distance simple (max ", fetch$max_fetch, "m)")
    colors <- c("#1a1a1a", "#3d3d3d", "#5f5f5f", "#818181", "#a3a3a3", "#c5c5c5", "#e7e7e7")
  } else if (type == "comparaison") {
    # Créer une comparaison côte à côte
    p1 <- visualiser_fetch(fetch, type = "simple", titre = "Distance simple (cercle)")
    p2 <- visualiser_fetch(fetch, type = "elliptique", titre = "Distance directionnelle (ellipse)")
    return(list(simple = p1, elliptique = p2))
  } else {
    r <- fetch$distance_elliptique
    if (is.null(titre)) {
      titre <- paste0("Fetch vent: ", round(fetch$angle_vent, 0), "° (ellipse x", fetch$coef_ellipse, ")")
    }
    # Palette style "hillshade" avec direction
    colors <- c("#000000", "#2d1b00", "#5c3a00", "#8c5a00", "#bc7a00",
                "#ec9a00", "#ffba40", "#ffda80", "#fff0b0", "#ffffe0")
  }

  df <- as.data.frame(r, xy = TRUE)
  names(df)[3] <- "value"

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = value)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(colors = colors, name = "Distance (m)") +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = titre, x = "X (m)", y = "Y (m)")
}


#' Simuler la vitesse du vent avec effet de fetch
#'
#' @param fetch Résultat de calculer_fetch_vent()
#' @param vitesse_ref Vitesse de référence (m/s)
#' @param coef_acceleration Coefficient d'accélération après les arbres
#'
#' @return Raster de vitesse
#' @export
#'
simuler_vitesse_vent_fetch <- function(fetch,
                                   vitesse_ref = 5,
                                   coef_acceleration = 0.3) {

  # Distance directionnelle
  dist <- terra::values(fetch$distance_elliptique)
  dist[is.na(dist)] <- fetch$max_fetch

  # Modèle simple: la vitesse augmente avec la distance depuis l'arbre
  # Dans la direction du vent (distance positive), la vitesse augmente
  vitesse <- vitesse_ref * (1 + coef_acceleration * (dist / fetch$max_fetch))

  # Limiter
  vitesse <- pmin(vitesse, vitesse_ref * 2)

  r <- terra::rast(fetch$distance_elliptique)
  terra::values(r) <- vitesse

  list(
    vitesse = r,
    vitesse_ref = vitesse_ref,
    coef = coef_acceleration
  )
}

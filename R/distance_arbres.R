#' Calculer la distance aux arbres avec buffer
#'
#' Calcule pour chaque cellule la distance euclidienne au buffer
#' autour des arbres les plus proches.
#'
#' @param arbres_sf Objet sf POINT avec les arbres
#' @param champ_bbox Objet sf avec le contour du champ
#' @param resolution Résolution du raster en mètres
#' @param buffer_arbre Rayon du buffer autour des arbres (m)
#' @param max_distance Distance maximale à calculer
#' @param taille_lissage Taille de la fenêtre de lissage (cellules)
#'
#' @return Liste avec les rasters
#' @export
#'
calculer_distance_arbres <- function(arbres_sf,
                                    champ_bbox,
                                    resolution = 2,
                                    buffer_arbre = 3,
                                    max_distance = 200,
                                    taille_lissage = 5) {

  if (!requireNamespace("terra", quietly = TRUE))
    stop("Le package 'terra' est requis.")
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Le package 'sf' est requis.")

  message(sprintf("Calcul distance aux arbres (buffer: %dm)...", buffer_arbre))

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

  # Buffer dissous autour des arbres
  message("Création du buffer...")
  arbres_buffer <- sf::st_buffer(arbres_sf, dist = buffer_arbre)
  buffer_dissous <- sf::st_union(arbres_buffer)

  # Rasteriser le buffer (1 = dans le buffer, NA = hors du buffer)
  vect_buffer <- terra::vect(buffer_dissous)
  buffer_raster <- terra::rasterize(vect_buffer, r_template, field = 1, background = NA)

  # Calculer la distance au buffer le plus proche
  message("Calcul des distances euclidiennes...")
  dist_raster <- terra::distance(buffer_raster, unit = "m")

  # Distance euclidienne simple (sans buffer)
  # Rasteriser les points d'arbres
  vect_arbres <- terra::vect(arbres_sf)
  arbres_points_raster <- terra::rasterize(vect_arbres, r_template, field = 1, background = NA)
  dist_points <- terra::distance(arbres_points_raster, unit = "m")

  # Lissage (optionnel)
  if (taille_lissage > 1) {
    message("Lissage...")
    w <- matrix(1, nrow = taille_lissage, ncol = taille_lissage)
    w <- w / sum(w)
    dist_raster <- terra::focal(dist_raster, w = w, na.rm = TRUE)
    dist_points <- terra::focal(dist_points, w = w, na.rm = TRUE)
  }

  # Limiter
  dist_raster <- terra::ifel(dist_raster > max_distance, max_distance, dist_raster)
  dist_points <- terra::ifel(dist_points > max_distance, max_distance, dist_points)

  # Crop au champ
  ext_crop <- terra::ext(xmin, xmax, ymin, ymax)
  dist_buffer_crop <- terra::crop(dist_raster, ext_crop)
  dist_points_crop <- terra::crop(dist_points, ext_crop)
  buffer_crop <- terra::crop(buffer_raster, ext_crop)

  message("Terminé!")

  list(
    distance_buffer = dist_buffer_crop,
    distance_points = dist_points_crop,
    buffer = buffer_crop,
    buffer_arbre = buffer_arbre,
    max_distance = max_distance
  )
}


viz_distance_arbres <- function(result, r, title, colors) {
  df <- as.data.frame(r, xy = TRUE)
  names(df)[3] <- "value"
  max_val <- result$max_distance

  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = value)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(colors = colors, name = "Distance (m)",
                                  limits = c(0, max_val), na.value = "transparent") +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title, x = "X (m)", y = "Y (m)") +
    ggplot2::theme(legend.position = "right")
}


#' Visualiser la distance aux arbres
#'
#' @param result Résultat de calculer_distance_arbres()
#' @param type "buffer", "points" ou "comparaison"
#' @param titre Titre optionnel
#'
#' @return ggplot2
#' @export
#'
visualiser_distance_arbres <- function(result,
                                       type = c("buffer", "points", "comparaison"),
                                       titre = NULL) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Le package 'ggplot2' requis")

  type <- match.arg(type)

  colors <- c("#000000", "#1a1200", "#332400", "#4d3600", "#664800",
              "#805a00", "#996c00", "#b37e00", "#cc9000", "#e6a200",
              "#ffb400", "#ffc640", "#ffd880", "#ffeab0", "#fff0d0")

  if (type == "comparaison") {
    p1 <- viz_distance_arbres(result, result$distance_points, "Distance aux POINTS d'arbres", colors)
    p2 <- viz_distance_arbres(result, result$distance_buffer, paste0("Distance au BUFFER (", result$buffer_arbre, "m)"), colors)
    return(list(points = p1, buffer = p2))
  } else if (type == "points") {
    r <- result$distance_points
    if (is.null(titre)) titre <- "Distance aux points d'arbres (m)"
  } else {
    r <- result$distance_buffer
    if (is.null(titre)) {
      titre <- paste0("Distance au buffer des arbres (", result$buffer_arbre, "m)")
    }
  }

  viz_distance_arbres(result, r, titre, colors)
}


#' Simuler la vitesse du vent basée sur la distance simple aux arbres
#'
#' Cette fonction est utilisée avec le résultat de \code{calculer_distance_arbres()}.
#' Pour les distances directionnelles (amont/aval), utilisez plutôt
#' \code{simuler_vitesse_vent()} qui prend le résultat de
#' \code{calculer_distances_amont_aval()}.
#'
#' @param dist_result Résultat de calculer_distance_arbres()
#' @param vitesse_ref Vitesse de référence (m/s)
#' @param coef_protection Coefficient de protection (0-1)
#'
#' @return Liste avec: vitesse (raster), vitesse_ref, coef_protection
#' @export
#'
simuler_vitesse_vent_simple <- function(dist_result,
                                        vitesse_ref = 5,
                                        coef_protection = 0.5) {

  dist <- terra::values(dist_result$distance_buffer)
  dist[is.na(dist)] <- dist_result$max_distance

  # Plus on est proche d'un arbre (buffer), plus le vent est ralenti
  # Vitesse minimale quand dans le buffer (dist = 0)
  v <- vitesse_ref * (1 - coef_protection * (1 - dist / dist_result$max_distance))

  # Limiter
  v <- pmax(vitesse_ref * 0.3, pmin(vitesse_ref, v))

  r <- terra::rast(dist_result$distance_buffer)
  terra::values(r) <- v

  list(
    vitesse = r,
    vitesse_ref = vitesse_ref,
    coef_protection = coef_protection
  )
}


#' Récapitulatif des fonctions distance_arbres
#'
#' Ce module fournit des fonctions pour calculer et visualiser les distances aux arbres.
#'
#' \itemize{
#'   \item calculer_distance_arbres(): Distance euclidienne simple aux arbres
#'   \item calculer_distances_amont_aval(): Distance directionnelle (amont/aval)
#'   \item visualiser_distance_arbres(): Visualiser la distance simple
#'   \item visualiser_distances_vent(): Visualiser la distance directionnelle
#'   \item simuler_vitesse_vent_simple(): Simuler la vitesse du vent (distance simple)
#'   \item simuler_vitesse_vent(): Simuler la vitesse du vent (amont/aval)
#' }
#'
#' @name distance_arbres-package
#' @examples
#' \dontrun{
#' # Distance simple
#' dist <- calculer_distance_arbres(arbres, champ, buffer_arbre = 3)
#' visualiser_distance_arbres(dist, type = "buffer")
#'
#' # Vitesse du vent (distance simple)
#' vitesse <- simuler_vitesse_vent_simple(dist, vitesse_ref = 5, coef_protection = 0.5)
#'
#' # Distance directionnelle (amont/aval)
#' dist_dir <- calculer_distances_amont_aval(arbres, 245, champ, buffer_arbre = 3)
#' visualiser_distances_vent(dist_dir, type = "comparaison")
#'
#' # Vitesse du vent (direction du vent)
#' vitesse <- simuler_vitesse_vent(dist_dir, vitesse_ref = 5)
#' }
NULL

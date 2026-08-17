#' Calculer l'orientation principale d'un champ
#'
#' Calcule l'orientation principale (azimut) d'un champ agricole à partir de sa géométrie.
#' Utilise la bounding box orientée ou les moments d'inertie pour déterminer l'angle principal.
#'
#' @param polygone Un objet `sf` représentant le champ ou un chemin vers un fichier vectoriel
#' @param methode Méthode de calcul: "mbr" (Minimum Bounding Rectangle, défaut) ou "pca" (analyse en composantes principales)
#' @param unite Unité de sortie: "degres" (défaut) ou "radians"
#' @param orientation Référence d'orientation: "geographique" (0=Nord, 90=Est, défaut) ou "mathematique" (0=Est, 90=Nord)
#'
#' @return Une liste contenant:
#' \item{angle}{L'angle principal en degrés ou radians}
#' \item{angle_perpendiculaire}{L'angle perpendiculaire (±90°)}
#' \item{longueur}{La longueur selon l'axe principal}
#' \item{largeur}{La largeur selon l'axe perpendiculaire}
#' \item{rapport_aspect}{Le rapport longueur/largeur}
#' \item{geometry}{La géométrie de la bounding box orientée}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' champ <- sf::st_read("champ.shp")
#' orientation <- calculer_orientation_champ(champ)
#' print(paste("Orientation principale:", round(orientation$angle, 1), "degrés"))
#'
#' # Avec visualisation
#' plot(champ)
#' plot(orientation$geometry, add = TRUE, border = "red", lwd = 2)
#' }
#'
#' @importFrom sf st_read st_crs st_transform st_bbox st_convex_hull st_coordinates st_as_sf st_union st_is_longlat st_centroid st_geometry st_cast st_linestring st_sfc st_sf st_polygon st_distance st_nearest_points
#' @importFrom methods is

# Détecter le code EPSG UTM approprié pour un objet sf
detecter_utm_epsg <- function(sf_obj) {
  # Si pas de CRS, on assume WGS84 par défaut
  if (is.na(sf::st_crs(sf_obj))) {
    sf::st_crs(sf_obj) <- 4326
  }
  
  # On travaille sur l'union pour avoir un centroïde global
  # Utiliser st_convex_hull pour plus de stabilité sur le centroïde
  poly_union <- sf::st_union(sf_obj)
  poly_hull <- sf::st_convex_hull(poly_union)
  centroid_sf <- sf::st_centroid(poly_hull)
  
  # Transformer en WGS84 pour calculer la zone UTM
  centroid_wgs84 <- sf::st_transform(centroid_sf, 4326)
  coords <- sf::st_coordinates(centroid_wgs84)
  
  lon <- coords[1, 1]
  lat <- coords[1, 2]
  
  # Calculer la zone UTM
  utm_zone <- floor((lon + 180) / 6) + 1
  epsg_code <- ifelse(lat >= 0, 32600 + utm_zone, 32700 + utm_zone)
  
  return(epsg_code)
}

#' Calculer l'orientation d'un champ
#'
#' Détermine l'orientation principale d'un polygone de champ selon la méthode
#' du Minimum Bounding Rectangle (MBR) ou de l'analyse en composantes principales (PCA).
#'
#' @param polygone Objet sf (polygone) ou chemin vers un fichier vectoriel
#' @param methode Méthode de calcul: "mbr" (défaut) ou "pca"
#' @param unite Unité de l'angle: "degres" (défaut) ou "radians"
#' @param orientation Référence d'orientation: "geographique" (0=Nord, 90=Est, défaut)
#'   ou "mathematique" (0=Est, 90=Nord)
#'
#' @return Une liste contenant:
#'   \itemize{
#'     \item angle: L'angle principal en degrés ou radians
#'     \item angle_perpendiculaire: L'angle perpendiculaire (±90°)
#'     \item longueur: La longueur selon l'axe principal
#'     \item largeur: La largeur selon l'axe perpendiculaire
#'     \item rapport_aspect: Le rapport longueur/largeur
#'     \item geometry: La géométrie de la bounding box orientée
#'   }
#'
#' @details
#' Le polygone est projeté en UTM (zone détectée automatiquement) pour garantir
#' des angles conformes et des dimensions métriques. Si le polygone n'a pas de
#' CRS, les coordonnées sont traitées comme des coordonnées planaires locales.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' champ <- sf::st_read("champ.shp")
#' orientation <- calculer_orientation_champ(champ)
#' print(paste("Orientation principale:", round(orientation$angle, 1), "degrés"))
#'
#' # Avec visualisation
#' plot(champ)
#' plot(orientation$geometry, add = TRUE, border = "red", lwd = 2)
#' }
#'
#' @importFrom sf st_read st_crs st_transform st_bbox st_convex_hull st_coordinates st_as_sf st_union st_is_longlat
#' @importFrom methods is
calculer_orientation_champ <- function(polygone, methode = "mbr", 
                                        unite = "degres", 
                                        orientation = "geographique") {
  
  # Lire le polygone si c'est un chemin de fichier
  if (is.character(polygone)) {
    polygone <- sf::st_read(polygone, quiet = TRUE)
  }
  
  # Vérifier que c'est bien un objet sf
  if (!methods::is(polygone, "sf")) {
    stop("Le polygone doit être un objet sf ou un chemin vers un fichier vectoriel")
  }
  
  # Mémoriser le CRS original
  crs_orig <- sf::st_crs(polygone)
  
  if (is.na(crs_orig)) {
    # Pas de CRS: travailler en coordonnées planaires locales (angles préservés)
    polygone_proj <- polygone
  } else {
    # Toujours projeter en UTM pour garantir des calculs métriques et des angles conformes
    epsg_utm <- detecter_utm_epsg(polygone)
    polygone_proj <- sf::st_transform(polygone, epsg_utm)
  }
  
  # Unir les géométries si plusieurs
  polygone_union <- sf::st_union(polygone_proj)
  
  if (methode == "mbr") {
    # Méthode: Minimum Bounding Rectangle (Rectangle englobant minimum)
    result <- calculer_mbr(polygone_union)
  } else if (methode == "pca") {
    # Méthode: Analyse en composantes principales
    result <- calculer_pca_orientation(polygone_union)
  } else {
    stop("Méthode non reconnue. Utilisez 'mbr' ou 'pca'")
  }
  
  # Appliquer la référence d'orientation
  if (orientation == "geographique") {
    # Convertir de mathématique (0=Est, 90=Nord) à géographique (0=Nord, 90=Est)
    # Azimuth = (90 - AngleMath) %% 180
    result$angle <- (90 - result$angle) %% 180
    result$angle_perpendiculaire <- (90 - result$angle_perpendiculaire) %% 180
  }
  
  # Convertir l'angle si nécessaire
  if (unite == "radians") {
    result$angle <- result$angle * pi / 180
    result$angle_perpendiculaire <- result$angle_perpendiculaire * pi / 180
  }
  
  # Reprojeter la géométrie dans le CRS original
  if (!is.null(result$geometry) && !is.na(crs_orig)) {
    result$geometry <- sf::st_transform(result$geometry, crs_orig)
  }
  
  return(result)
}

#' Créer la géométrie d'un rectangle orienté
#' @noRd
creer_geometrie_rect <- function(center, angle_math, longueur, largeur, crs) {
  angle_rad <- angle_math * pi / 180
  cos_a <- cos(angle_rad)
  sin_a <- sin(angle_rad)
  
  # Coins du rectangle centré sur (0,0)
  dx <- longueur / 2
  dy <- largeur / 2
  corners <- matrix(c(
    -dx, -dy,
    dx, -dy,
    dx, dy,
    -dx, dy,
    -dx, -dy
  ), ncol = 2, byrow = TRUE)
  
  # Rotation
  rot_matrix <- matrix(c(cos_a, -sin_a, sin_a, cos_a), nrow = 2)
  corners_rotated <- corners %*% rot_matrix
  
  # Translation vers le centre réel
  corners_final <- corners_rotated + matrix(center, nrow = 5, ncol = 2, byrow = TRUE)
  
  # Créer le polygon sf
  poly <- sf::st_polygon(list(corners_final))
  sfc <- sf::st_sfc(poly, crs = crs)
  return(sf::st_sf(geometry = sfc))
}

#' Calculer le Minimum Bounding Rectangle
#' @noRd
calculer_mbr <- function(polygone) {
  # Obtenir les coordonnées
  coords <- sf::st_coordinates(polygone)[, 1:2]
  
  # Calculer le centroïde des points pour centrer les calculs (stabilité numérique)
  centroid_pts <- colMeans(coords)
  coords_centered <- coords - matrix(centroid_pts, nrow = nrow(coords), ncol = 2, byrow = TRUE)
  
  # Tester différents angles pour trouver le rectangle d'aire minimale
  angles_test <- seq(0, 89.5, by = 0.5)
  
  meilleur_angle <- 0
  meilleure_aire <- Inf
  meilleures_dims <- c(0, 0)
  meilleur_center_rot <- c(0, 0)
  
  for (angle in angles_test) {
    # Rotation (mathématique)
    angle_rad <- angle * pi / 180
    # On utilise la transposée de la matrice de rotation pour tourner les points
    # ou on définit la matrice pour une rotation anti-horaire
    rot <- matrix(c(cos(angle_rad), sin(angle_rad), -sin(angle_rad), cos(angle_rad)), nrow = 2)
    coords_rotated <- coords_centered %*% rot
    
    # Calculer les dimensions et le centre du rectangle dans l'espace tourné
    r_x <- range(coords_rotated[, 1])
    r_y <- range(coords_rotated[, 2])
    x_range <- diff(r_x)
    y_range <- diff(r_y)
    aire <- x_range * y_range
    
    if (aire < meilleure_aire) {
      meilleure_aire <- aire
      meilleur_angle <- angle
      meilleures_dims <- c(x_range, y_range)
      meilleur_center_rot <- c(mean(r_x), mean(r_y))
    }
  }
  
  # Déterminer l'axe principal (le plus long)
  if (meilleures_dims[1] >= meilleures_dims[2]) {
    longueur <- meilleures_dims[1]
    largeur <- meilleures_dims[2]
    angle_math <- meilleur_angle
  } else {
    longueur <- meilleures_dims[2]
    largeur <- meilleures_dims[1]
    angle_math <- (meilleur_angle + 90) %% 180
  }
  
  # Calculer le centre du rectangle dans l'espace original
  # On doit inverser la rotation du meilleur_center_rot
  angle_rad_meilleur <- meilleur_angle * pi / 180
  rot_inv <- matrix(c(cos(angle_rad_meilleur), -sin(angle_rad_meilleur), 
                       sin(angle_rad_meilleur), cos(angle_rad_meilleur)), nrow = 2)
  rect_center_orig <- (matrix(meilleur_center_rot, nrow = 1) %*% rot_inv) + matrix(centroid_pts, nrow = 1)
  
  # Créer la géométrie
  mbr_sf <- creer_geometrie_rect(as.numeric(rect_center_orig), angle_math, longueur, largeur, sf::st_crs(polygone))
  
  list(
    angle = angle_math,
    angle_perpendiculaire = (angle_math + 90) %% 180,
    longueur = longueur,
    largeur = largeur,
    rapport_aspect = longueur / largeur,
    aire = meilleure_aire,
    geometry = mbr_sf,
    methode = "mbr"
  )
}

#' Calculer l'orientation par PCA
#' @noRd
calculer_pca_orientation <- function(polygone) {
  # Obtenir les coordonnées
  coords <- sf::st_coordinates(polygone)[, 1:2]
  
  # Centrer les données
  centroid_pts <- colMeans(coords)
  coords_centered <- scale(coords, center = centroid_pts, scale = FALSE)
  
  # Calculer la matrice de covariance
  cov_matrix <- cov(coords_centered)
  
  # Décomposition en valeurs propres
  eigen_decomp <- eigen(cov_matrix)
  
  # L'axe principal est celui avec la plus grande valeur propre
  pc1 <- eigen_decomp$vectors[, 1]
  pc2 <- eigen_decomp$vectors[, 2]
  
  # Calculer l'angle mathématique
  angle_math <- atan2(pc1[2], pc1[1]) * 180 / pi
  angle_math <- angle_math %% 180
  
  # Projeter pour obtenir les dimensions
  proj1 <- coords_centered %*% pc1
  proj2 <- coords_centered %*% pc2
  
  longueur <- diff(range(proj1))
  largeur <- diff(range(proj2))
  
  # Centre du rectangle dans l'espace des composantes
  c1 <- mean(range(proj1))
  c2 <- mean(range(proj2))
  
  # S'assurer que longueur >= largeur
  if (largeur > longueur) {
    temp <- longueur
    longueur <- largeur
    largeur <- temp
    angle_math <- (angle_math + 90) %% 180
    
    # Si on a pivoté de 90, le centre change aussi d'axe
    temp_c <- c1
    c1 <- c2
    c2 <- -temp_c # Rotation de 90 deg
  }
  
  # Reconvertir le centre en coordonnées originales
  # Coordonnées dans l'espace PCA vers espace centré
  # pts_centered = proj %*% t(vectors)
  pca_center_centered <- c(c1, c2) %*% t(eigen_decomp$vectors)
  rect_center_orig <- pca_center_centered + centroid_pts
  
  # Créer la géométrie
  pca_geom <- creer_geometrie_rect(as.numeric(rect_center_orig), angle_math, longueur, largeur, sf::st_crs(polygone))
  
  list(
    angle = angle_math,
    angle_perpendiculaire = (angle_math + 90) %% 180,
    longueur = longueur,
    largeur = largeur,
    rapport_aspect = longueur / largeur,
    variance_expliquee = eigen_decomp$values[1] / sum(eigen_decomp$values),
    geometry = pca_geom,
    methode = "pca"
  )
}

#' Visualiser l'orientation d'un champ
#'
#' Crée une carte montrant le champ avec sa bounding box orientée et l'angle principal
#'
#' @param polygone Un objet `sf` représentant le champ
#' @param orientation Résultat de `calculer_orientation_champ()` (optionnel)
#' @param afficher_angles Logique. Si TRUE, affiche les valeurs des angles sur la carte
#'
#' @return Un objet ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' champ <- sf::st_read("champ.shp")
#' visualiser_orientation(champ)
#' }
#'
#' @importFrom ggplot2 ggplot geom_sf labs theme_minimal annotate
#' @importFrom sf st_coordinates st_centroid st_union
visualiser_orientation <- function(polygone, orientation = NULL, afficher_angles = TRUE) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Le package 'ggplot2' est requis pour la visualisation")
  }
  
  # Calculer l'orientation si non fournie
  if (is.null(orientation)) {
    orientation <- calculer_orientation_champ(polygone, methode = "mbr")
  }
  
  # Créer le plot de base
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = polygone, fill = "lightgreen", color = "darkgreen", alpha = 0.5)
  
  # Ajouter la bounding box orientée si disponible
  if (!is.null(orientation$geometry)) {
    p <- p + ggplot2::geom_sf(data = orientation$geometry, 
                               fill = NA, color = "red", linewidth = 1.5, linetype = "dashed")
  }
  
  # Ajouter les angles
  if (afficher_angles) {
    centroid <- sf::st_coordinates(sf::st_centroid(sf::st_union(polygone)))
    
    p <- p + ggplot2::annotate("text", 
                               x = centroid[1], 
                               y = centroid[2] + 100,
                               label = paste("Angle:", round(orientation$angle, 1), "°"),
                               color = "red", fontface = "bold", size = 5) +
      ggplot2::annotate("text",
                        x = centroid[1],
                        y = centroid[2] - 100,
                        label = paste("Rapport L/l:", round(orientation$rapport_aspect, 2)),
                        color = "blue", fontface = "bold", size = 4)
  }
  
  p <- p + 
    ggplot2::labs(title = "Orientation du champ",
                  subtitle = paste("Méthode:", orientation$methode, 
                                   "| Angle principal:", round(orientation$angle, 1), "°")) +
    ggplot2::theme_minimal()
  
  return(p)
}


#' Calculer la distance aux bordures selon l'orientation du champ
#'
#' Calcule pour chaque point ou cellule la distance aux bordures du champ
#' en distinguant le sens de la longueur (axe principal) et le sens de la largeur
#' (axe perpendiculaire). Cette fonction est utile pour analyser l'effet de bordure
#' dans les parcelles agricoles, notamment pour:
#' 
#' - Quantifier l'influence des haies brise-vent selon leur orientation

#' - Étudier les gradients de rendement depuis les bordures
#' - Identifier les zones d'ombrage potentiel
#' - Planifier les prélèvements d'échantillons de sol
#'
#' @param points_sf Objet sf POINT ou SpatRaster avec les positions à évaluer.
#'   Si NULL (défaut), un raster est créé automatiquement sur l'emprise du champ.
#' @param champ_poly Objet sf POLYGON représentant le contour du champ.
#'   Requis. Doit avoir un CRS défini (projeté de préférence).
#' @param resolution Résolution du raster en mètres (défaut: 2).
#'   Utilisé uniquement si points_sf est NULL.
#' @param buffer Buffer autour du champ en mètres (défaut: 50).
#'   Définit la zone de calcul autour du champ.
#'
#' @return Une liste contenant:
#' \describe{
#'   \item{distance_long}{SpatRaster ou colonne sf: Distance (m) dans le sens 
#'     de la longueur du champ (axe principal). Représente la composante de la
#'     distance projetée sur l'axe le plus long du champ.}
#'   \item{distance_large}{SpatRaster ou colonne sf: Distance (m) dans le sens 
#'     de la largeur du champ (axe perpendiculaire). Représente la composante
#'     projetée sur l'axe le plus court.}
#'   \item{distance_min}{SpatRaster ou colonne sf: Distance euclidienne minimale
#'     (m) à n'importe quelle bordure du champ.}
#'   \item{orientation}{Liste: Résultat de \code{\link{calculer_orientation_champ}},
#'     contenant l'angle principal, les dimensions et la géométrie du MBR.}
#'   \item{champ_buffer}{sf: Zone tampon autour du champ utilisée pour les calculs.}
#' }
#' 
#' En mode points (points_sf fourni), la sortie inclut \code{points} au lieu des
#' rasters distance_*, avec les distances comme colonnes.
#'
#' @details
#' ## Algorithme
#' 
#' La fonction procède en plusieurs étapes:
#' 
#' 1. **Orientation**: Calcule l'orientation principale du champ via le 
#'    Minimum Bounding Rectangle (MBR), déterminant l'axe long et l'axe large.
#' 
#' 2. **Extraction des bordures**: Convertit le polygone en lignes pour
#'    calculer les distances.
#' 

#' 3. **Calcul des distances**: Pour chaque point:
#'    - Trouve la bordure la plus proche (distance euclidienne)
#'    - Calcule le vecteur vers cette bordure
#'    - Projette ce vecteur sur les axes long et large
#' 
#' ## Interprétation des résultats
#' 
#' - **distance_long faible**: Point proche d'une bordure perpendiculaire
#'   à l'axe principal (extrémités courtes du champ)
#' - **distance_large faible**: Point proche d'une bordure parallèle
#'   à l'axe principal (côtés longs du champ)
#' - Les deux distances ensemble permettent d'identifier la position
#'   relative dans le champ
#'
#' @seealso 
#' \code{\link{calculer_orientation_champ}} pour le calcul de l'orientation,
#' \code{\link{classifier_distances_bordures}} pour classifier les distances,
#' \code{\link{visualiser_distances_bordures}} pour la visualisation
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(terra)
#' 
#' # Charger un champ
#' champ <- st_read("mon_champ.shp")
#' 
#' # Mode raster: créer une grille sur le champ
#' dist_bordures <- calculer_distance_bordures_orientee(
#'   champ_poly = champ,
#'   resolution = 2,
#'   buffer = 100
#' )
#' 
#' # Visualiser les résultats
#' plot(dist_bordures$distance_long, main = "Distance sens long")
#' plot(dist_bordures$distance_large, main = "Distance sens large")
#' 
#' # Afficher l'orientation
#' cat("Angle principal:", dist_bordures$orientation$angle, "degrés\n")
#' cat("Dimensions:", dist_bordures$orientation$longueur, "x", 
#'     dist_bordures$orientation$largeur, "m\n")
#' 
#' # Mode points: calculer pour des positions spécifiques
#' points <- st_sample(champ, 100)
#' points <- st_as_sf(points)
#' dist_points <- calculer_distance_bordures_orientee(
#'   points_sf = points,
#'   champ_poly = champ
#' )
#' 
#' # Les distances sont dans le sf retourné
#' head(dist_points$points)
#' }
#'
#' @importFrom sf st_cast st_difference st_nearest_feature st_distance st_coordinates st_bbox st_buffer st_crs st_union
#' @importFrom terra rast rasterize as.points values crs project ext crop distance mask
#' @importFrom methods is
calculer_distance_bordures_orientee <- function(points_sf = NULL,
                                                 champ_poly = NULL,
                                                 resolution = 2,
                                                 buffer = 50) {
  
  if (is.null(champ_poly)) {
    stop("Le paramètre champ_poly est requis")
  }
  
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Le package 'sf' est requis.")
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Le package 'terra' est requis.")
  
  message("Calcul de l'orientation du champ...")
  
  # Toujours projeter le champ en UTM pour assurer des calculs de distance métriques corrects
  crs_orig <- sf::st_crs(champ_poly)
  epsg_utm <- NULL
  if (is.na(crs_orig)) {
    # Pas de CRS: travailler en coordonnées planaires locales (CRS factice pour les calculs)
    epsg_utm <- 32618
    champ_poly_proj <- sf::st_set_crs(champ_poly, epsg_utm)
  } else {
    epsg_utm <- detecter_utm_epsg(champ_poly)
    champ_poly_proj <- sf::st_transform(champ_poly, epsg_utm)
  }
  
  # Calculer l'orientation du champ (en mode mathématique pour les vecteurs de projection)
  orientation_math <- calculer_orientation_champ(champ_poly_proj, methode = "mbr", orientation = "mathematique")
  
  # Créer une version géographique pour l'affichage et le retour
  orientation <- orientation_math
  orientation$angle <- (90 - orientation_math$angle) %% 180
  orientation$angle_perpendiculaire <- (90 - orientation_math$angle_perpendiculaire) %% 180
  orientation$orientation <- "geographique"
  
  message(sprintf("Angle principal: %.1f° | Longueur: %.1fm | Largeur: %.1fm",
                  orientation$angle, orientation$longueur, orientation$largeur))
  
  # Extraire les axes (utiliser les angles mathématiques pour cos/sin)
  angle_principal_rad <- orientation_math$angle * pi / 180
  angle_perp_rad <- orientation_math$angle_perpendiculaire * pi / 180
  
  # Vecteurs directeurs des axes
  vecteur_long <- c(cos(angle_principal_rad), sin(angle_principal_rad))
  vecteur_large <- c(cos(angle_perp_rad), sin(angle_perp_rad))
  
  # Convertir le champ en ligne (bordures) sur la version projetée
  message("Extraction des bordures du champ...")
  bordures <- sf::st_cast(champ_poly_proj, "MULTILINESTRING")
  
  # Créer un buffer autour du champ pour les calculs (sur la version projetée)
  champ_buffer_proj <- sf::st_buffer(champ_poly_proj, dist = buffer)
  
  # Si pas de points fournis, travailler directement en mode raster (beaucoup plus rapide)
  if (is.null(points_sf)) {
    message(sprintf("Création d'un raster de résolution %.1fm...", resolution))
    
    # Bounding box avec buffer
    bbox <- sf::st_bbox(champ_buffer_proj)
    
    # Créer le raster template
    r_template <- terra::rast(
      ncols = ceiling((bbox["xmax"] - bbox["xmin"]) / resolution),
      nrows = ceiling((bbox["ymax"] - bbox["ymin"]) / resolution),
      xmin = bbox["xmin"], xmax = bbox["xmax"],
      ymin = bbox["ymin"], ymax = bbox["ymax"],
      crs = sf::st_crs(champ_poly_proj)$wkt
    )
    
    # Calculer la distance minimale globale
    message("Calcul de la distance minimale globale...")
    bordures_vect <- terra::vect(bordures)
    r_min <- terra::distance(r_template, bordures_vect)
    
    # Identifier les bordures selon leur orientation par rapport à l'axe principal
    message("Classification des segments de bordure...")
    bordures_lignes <- sf::st_cast(bordures, "LINESTRING")
    
    segments_transversaux <- list() # Perpendiculaires à l'axe long (extrémités)
    segments_longitudinaux <- list() # Parallèles à l'axe long (côtés)
    
    for (j in seq_len(length(bordures_lignes))) {
      ligne <- bordures_lignes[j]
      coords_b <- sf::st_coordinates(ligne)
      if (nrow(coords_b) < 2) next
      
      for (i in 1:(nrow(coords_b)-1)) {
        p1 <- coords_b[i, 1:2]
        p2 <- coords_b[i+1, 1:2]
        v_seg <- p2 - p1
        if (all(v_seg == 0)) next
        
        angle_seg <- (atan2(v_seg[2], v_seg[1]) * 180 / pi) %% 180
        diff_angle <- abs(angle_seg - orientation_math$angle) %% 180
        if (diff_angle > 90) diff_angle <- 180 - diff_angle
        
        seg <- sf::st_linestring(matrix(c(p1, p2), ncol = 2, byrow = TRUE))
        # Si diff_angle est proche de 90° (> 45°), le segment est transversal (extrémité)
        if (diff_angle > 45) {
          segments_transversaux[[length(segments_transversaux) + 1]] <- seg
        } else {
          # Sinon il est parallèle à la longueur (côté)
          segments_longitudinaux[[length(segments_longitudinaux) + 1]] <- seg
        }
      }
    }
    
    # Calcul des distances aux deux types de bordures
    message("Calcul des distances par type de bordure...")
    
    if (length(segments_transversaux) > 0) {
      b_trans_vect <- terra::vect(sf::st_sfc(segments_transversaux, crs = epsg_utm))
      r_long <- terra::distance(r_template, b_trans_vect)
    } else {
      r_long <- r_min
    }
    
    if (length(segments_longitudinaux) > 0) {
      b_long_vect <- terra::vect(sf::st_sfc(segments_longitudinaux, crs = epsg_utm))
      r_large <- terra::distance(r_template, b_long_vect)
    } else {
      r_large <- r_min
    }
    
    # Masquer par le champ original
    champ_vect <- terra::vect(champ_poly_proj)
    r_long <- terra::mask(r_long, champ_vect)
    r_large <- terra::mask(r_large, champ_vect)
    r_min <- terra::mask(r_min, champ_vect)
    
    message("Terminé!")
    
    if (is.na(crs_orig)) {
      # Pas de CRS: retourner les rasters sans CRS (CRS factice utilisé pour les calculs)
      terra::crs(r_long) <- ""
      terra::crs(r_large) <- ""
      terra::crs(r_min) <- ""
    }
    
    return(list(
      distance_long = r_long,
      distance_large = r_large,
      distance_min = r_min,
      orientation = orientation,
      champ_buffer = if (!is.na(crs_orig)) sf::st_transform(champ_buffer_proj, crs_orig) else sf::st_set_crs(champ_buffer_proj, NA)
    ))
    
  } else {
    # Mode points
    if (methods::is(points_sf, "SpatVector") || methods::is(points_sf, "SpatRaster")) {
      points_sf <- sf::st_as_sf(points_sf)
    }
    
    # Projeter les points dans le même UTM que le champ
    if (is.na(crs_orig)) {
      points_sf_proj <- sf::st_set_crs(points_sf, epsg_utm)
    } else {
      points_sf_proj <- sf::st_transform(points_sf, epsg_utm)
    }
    
    message(sprintf("Calcul pour %d points...", nrow(points_sf_proj)))
    
    # Même logique de classification des segments pour les points
    bordures_lignes <- sf::st_cast(bordures, "LINESTRING")
    segments_transversaux <- list()
    segments_longitudinaux <- list()
    
    for (j in seq_len(length(bordures_lignes))) {
      ligne <- bordures_lignes[j]
      coords_b <- sf::st_coordinates(ligne)
      if (nrow(coords_b) < 2) next
      for (i in 1:(nrow(coords_b)-1)) {
        p1 <- coords_b[i, 1:2]; p2 <- coords_b[i+1, 1:2]
        v_seg <- p2 - p1
        if (all(v_seg == 0)) next
        angle_seg <- (atan2(v_seg[2], v_seg[1]) * 180 / pi) %% 180
        diff_angle <- abs(angle_seg - orientation_math$angle) %% 180
        if (diff_angle > 90) diff_angle <- 180 - diff_angle
        seg <- sf::st_linestring(matrix(c(p1, p2), ncol = 2, byrow = TRUE))
        if (diff_angle > 45) {
          segments_transversaux[[length(segments_transversaux) + 1]] <- seg
        } else {
          segments_longitudinaux[[length(segments_longitudinaux) + 1]] <- seg
        }
      }
    }
    
    # Distances vectorisées
    points_sf_proj$distance_min <- as.numeric(sf::st_distance(points_sf_proj, sf::st_union(bordures)))
    
    if (length(segments_transversaux) > 0) {
      points_sf_proj$distance_long <- as.numeric(sf::st_distance(points_sf_proj, sf::st_union(sf::st_sfc(segments_transversaux, crs=epsg_utm))))
    } else {
      points_sf_proj$distance_long <- points_sf_proj$distance_min
    }
    
    if (length(segments_longitudinaux) > 0) {
      points_sf_proj$distance_large <- as.numeric(sf::st_distance(points_sf_proj, sf::st_union(sf::st_sfc(segments_longitudinaux, crs=epsg_utm))))
    } else {
      points_sf_proj$distance_large <- points_sf_proj$distance_min
    }
    
    message("Terminé!")
    
    return(list(
      points = if (!is.na(crs_orig)) sf::st_transform(points_sf_proj, crs_orig) else sf::st_set_crs(points_sf_proj, NA),
      orientation = orientation,
      champ_buffer = if (!is.na(crs_orig)) sf::st_transform(champ_buffer_proj, crs_orig) else sf::st_set_crs(champ_buffer_proj, NA)
    ))
  }
}


#' Visualiser les distances aux bordures orientées
#'
#' Crée une visualisation cartographique des distances aux bordures selon 
#' l'orientation du champ. Génère des cartes de chaleur montrant les gradients
#' de distance depuis les bordures du champ.
#'
#' @param dist_result Liste: Résultat de \code{\link{calculer_distance_bordures_orientee}}.
#'   Doit contenir les rasters distance_long, distance_large et distance_min.
#' @param type Character: Type de visualisation à produire.
#' \describe{
#'   \item{"long"}{Distance dans le sens de la longueur du champ}
#'   \item{"large"}{Distance dans le sens de la largeur du champ}
#'   \item{"min"}{Distance minimale à n'importe quelle bordure}
#'   \item{"comparaison"}{Les trois visualisations côte à côte}
#' }
#' @param titre Character: Titre personnalisé pour le graphique (optionnel).
#'   Si NULL, un titre automatique est généré selon le type.
#'
#' @return 
#' - Si type != "comparaison": Un objet ggplot2
#' - Si type == "comparaison": Une liste nommée avec trois ggplot2 
#'   (long, large, min) que vous pouvez arranger avec patchwork ou cowplot
#'
#' @details
#' La palette de couleurs utilisée va du bleu (distances faibles, proche des

#' bordures) au rouge (distances élevées, centre du champ).
#' 
#' Pour combiner les trois graphiques de "comparaison", utilisez:
#' \preformatted{
#' library(patchwork)
#' plots <- visualiser_distances_bordures(dist, "comparaison")
#' plots$long + plots$large + plots$min
#' }
#'
#' @seealso 
#' \code{\link{calculer_distance_bordures_orientee}} pour calculer les distances,
#' \code{\link{classifier_distances_bordures}} pour classifier les distances
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculer les distances
#' dist <- calculer_distance_bordures_orientee(champ_poly = champ)
#' 
#' # Visualiser une seule distance
#' visualiser_distances_bordures(dist, type = "long")
#' 
#' # Visualiser les trois
#' plots <- visualiser_distances_bordures(dist, type = "comparaison")
#' 
#' # Avec patchwork
#' library(patchwork)
#' plots$long + plots$large + plots$min + plot_layout(ncol = 3)
#' }
#'
#' @importFrom ggplot2 ggplot geom_raster scale_fill_gradientn labs theme_minimal coord_fixed theme geom_sf aes
#' @importFrom terra as.data.frame
visualiser_distances_bordures <- function(dist_result, 
                                           type = c("long", "large", "min", "comparaison"),
                                           titre = NULL) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Le package 'ggplot2' est requis pour la visualisation")
  }
  
  type <- match.arg(type)
  
  # Définir les couleurs (du proche au loin)
  colors <- c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0", 
              "#f7f7f7", "#fddbc7", "#f4a582", "#d6604d", "#b2182b")
  
  # Extraire les données selon le type
  if (type == "comparaison") {
    # Créer les trois plots
    p_long <- visualiser_distances_bordures(dist_result, type = "long")
    p_large <- visualiser_distances_bordures(dist_result, type = "large")
    p_min <- visualiser_distances_bordures(dist_result, type = "min")
    
    return(list(long = p_long, large = p_large, min = p_min))
  }
  
  # Sélectionner le raster approprié
  if (type == "long") {
    r <- dist_result$distance_long
    if (is.null(titre)) titre <- "Distance dans le sens de la LONGUEUR (m)"
    subtitle <- sprintf("Orientation: %.1f°", dist_result$orientation$angle)
  } else if (type == "large") {
    r <- dist_result$distance_large
    if (is.null(titre)) titre <- "Distance dans le sens de la LARGEUR (m)"
    subtitle <- sprintf("Perpendiculaire: %.1f°", dist_result$orientation$angle_perpendiculaire)
  } else {
    r <- dist_result$distance_min
    if (is.null(titre)) titre <- "Distance minimum aux bordures (m)"
    subtitle <- sprintf("Champ: %.1fm × %.1fm", 
                        dist_result$orientation$longueur,
                        dist_result$orientation$largeur)
  }
  
  # Convertir en data frame pour ggplot
  df <- terra::as.data.frame(r, xy = TRUE)
  names(df)[3] <- "value"
  
  # Créer le plot
  p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = value)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_gradientn(
      colors = colors,
      name = "Distance (m)",
      na.value = "transparent"
    ) +
    ggplot2::coord_fixed() +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      title = titre,
      subtitle = subtitle,
      x = "X (m)",
      y = "Y (m)"
    ) +
    ggplot2::theme(legend.position = "right")
  
  return(p)
}


#' Classifier les distances aux bordures
#'
#' Crée des classes de distance aux bordures du champ selon des seuils
#' personnalisables. Cette fonction permet de catégoriser les zones du champ
#' en fonction de leur proximité aux bordures, utile pour:
#' 
#' - Définir des zones de bordure vs zones centrales
#' - Stratifier les échantillonnages
#' - Identifier les zones d'influence des haies
#' - Créer des masques pour l'analyse
#'
#' @param dist_result Liste: Résultat de \code{\link{calculer_distance_bordures_orientee}}.
#' @param seuils_long Numeric vector: Seuils de classification (m) pour la 
#'   distance dans le sens long. Défaut: c(10, 25, 50, 100).
#' @param seuils_large Numeric vector: Seuils de classification (m) pour la
#'   distance dans le sens large. Défaut: c(5, 15, 30, 50).
#' @param labels_long Character vector: Noms des classes pour le sens long.
#'   Doit avoir length(seuils_long) + 1 éléments.
#'   Défaut: c("bordure_long", "proche_long", "intermediaire_long", 
#'   "eloigne_long", "centre_long").
#' @param labels_large Character vector: Noms des classes pour le sens large.
#'   Défaut: c("bordure_large", "proche_large", "intermediaire_large",
#'   "eloigne_large", "centre_large").
#'
#' @return Liste contenant:
#' \describe{
#'   \item{classe_long}{SpatRaster: Classification selon la distance sens long}
#'   \item{classe_large}{SpatRaster: Classification selon la distance sens large}
#'   \item{classe_combinee}{SpatRaster: Classification combinée (long * 10 + large)
#'     permettant d'identifier chaque combinaison unique}
#'   \item{seuils_long}{Les seuils utilisés pour le sens long}
#'   \item{seuils_large}{Les seuils utilisés pour le sens large}
#'   \item{labels_long}{Les labels utilisés pour le sens long}
#'   \item{labels_large}{Les labels utilisés pour le sens large}
#'   \item{table_classes}{Data.frame avec la correspondance des codes}
#' }
#'
#' @details
#' ## Interprétation des classes
#' 
#' Les classes sont numérotées de 1 (plus proche des bordures) à N (plus éloigné).
#' La classe combinée encode les deux informations: dizaines = classe_long,
#' unités = classe_large. Par exemple, 23 = classe 2 en long, classe 3 en large.
#' 
#' ## Choix des seuils
#' 
#' Les seuils par défaut sont adaptés à des champs agricoles typiques:
#' - Sens long: 10m (bordure immédiate), 25m (zone tampon), 50m (transition), 100m
#' - Sens large: 5m (bordure), 15m (tampon), 30m (transition), 50m
#' 
#' Adaptez ces seuils selon la taille de vos champs et l'effet étudié.
#'
#' @seealso 
#' \code{\link{calculer_distance_bordures_orientee}} pour calculer les distances,
#' \code{\link{visualiser_classes_bordures}} pour visualiser les classes
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculer les distances
#' dist <- calculer_distance_bordures_orientee(champ_poly = champ)
#' 
#' # Classifier avec les seuils par défaut
#' classes <- classifier_distances_bordures(dist)
#' 
#' # Visualiser
#' plot(classes$classe_long, main = "Classes sens long")
#' plot(classes$classe_large, main = "Classes sens large")
#' 
#' # Seuils personnalisés pour un petit champ
#' classes_petit <- classifier_distances_bordures(
#'   dist,
#'   seuils_long = c(5, 10, 20),
#'   seuils_large = c(3, 8, 15),
#'   labels_long = c("bord", "proche", "milieu", "centre"),
#'   labels_large = c("bord", "proche", "milieu", "centre")
#' )
#' }
#'
#' @importFrom terra classify values ifel
classifier_distances_bordures <- function(dist_result,
                                           seuils_long = c(10, 25, 50, 100),
                                           seuils_large = c(5, 15, 30, 50),
                                           labels_long = NULL,
                                           labels_large = NULL) {
  
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Le package 'terra' est requis.")
  
  # Vérifier que dist_result contient les rasters nécessaires
  if (is.null(dist_result$distance_long) || is.null(dist_result$distance_large)) {
    stop("dist_result doit contenir distance_long et distance_large")
  }
  
  # Labels par défaut
  if (is.null(labels_long)) {
    labels_long <- c("bordure_long", "proche_long", "intermediaire_long", 
                     "eloigne_long", "centre_long")
  }
  if (is.null(labels_large)) {
    labels_large <- c("bordure_large", "proche_large", "intermediaire_large",
                      "eloigne_large", "centre_large")
  }
  
  # Vérifier la cohérence des seuils et labels
  if (length(labels_long) != length(seuils_long) + 1) {
    stop("labels_long doit avoir length(seuils_long) + 1 éléments")
  }
  if (length(labels_large) != length(seuils_large) + 1) {
    stop("labels_large doit avoir length(seuils_large) + 1 éléments")
  }
  
  message("Classification des distances aux bordures...")
  message(sprintf("  Seuils long: %s m", paste(seuils_long, collapse = ", ")))
  message(sprintf("  Seuils large: %s m", paste(seuils_large, collapse = ", ")))
  
  # Créer les matrices de reclassification
  # Format: from, to, becomes
  # Utiliser -Inf comme borne inférieure pour capturer les valeurs 0
  n_long <- length(seuils_long)
  n_large <- length(seuils_large)
  
  # Matrice pour sens long
  rcl_long <- matrix(nrow = n_long + 1, ncol = 3)
  rcl_long[1, ] <- c(-Inf, seuils_long[1], 1)
  for (i in seq_len(n_long - 1)) {
    rcl_long[i + 1, ] <- c(seuils_long[i], seuils_long[i + 1], i + 1)
  }
  rcl_long[n_long + 1, ] <- c(seuils_long[n_long], Inf, n_long + 1)
  
  # Matrice pour sens large
  rcl_large <- matrix(nrow = n_large + 1, ncol = 3)
  rcl_large[1, ] <- c(-Inf, seuils_large[1], 1)
  for (i in seq_len(n_large - 1)) {
    rcl_large[i + 1, ] <- c(seuils_large[i], seuils_large[i + 1], i + 1)
  }
  rcl_large[n_large + 1, ] <- c(seuils_large[n_large], Inf, n_large + 1)
  
  # Classifier
  classe_long <- terra::classify(dist_result$distance_long, rcl_long)
  classe_large <- terra::classify(dist_result$distance_large, rcl_large)
  
  # Nommer les couches
  names(classe_long) <- "classe_long"
  names(classe_large) <- "classe_large"
  
  # Classe combinée
  classe_combinee <- classe_long * 10 + classe_large
  names(classe_combinee) <- "classe_combinee"
  
  # Créer la table de correspondance
  table_classes <- expand.grid(
    classe_long = seq_along(labels_long),
    classe_large = seq_along(labels_large)
  )
  table_classes$code_combine <- table_classes$classe_long * 10 + table_classes$classe_large
  table_classes$label_long <- labels_long[table_classes$classe_long]
  table_classes$label_large <- labels_large[table_classes$classe_large]
  table_classes$label_combine <- paste(table_classes$label_long, 
                                        table_classes$label_large, sep = " + ")
  
  message("Terminé!")
  message(sprintf("  %d classes dans le sens long", length(labels_long)))
  message(sprintf("  %d classes dans le sens large", length(labels_large)))
  message(sprintf("  %d combinaisons possibles", nrow(table_classes)))
  
  list(
    classe_long = classe_long,
    classe_large = classe_large,
    classe_combinee = classe_combinee,
    seuils_long = seuils_long,
    seuils_large = seuils_large,
    labels_long = labels_long,
    labels_large = labels_large,
    table_classes = table_classes
  )
}


#' Visualiser les classes de distance aux bordures
#'
#' Crée une visualisation cartographique des classes de distance aux bordures.
#' Utilise une palette de couleurs catégorielle pour distinguer les classes.
#'
#' @param classes_result Liste: Résultat de \code{\link{classifier_distances_bordures}}.
#' @param type Character: Type de visualisation.
#' \describe{
#'   \item{"long"}{Classes selon la distance sens long}
#'   \item{"large"}{Classes selon la distance sens large}
#'   \item{"combinee"}{Classes combinées (toutes les combinaisons)}
#' }
#' @param titre Character: Titre personnalisé (optionnel).
#'
#' @return Un objet ggplot2
#'
#' @seealso \code{\link{classifier_distances_bordures}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dist <- calculer_distance_bordures_orientee(champ_poly = champ)
#' classes <- classifier_distances_bordures(dist)
#' visualiser_classes_bordures(classes, type = "long")
#' }
#'
#' @importFrom ggplot2 ggplot geom_raster scale_fill_viridis_c scale_fill_brewer labs theme_minimal coord_fixed aes
#' @importFrom terra as.data.frame
visualiser_classes_bordures <- function(classes_result,
                                         type = c("long", "large", "combinee"),
                                         titre = NULL) {
  
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Le package 'ggplot2' est requis pour la visualisation")
  }
  
  type <- match.arg(type)
  
  # Sélectionner le raster et les labels
  if (type == "long") {
    r <- classes_result$classe_long
    labels <- classes_result$labels_long
    if (is.null(titre)) titre <- "Classes de distance - Sens LONG"
  } else if (type == "large") {
    r <- classes_result$classe_large
    labels <- classes_result$labels_large
    if (is.null(titre)) titre <- "Classes de distance - Sens LARGE"
  } else {
    r <- classes_result$classe_combinee
    labels <- NULL
    if (is.null(titre)) titre <- "Classes combinées (long + large)"
  }
  
  # Convertir en data frame
  df <- terra::as.data.frame(r, xy = TRUE)
  names(df)[3] <- "classe"
  
  # Créer le plot
  if (type != "combinee" && !is.null(labels)) {
    df$classe_label <- factor(df$classe, levels = seq_along(labels), labels = labels)
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = classe_label)) +
      ggplot2::geom_raster() +
      ggplot2::scale_fill_brewer(palette = "RdYlGn", direction = -1, 
                                  name = "Classe", na.value = "transparent") +
      ggplot2::coord_fixed() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = titre, x = "X (m)", y = "Y (m)")
  } else {
    p <- ggplot2::ggplot(df, ggplot2::aes(x = x, y = y, fill = classe)) +
      ggplot2::geom_raster() +
      ggplot2::scale_fill_viridis_c(name = "Code classe", na.value = "transparent") +
      ggplot2::coord_fixed() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = titre, x = "X (m)", y = "Y (m)")
  }
  
  return(p)
}

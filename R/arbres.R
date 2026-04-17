#' Extraire et classifier les arbres LiDAR autour d'un champ
#'
#' Pipeline complet:
#' 1) DTM (rasterize_terrain)
#' 2) Normalisation (Z - DTM)
#' 3) CHM (rasterize_canopy)
#' 4) Segmentation (lidaRtRee)
#' 5) Extraction arbres (lidaRtRee)
#' 6) Clustering + classification: foret / haie_brise_vent / individuel
#' 7) Export des rectangles des haies (largeur, longueur, angle)
#'
#' @param nuage_points Objet LAS (lidR) - typiquement l$nuage_points
#' @param res_dtm Resolution DTM (m)
#' @param res_chm Resolution CHM (m)
#' @param hmin Hauteur minimale (m) pour la segmentation / extraction
#' @param eps_dbscan Paramètre eps DBSCAN (m) pour regrouper les arbres en clusters
#' @param minPts_dbscan minPts DBSCAN
#' @param seuil_aspect Seuil L/W minimal pour haie (ex: 6)
#' @param seuil_largeur_max Largeur max (m) d'une haie (ex: 12)
#' @param seuil_linearity Linéarité PCA min pour haie (ex: 8)
#' @param seuil_largeur_foret Largeur (m) indicative de forêt (ex: 20)
#' @param seuil_n_foret Nombre d'arbres indicatif de forêt (ex: 30)
#'
#' @return Une liste:
#' - dtm: SpatRaster DTM
#' - chm: SpatRaster CHM
#' - segms: segmentation (lidaRtRee)
#' - trees: output brut lidaRtRee::tree_extraction
#' - trees_sf: sf points avec attributs + cluster + classe
#' - cluster_stats: table des métriques par cluster
#' - haies_rectangles: sf POLYGON avec les rectangles des haies (largeur, longueur, angle)
#'
#' @export
extraire_classifier_haies_lidar <- function(
  nuage_points,
  res_dtm = 1,
  res_chm = 0.25,
  hmin = 1,
  eps_dbscan = 6,
  minPts_dbscan = 3,
  seuil_aspect = 6,
  seuil_largeur_max = 12,
  seuil_linearity = 8,
  seuil_largeur_foret = 20,
  seuil_n_foret = 30
) {

  # ----------------------------
  # Checks
  # ----------------------------
  if (!requireNamespace("lidR", quietly = TRUE))
    stop("Le package 'lidR' est requis.")
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Le package 'terra' est requis.")
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Le package 'sf' est requis.")
  if (!requireNamespace("dbscan", quietly = TRUE))
    stop("Le package 'dbscan' est requis.")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Le package 'dplyr' est requis.")
  if (!requireNamespace("purrr", quietly = TRUE))
    stop("Le package 'purrr' est requis.")
  if (!requireNamespace("tibble", quietly = TRUE))
    stop("Le package 'tibble' est requis.")
  if (!requireNamespace("lidaRtRee", quietly = TRUE))
    stop("Le package 'lidaRtRee' est requis.")

  # ----------------------------
  # Helper: PCA-MBR avec rectangle et angle (stable, sans lwgeom)
  # ----------------------------
  compute_mbr_pca <- function(xy) {
    xy <- as.matrix(xy)
    storage.mode(xy) <- "double"
    xy <- xy[, 1:2, drop = FALSE]

    ok <- is.finite(xy[, 1]) & is.finite(xy[, 2])
    xy <- xy[ok, , drop = FALSE]

    if (nrow(xy) < 3) {
      return(tibble::tibble(
        L = NA_real_, W = NA_real_, aspect = NA_real_, linearity = NA_real_,
        angle_deg = NA_real_, x_center = NA_real_, y_center = NA_real_,
        rect_xmin = NA_real_, rect_xmax = NA_real_, rect_ymin = NA_real_, rect_ymax = NA_real_
      ))
    }

    pca <- stats::prcomp(xy, center = TRUE, scale. = FALSE)
    R <- pca$rotation[, 1:2, drop = FALSE]
    xy_rot <- xy %*% R

    dx <- diff(range(xy_rot[, 1]))
    dy <- diff(range(xy_rot[, 2]))

    L <- max(dx, dy)
    W <- min(dx, dy)

    # Calcul de l'angle (orientation de l'axe principal en degrés, 0-180)
    # L'axe principal est la première composante principale
    angle_rad <- atan2(R[2, 1], R[1, 1])
    angle_deg <- (angle_rad * 180 / pi) %% 180

    # Centre du cluster
    x_center <- mean(xy[, 1], na.rm = TRUE)
    y_center <- mean(xy[, 2], na.rm = TRUE)

    # Bounding box dans l'espace original
    rect_xmin <- min(xy[, 1], na.rm = TRUE)
    rect_xmax <- max(xy[, 1], na.rm = TRUE)
    rect_ymin <- min(xy[, 2], na.rm = TRUE)
    rect_ymax <- max(xy[, 2], na.rm = TRUE)

    tibble::tibble(
      L = L,
      W = W,
      aspect = L / max(W, 0.01),
      linearity = (pca$sdev[1]^2) / max(pca$sdev[2]^2, 1e-6),
      angle_deg = angle_deg,
      x_center = x_center,
      y_center = y_center,
      rect_xmin = rect_xmin,
      rect_xmax = rect_xmax,
      rect_ymin = rect_ymin,
      rect_ymax = rect_ymax
    )
  }

  # Helper: Créer un rectangle orienté pour une haie
  # ----------------------------
  create_oriented_rectangle <- function(x_center, y_center, L, W, angle_deg) {
    # CORRECTION: Ajouter 90 degrés pour aligner correctement avec les zones
    # car les zones utilisent l'angle comme direction de la longueur
    angle_deg <- angle_deg + 90
    angle_rad <- angle_deg * pi / 180

    # Demi-dimensions
    half_L <- L / 2
    half_W <- W / 2

    # Coins du rectangle non-orienté (centré à l'origine)
    # Le rectangle est initialement aligné avec la largeur suivant l'axe X
    # et la longueur suivant l'axe Y (après rotation de 90°)
    corners <- matrix(c(
      -half_W, -half_L,
       half_W, -half_L,
       half_W,  half_L,
      -half_W,  half_L,
      -half_W, -half_L  # Fermer le polygone
    ), ncol = 2, byrow = TRUE)

    # Matrice de rotation
    cos_a <- cos(angle_rad)
    sin_a <- sin(angle_rad)
    R_mat <- matrix(c(cos_a, -sin_a, sin_a, cos_a), ncol = 2, byrow = TRUE)

    # Rotation des coins
    corners_rot <- corners %*% R_mat

    # Translation au centre
    corners_rot[, 1] <- corners_rot[, 1] + x_center
    corners_rot[, 2] <- corners_rot[, 2] + y_center

    # Créer le polygone sf
    sf::st_polygon(list(corners_rot))
  }

  # ----------------------------
  # 1) DTM
  # ----------------------------
  message("Calcul du DTM...")
  dtm <- lidR::rasterize_terrain(
    nuage_points,
    res = res_dtm,
    algorithm = lidR::tin()
  )

  # ----------------------------
  # 2) Normalisation (Z - DTM)
  # ----------------------------
  message("Normalisation des hauteurs...")
  las_norm <- nuage_points - dtm

  # Option: filtrer les points aberrants après normalisation
  las_norm <- lidR::filter_poi(las_norm, Z >= -2 & Z <= 80)

  # ----------------------------
  # 3) CHM
  # ----------------------------
  message("Calcul du CHM...")
  chm <- lidR::rasterize_canopy(
    las_norm,
    res = res_chm,
    algorithm = lidR::p2r(),
    pkg = "terra"
  )

  # ----------------------------
  # 4) Segmentation
  # ----------------------------
  message("Segmentation des arbres...")
  segms <- lidaRtRee::tree_segmentation(chm, hmin = hmin)

  # ----------------------------
  # 5) Extraction des arbres
  # ----------------------------
  message("Extraction des arbres...")
  trees <- lidaRtRee::tree_extraction(segms)

  # Attendu: data.frame avec x,y, h, etc.
  if (!is.data.frame(trees) || nrow(trees) == 0) {
    stop("tree_extraction() n'a retourné aucun arbre.")
  }

  message("  ", nrow(trees), " arbres extraits")

  # ----------------------------
  # 6) Mise en sf (CRS du LAS)
  # ----------------------------
  crs_las <- try(sf::st_crs(nuage_points), silent = TRUE)
  # fallback si st_crs(LAS) n'est pas reconnu:
  if (inherits(crs_las, "try-error") || is.na(crs_las)) {
    # lidR::epsg(nuage_points) existe parfois selon versions
    epsg <- try(lidR::epsg(nuage_points), silent = TRUE)
    if (!inherits(eps, "try-error") && !is.na(epsg)) {
      crs_las <- sf::st_crs(epsg)
    } else {
      warning("CRS du LAS non détecté. trees_sf sera sans CRS.")
      crs_las <- NA
    }
  }

  # Assure qu'on a des colonnes x/y
  if (!all(c("x", "y") %in% names(trees))) {
    stop("La table 'trees' doit contenir des colonnes 'x' et 'y'.")
  }

  trees_sf <- sf::st_as_sf(
    trees,
    coords = c("x", "y"),
    crs = crs_las,
    remove = FALSE
  )

  # ----------------------------
  # 7) Clustering DBSCAN
  # ----------------------------
  message("Clustering des arbres...")
  coords <- sf::st_coordinates(trees_sf)
  cl <- dbscan::dbscan(coords, eps = eps_dbscan, minPts = minPts_dbscan)
  trees_sf$cluster <- cl$cluster  # 0 = isolé

  n_clusters <- length(unique(cl$cluster[cl$cluster > 0]))
  n_isoles <- sum(cl$cluster == 0)
  message("  ", n_clusters, " clusters trouvés, ", n_isoles, " arbres isolés")

  # ----------------------------
  # 8) Stats par cluster (PCA-MBR)
  # ----------------------------
  message("Calcul des métriques par cluster...")
  clusters <- sort(unique(trees_sf$cluster))
  clusters <- clusters[clusters != 0]

  if (length(clusters) > 0) {
    cluster_stats <- purrr::map_dfr(clusters, function(k) {
      pts_k <- dplyr::filter(trees_sf, cluster == k)
      xy <- sf::st_coordinates(pts_k)

      mbr <- compute_mbr_pca(xy)

      cluster_data <- tibble::tibble(
        cluster = k,
        n = nrow(pts_k),
        h_p95 = if ("h" %in% names(pts_k)) stats::quantile(pts_k$h, 0.95, na.rm = TRUE) else NA_real_
      )

      cbind(cluster_data, mbr)
    })

    # ----------------------------
    # 9) Classification des clusters
    # ----------------------------
    cluster_stats <- dplyr::mutate(
      cluster_stats,
      classe_cluster = dplyr::case_when(
        n < 4 ~ "petit_groupe",
        aspect >= seuil_aspect &
          W <= seuil_largeur_max &
          linearity >= seuil_linearity ~ "haie_brise_vent",
        W >= seuil_largeur_foret | n >= seuil_n_foret ~ "foret",
        TRUE ~ "incertain"
      )
    )

    # ----------------------------
    # 10) Classe finale par arbre
    # ----------------------------
    cluster_subset <- cluster_stats[, c("cluster", "classe_cluster"), drop = FALSE]
    trees_sf <- dplyr::left_join(trees_sf, cluster_subset, by = "cluster")
    trees_sf$classe <- ifelse(trees_sf$cluster == 0, "individuel", trees_sf$classe_cluster)
    
    # ----------------------------
    # 11) Création des rectangles des haies
    # ----------------------------
    # Vérifier que toutes les colonnes nécessaires existent et ne sont pas vides
    required_cols <- c("x_center", "y_center", "L", "W", "angle_deg", "classe_cluster")
    missing_cols <- setdiff(required_cols, names(cluster_stats))
    
    if (length(missing_cols) > 0) {
      warning("Colonnes manquantes dans cluster_stats: ", paste(missing_cols, collapse = ", "))
      haies_rectangles <- NULL
    } else {
      haies_stats <- dplyr::filter(cluster_stats, classe_cluster == "haie_brise_vent")
      
      # Vérifier qu'il y a des haies valides (sans NA dans les colonnes nécessaires)
      valid_haies <- stats::complete.cases(
        haies_stats[, c("x_center", "y_center", "L", "W", "angle_deg")]
      )
      haies_stats_valid <- haies_stats[valid_haies, ]
      
      if (nrow(haies_stats_valid) > 0) {
        message("Création des rectangles des haies (", nrow(haies_stats_valid), " trouvées)...")
        
        # Créer les rectangles orientés pour chaque haie
        rectangles_list <- purrr::pmap(
          list(
            haies_stats_valid$x_center,
            haies_stats_valid$y_center,
            haies_stats_valid$L,
            haies_stats_valid$W,
            haies_stats_valid$angle_deg
          ),
          create_oriented_rectangle
        )
        
        # Créer l'objet sf avec les attributs
        haies_rectangles <- sf::st_sf(
          cluster = haies_stats_valid$cluster,
          n_arbres = haies_stats_valid$n,
          hauteur_p95 = haies_stats_valid$h_p95,
          largeur = haies_stats_valid$W,
          longueur = haies_stats_valid$L,
          aspect = haies_stats_valid$aspect,
          linearite = haies_stats_valid$linearity,
          angle_deg = haies_stats_valid$angle_deg,
          x_center = haies_stats_valid$x_center,
          y_center = haies_stats_valid$y_center,
          classe = haies_stats_valid$classe_cluster,
          geometry = sf::st_sfc(rectangles_list, crs = crs_las)
        )
        
        message("  ✓ ", nrow(haies_rectangles), " rectangles de haies créés")
      } else {
        haies_rectangles <- NULL
        if (nrow(haies_stats) > 0) {
          message("  ", nrow(haies_stats), " haie(s) détectée(s) mais invalide(s) (données manquantes)")
        } else {
          message("  Aucune haie détectée")
        }
      }
    }
  } else {
    # Pas de clusters, tous individuels
    cluster_stats <- tibble::tibble()
    haies_rectangles <- NULL
    trees_sf$cluster <- 0
    trees_sf$classe <- "individuel"
  }

  # Compter les classes
  table_classes <- table(trees_sf$classe)
  message("\nClassification:")
  for (cl in names(table_classes)) {
    message("  ", cl, ": ", table_classes[cl], " arbres")
  }

  # Sortie
  list(
    dtm = dtm,
    chm = chm,
    segms = segms,
    trees = trees,
    trees_sf = trees_sf,
    cluster_stats = cluster_stats,
    haies_rectangles = haies_rectangles
  )
}


#' Extraire la ligne centrale d'un rectangle de haie
#'
#' A partir d'un rectangle représentant une haie, extrait la ligne centrale
#' dans le sens de la longueur ainsi que ses deux extrémités.
#'
#' @param haies_rectangles Objet sf avec les rectangles des haies (résultat de extraire_classifier_haies_lidar)
#' @param output_type Type de sortie: "lignes" pour les lignes centrales, "extremites" pour les points d'extrémité, ou "tous" pour les deux
#'
#' @return Selon output_type:
#' - "lignes": sf LINESTRING avec la ligne centrale de chaque haie
#' - "extremites": sf POINT avec les deux extrémités de chaque haie (avec colonne extremite = "debut" ou "fin")
#' - "tous": liste avec lignes (sf LINESTRING) et extremites (sf POINT)
#'
#' @export
extraire_ligne_centrale_haies <- function(haies_rectangles, output_type = c("tous", "lignes", "extremites")) {
  
  output_type <- match.arg(output_type)
  
  if (is.null(haies_rectangles) || nrow(haies_rectangles) == 0) {
    warning("Aucune haie à traiter")
    if (output_type == "tous") {
      return(list(lignes = NULL, extremites = NULL))
    } else {
      return(NULL)
    }
  }
  
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Le package 'sf' est requis.")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Le package 'dplyr' est requis.")
  if (!requireNamespace("purrr", quietly = TRUE))
    stop("Le package 'purrr' est requis.")
  
  crs_haies <- sf::st_crs(haies_rectangles)
  
  # Pour chaque haie, calculer la ligne centrale et ses extrémités
  resultats <- purrr::pmap(
    list(
      haies_rectangles$x_center,
      haies_rectangles$y_center,
      haies_rectangles$longueur,
      haies_rectangles$largeur,
      haies_rectangles$angle_deg,
      seq_len(nrow(haies_rectangles))
    ),
    function(x_center, y_center, L, W, angle_deg, idx) {
      # Convertir l'angle en radians
      angle_rad <- angle_deg * pi / 180
      
      # Demi-longueur
      half_L <- L / 2
      
      # Calcul des extrémités de la ligne centrale
      # La ligne centrale passe par le centre et est orientée selon l'angle
      cos_a <- cos(angle_rad)
      sin_a <- sin(angle_rad)
      
      # Extrémité 1 (début): centre - demi-longueur dans la direction de l'angle
      x1 <- x_center - half_L * cos_a
      y1 <- y_center - half_L * sin_a
      
      # Extrémité 2 (fin): centre + demi-longueur dans la direction de l'angle
      x2 <- x_center + half_L * cos_a
      y2 <- y_center + half_L * sin_a
      
      # Créer la ligne centrale
      ligne_centrale <- sf::st_linestring(matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE))
      
      # Créer les points d'extrémité
      point_debut <- sf::st_point(c(x1, y1))
      point_fin <- sf::st_point(c(x2, y2))
      
      list(
        idx = idx,
        ligne = ligne_centrale,
        point_debut = point_debut,
        point_fin = point_fin,
        x1 = x1, y1 = y1,
        x2 = x2, y2 = y2
      )
    }
  )
  
  # Extraire les attributs des haies
  haies_attrs <- haies_rectangles %>%
    sf::st_drop_geometry() %>%
    dplyr::mutate(idx = dplyr::row_number())
  
  if (output_type %in% c("lignes", "tous")) {
    # Créer le sf des lignes centrales
    lignes_list <- purrr::map(resultats, "ligne")
    lignes_data <- purrr::map_dfr(resultats, function(x) {
      tibble::tibble(
        idx = x$idx,
        x1 = x$x1, y1 = x$y1,
        x2 = x$x2, y2 = x$y2
      )
    })
    
    lignes_sf <- sf::st_sf(
      lignes_data,
      geometry = sf::st_sfc(lignes_list, crs = crs_haies)
    ) %>%
      dplyr::left_join(haies_attrs, by = "idx") %>%
      dplyr::select(-idx)
    
    message("✓ ", nrow(lignes_sf), " lignes centrales créées")
  }
  
  if (output_type %in% c("extremites", "tous")) {
    # Créer le sf des points d'extrémité
    # Chaque haie a 2 points (début et fin)
    extremites_list <- list()
    extremites_data <- list()
    
    for (i in seq_along(resultats)) {
      res <- resultats[[i]]
      
      # Point début
      extremites_list[[length(extremites_list) + 1]] <- res$point_debut
      extremites_data[[length(extremites_data) + 1]] <- tibble::tibble(
        idx = res$idx,
        extremite = "debut",
        x = res$x1,
        y = res$y1
      )
      
      # Point fin
      extremites_list[[length(extremites_list) + 1]] <- res$point_fin
      extremites_data[[length(extremites_data) + 1]] <- tibble::tibble(
        idx = res$idx,
        extremite = "fin",
        x = res$x2,
        y = res$y2
      )
    }
    
    extremites_data <- dplyr::bind_rows(extremites_data)
    
    extremites_sf <- sf::st_sf(
      extremites_data,
      geometry = sf::st_sfc(extremites_list, crs = crs_haies)
    ) %>%
      dplyr::left_join(haies_attrs, by = "idx") %>%
      dplyr::select(-idx)
    
    message("✓ ", nrow(extremites_sf), " points d'extrémité créés (", nrow(extremites_sf)/2, " haies)")
  }
  
  # Retourner selon le type demandé
  if (output_type == "lignes") {
    return(lignes_sf)
  } else if (output_type == "extremites") {
    return(extremites_sf)
  } else {
    return(list(
      lignes = lignes_sf,
      extremites = extremites_sf
    ))
  }
}


#' Calculer les zones d'influence du vent sur les haies avec une forme de demi-lune
#'
#' A partir des rectangles de haies, calcule des zones de protection/d'influence
#' en forme de demi-lune dans le sens du vent dominant. La forme part des deux 
#' extrémités de la haie et s'étend en courbe dans le sens du vent avec une 
#' distance H (multiple de la hauteur), créant une forme spline arrondie.
#'
#' Cette fonction permet de créer plusieurs couches de zones avec différents 
#' facteurs de hauteur (1H, 2H, 3H, etc. jusqu'à max_H).
#'
#' @param haies_rectangles Objet sf avec les rectangles des haies (résultat de extraire_classifier_haies_lidar)
#' @param direction_vent Direction du vent dominant en degrés (0-360, 0 = Nord, 90 = Est, etc.)
#' @param facteur_hauteur Vecteur de multiplicateurs de la hauteur pour calculer les distances H (ex: 1:40)
#' @param champ_centroid Point central du champ (sf POINT ou vecteur c(x, y)) pour déterminer le sens de la protection
#' @param n_points Nombre de points pour la courbe spline (défaut: 50)
#'
#' @return sf POLYGON avec les zones de protection pour tous les facteurs. Attributs:
#' - cluster, n_arbres, hauteur_p95, largeur, longueur, angle_deg (de la haie)
#' - facteur_h: le multiple de H utilisé pour cette zone (1, 2, 3, etc.)
#' - direction_vent: direction utilisée
#' - distance_H: distance H calculée pour ce facteur
#' - orientation_protection: "amont" ou "aval" selon la position du champ
#'
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
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Le package 'dplyr' est requis.")
  if (!requireNamespace("purrr", quietly = TRUE))
    stop("Le package 'purrr' est requis.")
  
  crs_haies <- sf::st_crs(haies_rectangles)
  
  # Convertir la direction du vent en radians (pointe vers où le vent souffle)
  angle_vent_rad <- (90 - direction_vent) * pi / 180 + pi
  
  # Si un centroid de champ est fourni, calculer l'orientation de protection
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
  
  # Helper: Créer une demi-lune avec spline (version originale sans aplatissement)
  create_demilune_spline <- function(x1, y1, x2, y2, x_apex, y_apex, n = 50) {
    # Points de contrôle pour le spline
    # Ordre: extrémité1 -> apex -> extrémité2
    x_ctrl <- c(x1, x_apex, x2)
    y_ctrl <- c(y1, y_apex, y2)
    
    # Créer un spline paramétrique
    t_ctrl <- c(0, 0.5, 1)  # Paramètre t pour chaque point de contrôle
    t_interp <- seq(0, 1, length.out = n)
    
    # Interpolation spline pour x et y
    x_spline <- stats::spline(t_ctrl, x_ctrl, xout = t_interp, method = "natural")$y
    y_spline <- stats::spline(t_ctrl, y_ctrl, xout = t_interp, method = "natural")$y
    
    # Combiner les points du spline avec la base (ligne entre les extrémités)
    # La base est formée par une ligne droite entre (x1,y1) et (x2,y2)
    n_base <- round(n / 2)  # Nombre de points pour la base
    t_base <- seq(0, 1, length.out = n_base)
    x_base <- x2 + t_base * (x1 - x2)  # De x2 vers x1
    y_base <- y2 + t_base * (y1 - y2)  # De y2 vers y1
    
    # Combiner: base (droite) + courbe (spline)
    # On enlève les points de début et fin de la base car ils sont déjà aux extrémités
    x_poly <- c(x_base[-length(x_base)], x_spline[-1])
    y_poly <- c(y_base[-length(y_base)], y_spline[-1])
    
    # Créer le polygone
    coords <- matrix(c(x_poly, y_poly), ncol = 2)
    # Fermer le polygone
    coords <- rbind(coords, coords[1, ])
    
    sf::st_polygon(list(coords))
  }
  
  # Initialiser les listes pour stocker toutes les zones
  all_zones_list <- list()
  all_zones_data <- list()
  zone_counter <- 0
  
  message("Création des zones de vent avec multiples facteurs H...")
  message("  Facteurs: ", min(facteur_hauteur), " à ", max(facteur_hauteur), "H")
  message("  Nombre de haies: ", nrow(haies_rectangles))
  
  # Pour chaque haie
  for (i in seq_len(nrow(haies_rectangles))) {
    haie <- haies_rectangles[i, ]
    
    # Récupérer les informations de la haie
    x_center <- haie$x_center
    y_center <- haie$y_center
    hauteur <- haie$hauteur_p95
    angle_haie_rad <- haie$angle_deg * pi / 180
    L <- haie$longueur
    
    # Calculer les extrémités de la ligne centrale (communes à tous les facteurs)
    half_L <- L / 2
    cos_haie <- cos(angle_haie_rad)
    sin_haie <- sin(angle_haie_rad)
    
    # Extrémité 1 (début) et 2 (fin)
    x1 <- x_center - half_L * cos_haie
    y1 <- y_center - half_L * sin_haie
    x2 <- x_center + half_L * cos_haie
    y2 <- y_center + half_L * sin_haie
    
    # Déterminer le sens de la protection (amont ou aval)
    orientation_protection <- "aval"
    angle_vent_calc <- angle_vent_rad
    
    if (!is.null(centroid_coords)) {
      # Calculer la direction du champ par rapport à la haie
      dx_champ <- centroid_coords[1] - x_center
      dy_champ <- centroid_coords[2] - y_center
      angle_champ <- atan2(dy_champ, dx_champ) * 180 / pi
      angle_champ <- (angle_champ + 360) %% 360
      
      # Si le champ est dans le sens du vent par rapport à la haie
      diff_angles <- abs(angle_champ - direction_vent)
      if (diff_angles > 180) diff_angles <- 360 - diff_angles
      
      if (diff_angles < 90) {
        orientation_protection <- "aval"
      } else {
        orientation_protection <- "amont"
        angle_vent_calc <- angle_vent_rad + pi
      }
    }
    
    # Calculer l'apex de base (pour la direction)
    cos_vent <- cos(angle_vent_calc)
    sin_vent <- sin(angle_vent_calc)
    
    # Créer une zone pour chaque facteur de hauteur
    for (facteur in facteur_hauteur) {
      # Calculer la distance H pour ce facteur
      distance_H <- hauteur * facteur
      
      # Calculer l'apex pour ce facteur
      x_apex <- x_center + distance_H * cos_vent
      y_apex <- y_center + distance_H * sin_vent
      
      # Créer la demi-lune avec spline
      demilune <- create_demilune_spline(x1, y1, x2, y2, x_apex, y_apex, n = n_points)
      
      zone_counter <- zone_counter + 1
      all_zones_list[[zone_counter]] <- demilune
      
      # Stocker les attributs
      all_zones_data[[zone_counter]] <- tibble::tibble(
        id_zone = zone_counter,
        cluster = haie$cluster,
        n_arbres = haie$n_arbres,
        hauteur_p95 = hauteur,
        largeur = haie$largeur,
        longueur = L,
        angle_haie_deg = haie$angle_deg,
        facteur_h = facteur,
        direction_vent = direction_vent,
        distance_H = distance_H,
        orientation_protection = orientation_protection,
        x_apex = x_apex,
        y_apex = y_apex,
        x1 = x1, y1 = y1,
        x2 = x2, y2 = y2
      )
    }
    
    if (i %% 10 == 0 || i == nrow(haies_rectangles)) {
      message("  Haie ", i, "/", nrow(haies_rectangles), " traitée (", length(facteur_hauteur), " zones chacune)")
    }
  }
  
  # Combiner les données
  all_zones_data <- dplyr::bind_rows(all_zones_data)
  
  # Créer l'objet sf
  zones_sf <- sf::st_sf(
    all_zones_data,
    geometry = sf::st_sfc(all_zones_list, crs = crs_haies)
  )
  
  message("✓ ", nrow(zones_sf), " zones de vent créées au total")
  message("  ", length(unique(zones_sf$cluster)), " haies × ", length(facteur_hauteur), " facteurs H")
  message("  Direction du vent: ", direction_vent, "°")
  
  return(zones_sf)
}


#' Rasteriser les zones de vent avec le facteur H minimum par pixel
#'
#' Convertit les zones de vent (demi-lunes avec multiples facteurs H) en raster.
#' Chaque pixel contient le facteur H minimum parmi toutes les zones qui couvrent ce pixel.
#' Par exemple, si un pixel est couvert par les zones 1H, 3H et 5H, il aura la valeur 1.
#'
#' @param zones_sf Objet sf avec les zones de vent (résultat de calculer_zones_vent_spline)
#' @param resolution Résolution du raster en mètres (défaut: 1)
#' @param extent_bbox Bounding box optionnel (objet bbox ou NULL pour calculer automatiquement)
#'
#' @return SpatRaster avec les valeurs de facteur H minimum pour chaque pixel
#'
#' @export
rasteriser_zones_gradient <- function(zones_sf, resolution = 1, extent_bbox = NULL) {
  
  if (is.null(zones_sf) || nrow(zones_sf) == 0) {
    warning("Aucune zone à rasteriser")
    return(NULL)
  }
  
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Le package 'sf' est requis.")
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Le package 'terra' est requis.")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Le package 'dplyr' est requis.")
  
  # Déterminer l'étendue du raster
  if (is.null(extent_bbox)) {
    extent_bbox <- sf::st_bbox(zones_sf)
  }
  
  # Créer un raster vide
  r_template <- terra::rast(
    xmin = extent_bbox$xmin,
    xmax = extent_bbox$xmax,
    ymin = extent_bbox$ymin,
    ymax = extent_bbox$ymax,
    resolution = resolution,
    crs = sf::st_crs(zones_sf)$wkt
  )
  
  # Initialiser le raster avec des valeurs très grandes (pas NA)
  # On utilise Inf pour pouvoir prendre le minimum ensuite
  terra::values(r_template) <- Inf
  
  message("Rasterisation des zones avec facteur H minimum...")
  message("  Résolution: ", resolution, "m")
  message("  Nombre de zones: ", nrow(zones_sf))
  message("  Dimensions: ", terra::nrow(r_template), " x ", terra::ncol(r_template), " pixels")
  
  # Convertir en vecteur terra pour traitement rapide
  zones_vect <- terra::vect(zones_sf)
  facteurs_h <- zones_sf$facteur_h
  
  # Pour chaque zone, mettre à jour le raster avec le minimum
  for (i in seq_len(nrow(zones_sf))) {
    # Rasteriser cette zone avec sa valeur de facteur_h
    zone_raster <- terra::rasterize(
      zones_vect[i], 
      r_template, 
      field = facteurs_h[i],
      background = NA,
      update = TRUE,
      min = TRUE  # Prend le minimum entre la valeur existante et la nouvelle
    )
    
    # Mettre à jour le raster
    r_template <- zone_raster
    
    if (i %% 100 == 0 || i == nrow(zones_sf)) {
      message("  Zone ", i, "/", nrow(zones_sf), " rasterisée")
    }
  }
  
  # Remplacer Inf par NA (pixels non couverts par aucune zone)
  r_template <- terra::ifel(r_template == Inf, NA, r_template)
  
  # Nommer la couche
  names(r_template) <- "facteur_H_min"
  
  # Statistiques
  vals <- terra::values(r_template)
  vals <- vals[!is.na(vals)]
  
  if (length(vals) > 0) {
    message("✓ Raster créé avec succès")
    message("  Facteur H minimum: ", round(min(vals), 0), "H")
    message("  Facteur H maximum: ", round(max(vals), 0), "H")
    message("  Facteur H moyen: ", round(mean(vals), 1), "H")
    message("  Pixels couverts: ", length(vals))
  }
  
  return(r_template)
}


#' Rasteriser les zones de vent avec le facteur H minimum par pixel (version corrigée)
#'
#' Cette version utilise une approche différente qui garantit le calcul correct 
#' du minimum H par pixel.
#'
#' @param zones_sf Objet sf avec les zones de vent
#' @param resolution Résolution du raster en mètres (défaut: 1)
#' @param extent_bbox Bounding box optionnel
#'
#' @return SpatRaster avec les valeurs de facteur H minimum pour chaque pixel
#'
#' @export
rasteriser_zones_gradient_v2 <- function(zones_sf, resolution = 1, extent_bbox = NULL) {
  
  if (is.null(zones_sf) || nrow(zones_sf) == 0) {
    warning("Aucune zone à rasteriser")
    return(NULL)
  }
  
  if (!requireNamespace("sf", quietly = TRUE))
    stop("Le package 'sf' est requis.")
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Le package 'terra' est requis.")
  if (!requireNamespace("dplyr", quietly = TRUE))
    stop("Le package 'dplyr' est requis.")
  
  # Déterminer l'étendue du raster
  if (is.null(extent_bbox)) {
    extent_bbox <- sf::st_bbox(zones_sf)
  }
  
  # Créer un raster vide avec des valeurs très grandes
  r_template <- terra::rast(
    xmin = extent_bbox$xmin,
    xmax = extent_bbox$xmax,
    ymin = extent_bbox$ymin,
    ymax = extent_bbox$ymax,
    resolution = resolution,
    crs = sf::st_crs(zones_sf)$wkt
  )
  
  # Initialiser avec Inf (valeur très grande pour le minimum)
  terra::values(r_template) <- Inf
  
  message("Rasterisation des zones (v2) avec facteur H minimum...")
  message("  Résolution: ", resolution, "m")
  message("  Nombre de zones: ", nrow(zones_sf))
  
  # Trier les zones par facteur H croissant (important!)
  # Comme on veut le minimum, on traite d'abord les petits H
  zones_sorted <- zones_sf %>%
    dplyr::arrange(facteur_h)
  
  # Convertir en vecteur terra
  zones_vect <- terra::vect(zones_sorted)
  facteurs_h <- zones_sorted$facteur_h
  
  # Méthode corrigée: rasteriser toutes les zones d'abord
  # puis calculer le minimum
  message("  Création de la stack de rasters...")
  
  # Créer une liste pour stocker tous les rasters
  raster_list <- list()
  
  for (i in seq_len(nrow(zones_sorted))) {
    # Créer un raster temporaire pour cette zone avec sa valeur H
    zone_rast <- terra::rasterize(
      zones_vect[i], 
      r_template, 
      field = facteurs_h[i],
      background = NA
    )
    
    raster_list[[i]] <- zone_rast
    
    if (i %% 50 == 0 || i == nrow(zones_sorted)) {
      message("  Zone ", i, "/", nrow(zones_sorted), " rasterisée (H=", facteurs_h[i], ")")
    }
  }
  
  message("  Calcul du minimum par pixel...")
  # Combiner tous les rasters et calculer le minimum
  if (length(raster_list) > 0) {
    # Utiliser rast pour créer un SpatRasterDataset puis calculer le min
    r_stack <- terra::rast(raster_list)
    r_template <- terra::app(r_stack, fun = min, na.rm = TRUE)
  }
  
  # Remplacer Inf par NA
  r_template <- terra::ifel(r_template == Inf, NA, r_template)
  
  # Nommer
  names(r_template) <- "facteur_H_min"
  
  # Statistiques
  vals <- terra::values(r_template)
  vals <- vals[!is.na(vals)]
  
  if (length(vals) > 0) {
    message("✓ Raster créé avec succès")
    message("  Facteur H minimum: ", round(min(vals), 0), "H")
    message("  Facteur H maximum: ", round(max(vals), 0), "H")
    message("  Facteur H moyen: ", round(mean(vals), 1), "H")
    message("  Pixels couverts: ", length(vals))
  }
  
  return(r_template)
}
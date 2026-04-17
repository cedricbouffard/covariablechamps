#' Télécharger les données LiDAR ponctuelles (COPC) pour une zone
#'
#' Cette fonction télécharge les données LiDAR ponctuelles au format COPC (Cloud Optimized Point Cloud)
#' depuis CanElevation (Canada) ou Données Québec pour une zone d'intérêt donnée avec un buffer.
#'
#' @param polygone Un objet `sf` représentant la zone d'intérêt ou un chemin vers un fichier vectoriel
#' @param buffer Distance du buffer en mètres (défaut: 50)
#' @param source Source des données: "auto" (défaut), "canelevation", ou "donneesquebec"
#' @param dossier Dossier de sortie pour sauvegarder les fichiers (optionnel)
#' @param métriques Logique. Si TRUE, calcule les métriques de hauteur
#'
#' @return Une liste contenant le nuage de points (objet LAS) et les métriques calculées
#' @export
#'
#' @examples
#' \dontrun{
#' # Extraire le LiDAR ponctuel avec un buffer de 50m
#' champ <- sf::st_read("champ.shp")
#' lidar_points <- telecharger_lidar_ponctuel(champ, buffer = 50)
#'
#' # Visualiser
#' plot(lidar_points$nuage_points)
#' print(lidar_points$metriques)
#' }
#'
#' @importFrom sf st_read st_crs st_transform st_union st_bbox st_buffer st_is_longlat
#' @importFrom methods is
telecharger_lidar_ponctuel <- function(polygone, buffer = 50, source = "auto",
                                        dossier = NULL, metriques = TRUE) {
  
  # Lire le polygone si c'est un chemin de fichier
  if (is.character(polygone)) {
    polygone <- sf::st_read(polygone, quiet = TRUE)
  }
  
  # Vérifier que c'est bien un objet sf
  if (!methods::is(polygone, "sf")) {
    stop("Le polygone doit être un objet sf ou un chemin vers un fichier vectoriel")
  }
  
  # Assigner WGS84 si pas de CRS défini
  if (is.na(sf::st_crs(polygone))) {
    sf::st_crs(polygone) <- 4326
  }
  
  # Transformer en WGS84 pour les requêtes
  polygone_wgs84 <- polygone |>
    sf::st_transform(4326) |>
    sf::st_union()
  
  # Créer la zone avec buffer
  message("Préparation de la zone d'intérêt avec buffer de ", buffer, " m...")
  
  # Déterminer le CRS projeté approprié
  if (sf::st_is_longlat(polygone_wgs84)) {
    centroid <- sf::st_coordinates(sf::st_centroid(polygone_wgs84))
    lon <- centroid[1]
    lat <- centroid[2]
    utm_zone <- floor((lon + 180) / 6) + 1
    epsg_code <- ifelse(lat >= 0, 32600 + utm_zone, 32700 + utm_zone)
    crs_projete <- epsg_code
  } else {
    crs_projete <- sf::st_crs(polygone)$epsg
  }
  
  # Créer le buffer
  polygone_buffer <- polygone |>
    sf::st_transform(crs_projete) |>
    sf::st_union() |>
    sf::st_buffer(dist = buffer)
  
  message("Zone avec buffer créée")
  
  # Essayer les sources selon l'ordre demandé
  resultat <- NULL
  
  if (source %in% c("auto", "canelevation")) {
    message("\n=== Recherche dans CanElevation ===")
    resultat <- tryCatch({
      telecharger_copc_canelevation(polygone_buffer, dossier)
    }, error = function(e) {
      message("CanElevation non disponible: ", conditionMessage(e))
      NULL
    })
  }
  
  if (is.null(resultat) && source %in% c("auto", "donneesquebec")) {
    message("\n=== Recherche dans Données Québec ===")
    resultat <- tryCatch({
      telecharger_lidar_donnees_quebec(polygone_buffer, dossier)
    }, error = function(e) {
      message("Données Québec non disponible: ", conditionMessage(e))
      NULL
    })
  }
  
  if (is.null(resultat)) {
    stop("Aucune donnée LiDAR ponctuelle disponible pour cette zone.")
  }
  
  # Calculer les métriques si demandé
  if (metriques && !is.null(resultat$nuage_points)) {
    message("\n=== Calcul des métriques de hauteur ===")
    resultat$metriques <- calculer_metriques_lidar(resultat$nuage_points)
    print(resultat$metriques)
  }
  
  return(resultat)
}

  #' Télécharger les données COPC depuis CanElevation
#' @noRd
telecharger_copc_canelevation <- function(polygone_buffer, dossier = NULL) {
  
  message("Recherche des tuiles via l'API REST CanElevation...")
  
  # L'API REST utilise le CRS 3978 (Canada Atlas Lambert)
  crs_api <- 3978
  polygone_api <- sf::st_transform(polygone_buffer, crs_api)
  
  # Obtenir la bounding box (envelope) pour la requête
  bbox <- sf::st_bbox(polygone_api)
  bbox_str <- paste(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"], sep = ",")
  
  # Construire l'URL de la requête
  base_url <- "https://maps-cartes.services.geo.ca/server_serveur/rest/services/NRCan/lidar_point_cloud_canelevation_en/MapServer/1/query"
  
  query_params <- list(
    where = "1=1",
    geometry = bbox_str,
    geometryType = "esriGeometryEnvelope",
    spatialRel = "esriSpatialRelIntersects",
    inSR = "3978",
    outSR = "3978",
    outFields = "*",
    returnGeometry = "true",
    f = "json"
  )
  
  message("  Requête avec bbox: ", bbox_str)
  
  # Faire la requête
  response <- httr::GET(base_url, query = query_params)
  
  if (httr::status_code(response) != 200) {
    stop("Erreur lors de la requête à l'API CanElevation: ", httr::status_code(response))
  }
  
  # Parser la réponse
  tuiles_data <- httr::content(response, "parsed")
  
  # DEBUG: Afficher la réponse
  message("  Réponse API: ", length(tuiles_data$features), " features trouvés")
  if (!is.null(tuiles_data$error)) {
    message("  Erreur API: ", tuiles_data$error$message)
  }
  
  # Vérifier s'il y a des features
  if (is.null(tuiles_data$features) || length(tuiles_data$features) == 0) {
    # Essayer avec une requête plus simple (sans geometry)
    message("  Aucune tuile avec intersect, essai avec where...")
    query_params_simple <- list(
      where = "1=1",
      outFields = "*",
      returnGeometry = "false",
      resultRecordCount = "5",
      f = "json"
    )
    response_simple <- httr::GET(base_url, query = query_params_simple)
    if (httr::status_code(response_simple) == 200) {
      test_data <- httr::content(response_simple, "parsed")
      message("  Test API (5 premières tuiles): ", length(test_data$features), " features")
      if (length(test_data$features) > 0) {
        message("  Exemple d'URL: ", test_data$features[[1]]$attributes$url)
        message("  Exemple de bbox tuile: ", 
                test_data$features[[1]]$attributes$ Shape$extent$xmin, ", ",
                test_data$features[[1]]$attributes$Shape$extent$ymin, " - ",
                test_data$features[[1]]$attributes$Shape$extent$xmax, ", ",
                test_data$features[[1]]$attributes$Shape$extent$ymax)
      }
    }
    stop("Aucune tuile COPC disponible pour cette zone dans CanElevation")
  }
  
  # Convertir les données ESRI JSON en sf
  features <- tuiles_data$features
  geometries <- lapply(features, function(f) {
    rings <- f$geometry$rings
    # Créer un polygon à partir des rings
    coords <- do.call(rbind, lapply(rings[[1]], function(pt) c(pt[[1]], pt[[2]])))
    sf::st_polygon(list(coords))
  })
  
  # Créer le data.frame avec les attributs
  attrs <- lapply(features, function(f) {
    as.data.frame(f$attributes, stringsAsFactors = FALSE)
  })
  attrs_df <- do.call(rbind, attrs)
  
  # Créer l'objet sf
  tuiles_intersect <- sf::st_sf(attrs_df, geometry = sf::st_sfc(geometries, crs = 3978))
  
  # Ne garder que les tuiles qui intersectent réellement le polygone
  tuiles_intersect <- tuiles_intersect[sf::st_intersects(tuiles_intersect, polygone_api, sparse = FALSE), ]
  
  message("Nombre de tuiles trouvées: ", nrow(tuiles_intersect))
  
  # Télécharger et traiter chaque tuile
  nuages_points <- list()
  
  for (i in seq_len(nrow(tuiles_intersect))) {
    tuile <- tuiles_intersect[i, ]
    url_copc <- tuile$url
    
    message("\nTraitement de la tuile ", i, "/", nrow(tuiles_intersect), ":")
    message("  ", basename(url_copc))
    
    # Utiliser /vsicurl/ pour accès virtuel au fichier COPC
    # Cela permet de lire uniquement les données nécessaires sans télécharger tout le fichier
    url_vsi <- paste0("/vsicurl/", url_copc)
    message("  Accès virtuel via VSI...")
    
    # Lire le nuage de points avec lidR si disponible
    if (requireNamespace("lidR", quietly = TRUE)) {
      las <- tryCatch({
        # Lire le fichier COPC via VSI
        lidR::readLAS(url_vsi)
      }, error = function(e) {
        message("  ✗ Erreur de lecture LAS (VSI): ", conditionMessage(e))
        # Fallback: essayer de télécharger si VSI échoue
        message("  Tentative de téléchargement direct...")
        temp_copc <- tempfile(fileext = ".copc.laz")
        tryCatch({
          download.file(url_copc, temp_copc, mode = "wb", quiet = TRUE)
          las <- lidR::readLAS(temp_copc)
          unlink(temp_copc)
          return(las)
        }, error = function(e2) {
          unlink(temp_copc)
          message("  ✗ Échec du téléchargement: ", conditionMessage(e2))
          return(NULL)
        })
      })
      
      if (!is.null(las)) {
        # Découper selon le polygone
        las_decoupe <- tryCatch({
          lidR::clip_roi(las, polygone_buffer)
        }, error = function(e) {
          message("  ✗ Erreur de découpage: ", conditionMessage(e))
          return(NULL)
        })
        
        if (!is.null(las_decoupe) && lidR::npoints(las_decoupe) > 0) {
          message("  ✓ ", lidR::npoints(las_decoupe), " points extraits")
          nuages_points[[length(nuages_points) + 1]] <- las_decoupe
        }
      }
    } else {
      message("  Package 'lidR' non disponible - impossible de lire les points")
    }
  }
  
  # Fusionner les nuages de points
  if (length(nuages_points) == 0) {
    stop("Aucun point LiDAR extrait")
  }
  
  message("\nFusion des nuages de points...")
  if (length(nuages_points) == 1) {
    nuage_final <- nuages_points[[1]]
  } else {
    # Fusionner avec lidR
    if (requireNamespace("lidR", quietly = TRUE)) {
      nuage_final <- do.call(rbind, nuages_points)
    } else {
      nuage_final <- nuages_points[[1]]
    }
  }
  
  message("Total: ", lidR::npoints(nuage_final), " points")
  
  # Sauvegarder si dossier spécifié
  if (!is.null(dossier)) {
    dir.create(dossier, showWarnings = FALSE, recursive = TRUE)
    fichier_las <- file.path(dossier, "lidar_points.laz")
    lidR::writeLAS(nuage_final, fichier_las)
    message("Nuage de points sauvegardé: ", fichier_las)
  }
  
  list(
    nuage_points = nuage_final,
    source = "CanElevation",
    nb_tuiles = nrow(tuiles_intersect),
    crs = sf::st_crs(nuage_final)$epsg
  )
}

#' Obtenir le chemin du cache LiDAR
#' @noRd
get_lidar_cache_dir <- function() {
  cache_dir <- file.path(path.expand("~"), ".covariablechamps", "lidar_cache")
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  return(cache_dir)
}

#' Vider le cache LiDAR
#'
#' Supprime tous les fichiers LAZ téléchargés localement
#'
#' @return Message de confirmation
#' @export
#'
#' @examples
#' \dontrun{
#' vider_cache_lidar()
#' }
vider_cache_lidar <- function() {
  cache_dir <- get_lidar_cache_dir()
  
  if (!dir.exists(cache_dir)) {
    message("Le cache LiDAR n'existe pas encore.")
    return(invisible(NULL))
  }
  
  # Compter les fichiers
  fichiers <- list.files(cache_dir, pattern = "\\.laz$", full.names = TRUE)
  nb_fichiers <- length(fichiers)
  
  if (nb_fichiers == 0) {
    message("Le cache LiDAR est déjà vide.")
    return(invisible(NULL))
  }
  
  # Calculer la taille totale
  taille_totale <- sum(file.info(fichiers)$size) / (1024^3)  # En GB
  
  # Demander confirmation
  message("Le cache LiDAR contient ", nb_fichiers, " fichier(s) (", round(taille_totale, 2), " GB)")
  message("Chemin: ", cache_dir)
  
  # Supprimer les fichiers
  unlink(fichiers)
  message("✓ Cache LiDAR vidé avec succès")
  
  return(invisible(NULL))
}

#' Télécharger les données LiDAR depuis Données Québec
#' @noRd
telecharger_lidar_donnees_quebec <- function(polygone_buffer, dossier = NULL) {
  message("Recherche des tuiles via le service WFS de Données Québec...")
  
  # Le service WFS fonctionne avec EPSG:4326 (WGS84)
  crs_wfs <- 4326
  polygone_wfs <- sf::st_transform(polygone_buffer, crs_wfs)
  
  # Obtenir la bounding box
  bbox <- sf::st_bbox(polygone_wfs)
  bbox_str <- paste(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"], sep = ",")
  
  message("  Bbox (EPSG:4326): ", bbox_str)
  
  # Construire la requête WFS avec les paramètres corrects
  base_url <- "https://servicesvecto3.mern.gouv.qc.ca/geoserver/Index_Telechargement_Lidar_Pub/wfs"
  
  # Pour WFS 1.1.0 avec GeoServer, essayer l'ordre Y,X pour EPSG:4326
  # Certains serveurs attendent lat,lon au lieu de lon,lat
  bbox_str_yx <- paste(bbox["ymin"], bbox["xmin"], bbox["ymax"], bbox["xmax"], sep = ",")
  
  query_params <- list(
    service = "WFS",
    version = "1.1.0",
    request = "GetFeature",
    typeName = "Index_Telechargement_Lidar_Pub:IndexTelechargementLidarPlusRecent",
    srsName = "EPSG:4326",
    bbox = bbox_str_yx,
    outputFormat = "application/json"
  )
  
  message("  Requête WFS...")
  
  # Faire la requête avec retry
  response <- NULL
  for (attempt in 1:3) {
    tryCatch({
      response <- httr::GET(base_url, query = query_params, httr::timeout(60))
      if (httr::status_code(response) == 200) break
    }, error = function(e) {
      message("    Tentative ", attempt, " échouée: ", conditionMessage(e))
      if (attempt < 3) Sys.sleep(2)
    })
  }
  
  if (is.null(response) || httr::status_code(response) != 200) {
    stop("Erreur lors de la requête WFS Données Québec après 3 tentatives")
  }
  
  # Lire la réponse en texte brut
  response_text <- httr::content(response, "text", encoding = "UTF-8")
  
  # Parser le JSON avec jsonlite
  tuiles_data <- jsonlite::fromJSON(response_text, simplifyVector = FALSE)
  
  # Vérifier s'il y a des features
  if (is.null(tuiles_data$features) || length(tuiles_data$features) == 0) {
    stop("Aucune tuile LAZ disponible pour cette zone dans Données Québec")
  }
  
  message("  Nombre de tuiles trouvées: ", length(tuiles_data$features))
  
  # Écrire le GeoJSON dans un fichier temporaire pour sf::st_read
  temp_geojson <- tempfile(fileext = ".geojson")
  writeLines(response_text, temp_geojson)
  tuiles_intersect <- sf::st_read(temp_geojson, quiet = TRUE)
  unlink(temp_geojson)
  
  # Transformer dans le CRS du polygone pour filtrer
  tuiles_intersect <- sf::st_transform(tuiles_intersect, sf::st_crs(polygone_buffer))
  
  # Ne garder que les tuiles qui intersectent réellement
  tuiles_intersect <- tuiles_intersect[sf::st_intersects(tuiles_intersect, polygone_buffer, sparse = FALSE), ]
  
  message("  Tuiles intersectant la zone: ", nrow(tuiles_intersect))
  
  if (nrow(tuiles_intersect) == 0) {
    stop("Aucune tuile ne couvre la zone d'intérêt")
  }
  
  # Préparer le cache
  cache_dir <- get_lidar_cache_dir()
  message("\nCache LiDAR: ", cache_dir)
  
  # Télécharger et traiter chaque tuile
  nuages_points <- list()
  fichiers_telecharges <- c()
  
  for (i in seq_len(nrow(tuiles_intersect))) {
    tuile <- tuiles_intersect[i, ]
    # Le champ s'appelle TELECHARGEMENT_TUILE
    url_laz <- as.character(tuile$TELECHARGEMENT_TUILE)
    nom_fichier <- basename(url_laz)
    chemin_local <- file.path(cache_dir, nom_fichier)
    
    message("\nTraitement de la tuile ", i, "/", nrow(tuiles_intersect), ":")
    message("  ", nom_fichier)
    
    # Vérifier si le fichier existe déjà
    if (file.exists(chemin_local)) {
      message("  ✓ Fichier déjà en cache")
    } else {
      message("  Téléchargement (fichier ~100 MB, patience...)...")
      tryCatch({
        # Augmenter le timeout pour les gros fichiers (5 minutes)
        old_timeout <- getOption("timeout")
        options(timeout = 300)
        on.exit(options(timeout = old_timeout))
        
        download.file(url_laz, chemin_local, mode = "wb", quiet = TRUE)
        message("  ✓ Téléchargé (", round(file.size(chemin_local) / (1024^2), 1), " MB)")
        fichiers_telecharges <- c(fichiers_telecharges, chemin_local)
      }, error = function(e) {
        message("  ✗ Erreur de téléchargement: ", conditionMessage(e))
        # Supprimer le fichier partiel si erreur
        if (file.exists(chemin_local)) unlink(chemin_local)
        return(NULL)
      })
    }
    
    # Lire le fichier LAZ avec lidR
    if (requireNamespace("lidR", quietly = TRUE) && file.exists(chemin_local)) {
      message("  Lecture du fichier LAZ...")
      message("  Taille fichier: ", round(file.size(chemin_local) / (1024^2), 1), " MB")
      
      las <- tryCatch({
        lidR::readLAS(chemin_local)
      }, error = function(e) {
        message("  ✗ Erreur de lecture LAS: ", conditionMessage(e))
        return(NULL)
      })
      
      if (is.null(las)) {
        message("  ✗ Impossible de lire le fichier (NULL)")
      }
      
      if (!is.null(las)) {
        # Afficher les infos du LAS
        message("  Info LAS: ", lidR::npoints(las), " points, CRS: ", sf::st_crs(las)$epsg)
        
        # Transformer le polygone dans le CRS du LAS
        polygone_las <- sf::st_transform(polygone_buffer, sf::st_crs(las))
        
        # Afficher la bbox du LAS et du polygone
        bbox_las <- sf::st_bbox(las)
        bbox_poly <- sf::st_bbox(polygone_las)
        message("  Bbox LAS: ", paste(round(bbox_las, 0), collapse = ", "))
        message("  Bbox Polygone: ", paste(round(bbox_poly, 0), collapse = ", "))
        
        # Découper selon le polygone
        las_decoupe <- tryCatch({
          lidR::clip_roi(las, polygone_las)
        }, error = function(e) {
          message("  ✗ Erreur de découpage: ", conditionMessage(e))
          return(NULL)
        })
        
        if (!is.null(las_decoupe)) {
          message("  Points après découpage: ", lidR::npoints(las_decoupe))
          if (lidR::npoints(las_decoupe) > 0) {
            nuages_points[[length(nuages_points) + 1]] <- las_decoupe
          }
        }
      }
    }
  }
  
  # Fusionner les nuages de points
  if (length(nuages_points) == 0) {
    stop("Aucun point LiDAR extrait")
  }
  
  message("\nFusion des nuages de points...")
  if (length(nuages_points) == 1) {
    nuage_final <- nuages_points[[1]]
  } else {
    if (requireNamespace("lidR", quietly = TRUE)) {
      nuage_final <- do.call(rbind, nuages_points)
    } else {
      nuage_final <- nuages_points[[1]]
    }
  }
  
  message("Total: ", lidR::npoints(nuage_final), " points")
  
  # Sauvegarder si dossier spécifié
  if (!is.null(dossier)) {
    dir.create(dossier, showWarnings = FALSE, recursive = TRUE)
    fichier_las <- file.path(dossier, "lidar_points.laz")
    lidR::writeLAS(nuage_final, fichier_las)
    message("\nNuage de points sauvegardé: ", fichier_las)
  }
  
  # Info sur le cache
  tous_fichiers <- list.files(cache_dir, pattern = "\\.laz$", full.names = TRUE)
  taille_cache <- sum(file.info(tous_fichiers)$size) / (1024^3)  # En GB
  message("\nTaille du cache LiDAR: ", length(tous_fichiers), " fichier(s), ", round(taille_cache, 2), " GB")
  message("Utiliser vider_cache_lidar() pour vider le cache")
  
  list(
    nuage_points = nuage_final,
    source = "Données Québec",
    nb_tuiles = nrow(tuiles_intersect),
    fichiers_telecharges = fichiers_telecharges,
    cache_dir = cache_dir,
    crs = sf::st_crs(nuage_final)$epsg
  )
}

#' Calculer les métriques du nuage de points LiDAR
#' @noRd
calculer_metriques_lidar <- function(las) {
  if (!requireNamespace("lidR", quietly = TRUE)) {
    warning("Package 'lidR' requis pour calculer les métriques")
    return(NULL)
  }
  
  # Extraire les coordonnées Z (hauteurs)
  z <- lidR::filter_poi(las, Z > -50 & Z < 5000)$Z  # Filtrer les valeurs aberrantes
  
  if (length(z) == 0) {
    return(NULL)
  }
  
  # Calculer les métriques
  metriques <- data.frame(
    nb_points = length(z),
    z_min = min(z, na.rm = TRUE),
    z_max = max(z, na.rm = TRUE),
    z_moy = mean(z, na.rm = TRUE),
    z_median = median(z, na.rm = TRUE),
    z_sd = sd(z, na.rm = TRUE),
    z_p05 = quantile(z, 0.05, na.rm = TRUE),
    z_p25 = quantile(z, 0.25, na.rm = TRUE),
    z_p75 = quantile(z, 0.75, na.rm = TRUE),
    z_p95 = quantile(z, 0.95, na.rm = TRUE),
    z_iqr = IQR(z, na.rm = TRUE)
  )
  
  # Calculer la rugosité (écart-type local) si assez de points
  if (length(z) > 100) {
    # Rugosité = coefficient de variation
    metriques$rugosite_cv <- metriques$z_sd / metriques$z_moy
  }
  
  return(metriques)
}

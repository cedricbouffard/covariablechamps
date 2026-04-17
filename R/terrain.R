#' Calculer la pente à partir d'un MNT
#'
#' Calcule la pente en degrés à partir d'un modèle numérique de terrain (MNT).
#'
#' @param mnt Un objet `SpatRaster` représentant le MNT
#' @param filename Chemin de fichier pour sauvegarder le résultat (optionnel)
#'
#' @return Un objet `SpatRaster` contenant la pente en degrés
#' @export
#'
#' @examples
#' \dontrun{
#' mnt <- telecharger_lidar(champ)
#' pente <- calculer_pente(mnt)
#' }
#'
#' @importFrom terra terrain writeRaster
#' @importFrom methods is
calculer_pente <- function(mnt, filename = NULL) {
  if (!methods::is(mnt, "SpatRaster")) {
    stop("Le MNT doit être un objet SpatRaster")
  }
  
  message("Calcul de la pente...")
  pente <- terra::terrain(mnt, v = "slope", unit = "degrees")
  names(pente) <- "pente"
  
  if (!is.null(filename)) {
    terra::writeRaster(pente, filename, overwrite = TRUE)
    message("Pente sauvegardée: ", filename)
  }
  
  return(pente)
}

#' Calculer l'aspect (orientation) à partir d'un MNT
#'
#' Calcule l'aspect (orientation en degrés, 0=Nord, 90=Est, 180=Sud, 270=Ouest)
#' à partir d'un modèle numérique de terrain.
#'
#' @param mnt Un objet `SpatRaster` représentant le MNT
#' @param filename Chemin de fichier pour sauvegarder le résultat (optionnel)
#'
#' @return Un objet `SpatRaster` contenant l'aspect en degrés
#' @export
#'
#' @examples
#' \dontrun{
#' mnt <- telecharger_lidar(champ)
#' aspect <- calculer_aspect(mnt)
#' }
#'
#' @importFrom terra terrain writeRaster
#' @importFrom methods is
calculer_aspect <- function(mnt, filename = NULL) {
  if (!methods::is(mnt, "SpatRaster")) {
    stop("Le MNT doit être un objet SpatRaster")
  }
  
  message("Calcul de l'aspect...")
  aspect <- terra::terrain(mnt, v = "aspect", unit = "degrees")
  names(aspect) <- "aspect"
  
  if (!is.null(filename)) {
    terra::writeRaster(aspect, filename, overwrite = TRUE)
    message("Aspect sauvegardé: ", filename)
  }
  
  return(aspect)
}

#' Calculer les géomorphons à partir d'un MNT
#'
#' Utilise le package rgeomorphon pour classifier les formes de terrain
#' selon la méthode des géomorphons (Jasiewicz et Stepinski, 2013).
#'
#' @param mnt Un objet `SpatRaster` représentant le MNT
#' @param search_radius Rayon de recherche en mètres (défaut: 100)
#' @param skip_radius Rayon à ignorer autour du pixel central en mètres (défaut: 0)
#' @param flatness_threshold Seuil de platitude en pourcent de pente (défaut: 0.05)
#' @param filename Chemin de fichier pour sauvegarder le résultat (optionnel)
#' @param multi_scale Logique. Si TRUE, calcule aux échelles petite, moyenne et grande
#'
#' @return Un objet `SpatRaster` ou une liste de 3 rasters si multi_scale=TRUE
#' @export
#'
#' @details
#' Les géomorphons identifient 10 formes de terrain avec des labels en français:
#' 1 = Plat, 2 = Pic, 3 = Crête, 4 = Épaulement, 5 = Éperon,
#' 6 = Pente, 7 = Creux, 8 = Pied de pente, 9 = Vallée, 10 = Fosse
#'
#' @examples
#' \dontrun{
#' mnt <- telecharger_lidar(champ)
#' geom <- calculer_geomorphons(mnt, search_radius = 50)
#' 
#' # Calculer aux 3 échelles automatiquement
#' geom_multi <- calculer_geomorphons(mnt, multi_scale = TRUE)
#' }
#'
#' @importFrom terra writeRaster res values levels
#' @importFrom methods is
calculer_geomorphons <- function(mnt, search_radius = 100, skip_radius = 0,
                                   flatness_threshold = 0.05, filename = NULL,
                                   multi_scale = FALSE) {
  if (!methods::is(mnt, "SpatRaster")) {
    stop("Le MNT doit être un objet SpatRaster")
  }
  
  # Vérifier que rgeomorphon est disponible
  if (!requireNamespace("rgeomorphon", quietly = TRUE)) {
    stop("Le package 'rgeomorphon' est requis. Installez-le avec:\n",
         "remotes::install_github('brownag/rgeomorphon')")
  }
  
  # Convertir le seuil de platitude de pourcent à degrés
  # tan(angle) = pourcent/100, donc angle = atan(pourcent/100)
  flatness_deg <- atan(flatness_threshold / 100) * 180 / pi
  message("Seuil de platitude: ", flatness_threshold, "% = ", round(flatness_deg, 4), " degrés")
  
  if (multi_scale) {
    # Calculer aux 3 échelles
    scales <- list(
      petit = 50,   # Microformes
      moyen = 100,  # Formes à l'échelle du champ
      grand = 200   # Macroformes
    )
    
    message("\n=== Calcul des géomorphons aux 3 échelles ===")
    resultats <- list()
    
    for (nom_scale in names(scales)) {
      rayon <- scales[[nom_scale]]
      message("\nÉchelle ", nom_scale, " (", rayon, "m)...")
      
      geom <- calculer_geomorphons_single(mnt, search_radius = rayon, 
                                          skip_radius = skip_radius,
                                          flatness_threshold = flatness_threshold)
      
      names(geom) <- paste0("geomorphons_", nom_scale)
      resultats[[nom_scale]] <- geom
      
      # Sauvegarder si dossier spécifié
      if (!is.null(filename)) {
        fichier_scale <- sub("\\.tif$", paste0("_", nom_scale, ".tif"), filename)
        terra::writeRaster(geom, fichier_scale, overwrite = TRUE)
        message("  Sauvegardé: ", fichier_scale)
      }
    }
    
    message("\n=== Calcul aux 3 échelles terminé ===")
    return(resultats)
  } else {
    # Calcul simple à une échelle
    geom <- calculer_geomorphons_single(mnt, search_radius = search_radius,
                                        skip_radius = skip_radius,
                                        flatness_threshold = flatness_threshold)
    
    if (!is.null(filename)) {
      terra::writeRaster(geom, filename, overwrite = TRUE)
      message("Géomorphons sauvegardés: ", filename)
    }
    
    return(geom)
  }
}

#' Fonction interne pour calculer les géomorphons à une échelle
#' @noRd
calculer_geomorphons_single <- function(mnt, search_radius, skip_radius, flatness_threshold) {
  
  message("  Calcul avec rayon = ", search_radius, " m...")
  
  # Convertir le seuil de platitude de pourcent à degrés
  flatness_deg <- atan(flatness_threshold / 100) * 180 / pi
  
  # Calculer les rayons en pixels
  res_mnt <- terra::res(mnt)[1]
  search_pixels <- round(search_radius / res_mnt)
  skip_pixels <- round(skip_radius / res_mnt)
  
  if (search_pixels < 1) {
    warning("Le rayon de recherche est inférieur à la résolution du MNT. ",
            "Utilisation d'un rayon de 1 pixel.")
    search_pixels <- 1
  }
  
  # Calculer les géomorphons
  geom_result <- rgeomorphon::geomorphons(mnt,
                                           search = search_pixels,
                                           skip = skip_pixels,
                                           flat_angle_deg = flatness_deg,
                                           forms = "forms10")
  
  # La fonction retourne une liste si plusieurs sorties, sinon directement le raster
  if (is.list(geom_result)) {
    geom <- geom_result[[1]]
  } else {
    geom <- geom_result
  }
  
  # Ajouter les labels en français
  labels_fr <- c("Plat", "Pic", "Crête", "Épaulement", "Éperon", 
                 "Pente", "Creux", "Pied de pente", "Vallée", "Fosse")
  
  # Créer une table de catégories pour le raster
  vals <- terra::values(geom)
  uniq_vals <- sort(unique(vals[!is.na(vals)]))
  
  if (length(uniq_vals) > 0) {
    # Créer un data.frame avec les catégories
    cats <- data.frame(
      value = uniq_vals,
      label = labels_fr[uniq_vals]
    )
    
    # Appliquer les catégories au raster
    levels(geom) <- cats
  }
  
  return(geom)
}

#' Extraire les covariables terrain complètes
#'
#' Calcule et agrège toutes les covariables terrain (pente, aspect, géomorphons)
#' pour un champ donné à partir du MNT LiDAR.
#'
#' @param polygone Un objet `sf` représentant la zone d'intérêt ou un chemin de fichier
#' @param dossier Dossier de sortie pour sauvegarder les rasters (optionnel)
#' @param epsg Code EPSG pour la projection de sortie (défaut: 4326)
#' @param ... Arguments supplémentaires passés à `telecharger_lidar()`
#'
#' @return Une liste contenant les rasters MNT, pente, aspect et géomorphons
#'   (les géomorphons sont calculés aux 3 échelles: petit, moyen, grand)
#' @export
#'
#' @examples
#' \dontrun{
#' # Extraire toutes les covariables terrain pour un champ
#' covariables <- extraire_covariables_terrain("champ.shp")
#'
#' # Accéder aux résultats
#' plot(covariables$mnt)
#' plot(covariables$pente)
#' plot(covariables$aspect)
#' plot(covariables$geomorphons$petit)   # Échelle 50m
#' plot(covariables$geomorphons$moyen)   # Échelle 100m
#' plot(covariables$geomorphons$grand)   # Échelle 200m
#' }
#'
#' @importFrom methods is
extraire_covariables_terrain <- function(polygone, search_radius = 100,
                                          dossier = NULL, epsg = 4326, ...) {
  
  # Télécharger le MNT
  message("=== Étape 1: Téléchargement du LiDAR ===")
  mnt <- telecharger_lidar(polygone, dossier = dossier, mne = FALSE, epsg = epsg, ...)
  
  if (is.null(mnt)) {
    stop("Impossible d'obtenir les données LiDAR")
  }
  
  # Calculer les covariables
  message("\n=== Étape 2: Calcul des covariables terrain ===")
  
  # Pente
  fichier_pente <- if (!is.null(dossier)) {
    file.path(dossier, "pente.tif")
  } else NULL
  pente <- calculer_pente(mnt, filename = fichier_pente)
  
  # Aspect
  fichier_aspect <- if (!is.null(dossier)) {
    file.path(dossier, "aspect.tif")
  } else NULL
  aspect <- calculer_aspect(mnt, filename = fichier_aspect)
  
  # Géomorphons (aux 3 échelles par défaut)
  message("\n--- Calcul des géomorphons aux 3 échelles ---")
  fichier_geom <- if (!is.null(dossier)) {
    file.path(dossier, "geomorphons.tif")
  } else NULL
  
  geom_list <- calculer_geomorphons(mnt, multi_scale = TRUE, filename = fichier_geom)
  
  # Renommer pour compatibilité
  geom <- geom_list
  
  message("\n=== Covariables terrain extraites avec succès ===")
  
  # Retourner les résultats
  resultat <- list(
    mnt = mnt,
    pente = pente,
    aspect = aspect,
    geomorphons = geom
  )
  
  return(resultat)
}

#' Télécharger les données LiDAR pour une zone donnée
#'
#' Cette fonction interroge l'API STAC de RNCan pour télécharger les données LiDAR (MNE ou MNT)
#' correspondant à une géométrie fournie, puis les recadre à cette zone d'intérêt.
#' Par défaut, ne retient que la couverture la plus récente et complète.
#'
#' @param polygone Un objet `sf` représentant la zone d'intérêt ou un chemin vers un fichier vectoriel (`.shp`, `.gpkg`, etc.)
#' @param dossier Dossier de sortie pour enregistrer les fichiers raster (optionnel)
#' @param mne Logique. Si TRUE, télécharge le MNE (modèle de surface). Sinon, le MNT (modèle de terrain).
#' @param recent Logique. Si TRUE (par défaut), conserve uniquement la version la plus récente.
#' @param epsg Code EPSG pour la projection de sortie (défaut: 4326 - WGS84)
#'
#' @return Un objet `SpatRaster` contenant le MNT ou MNE recadré
#' @export
#'
#' @examples
#' \dontrun{
#' # Télécharger le MNT pour un champ
#' champ <- sf::st_read("champ.shp")
#' mnt <- telecharger_lidar(champ)
#'
#' # Télécharger le MNE
#' mne <- telecharger_lidar(champ, mne = TRUE)
#' }
#'
#' @importFrom sf st_read st_crs st_transform st_union st_bbox st_coordinates st_centroid
#' @importFrom rstac stac stac_search get_request assets_url items_datetime
#' @importFrom lubridate as_datetime year
#' @importFrom terra rast crop mask writeRaster varnames vect crs ext project
#' @importFrom methods is
telecharger_lidar <- function(polygone, dossier = NULL, mne = FALSE, recent = TRUE, epsg = 4326) {
  
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
  
  # Garder le CRS original pour la sortie finale
  crs_sortie <- sf::st_crs(polygone)$epsg
  if (is.na(crs_sortie)) crs_sortie <- 4326
  message("CRS du polygone d'entrée: EPSG:", crs_sortie)
  
  # Transformer en WGS84 et unir les géométries
  polygone_wgs84 <- polygone |> 
    sf::st_transform(4326) |> 
    sf::st_union()
  
  # Interroger l'API STAC de RNCan
  bbox_poly <- as.numeric(sf::st_bbox(polygone_wgs84))
  message("Recherche de données LiDAR pour la bbox: ", paste(round(bbox_poly, 6), collapse = ", "))
  
  stac_query <- rstac::stac("https://datacube.services.geo.ca/stac/api/", force_version = TRUE) |>
    rstac::stac_search(
      collections = "hrdem-lidar",
      bbox = bbox_poly,
      limit = 100
    ) |>
    rstac::get_request()
  
  # Vérifier si des résultats sont disponibles
  nb_resultats <- 0
  if (!is.null(stac_query$numberMatched)) {
    nb_resultats <- stac_query$numberMatched
  } else if (!is.null(stac_query$numberReturned)) {
    nb_resultats <- stac_query$numberReturned
  } else if (!is.null(stac_query$features)) {
    nb_resultats <- length(stac_query$features)
  }
  
  if (nb_resultats == 0) {
    warning("Aucune donnée LiDAR disponible pour cette zone.")
    return(NULL)
  }
  
  message("Nombre de tuiles trouvées: ", nb_resultats)
  
  # Extraire les dates et URLs
  datetimes <- lubridate::as_datetime(rstac::items_datetime(stac_query))
  annees <- lubridate::year(datetimes)
  urls <- rstac::assets_url(stac_query, ifelse(mne, "dsm", "dtm"))
  
  if (length(urls) == 0) {
    warning("Aucune URL trouvée pour ", ifelse(mne, "le MNE", "le MNT"))
    return(NULL)
  }
  
  # Créer le dossier de sortie si nécessaire
  if (!is.null(dossier)) {
    dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Organiser les URLs par année
  annees_uniques <- sort(unique(annees), decreasing = TRUE)
  message("\nAnnées disponibles: ", paste(annees_uniques, collapse = ", "))
  
  # Essayer chaque année (de la plus récente à la plus ancienne)
  r <- NULL
  annee_selectionnee <- NULL
  
  for (annee_courante in annees_uniques) {
    message("\n", paste(rep("=", 60), collapse = ""))
    message("Test des données de l'année ", annee_courante)
    message(paste(rep("=", 60), collapse = ""))
    
    # Filtrer les URLs pour cette année
    urls_annee <- urls[annees == annee_courante]
    message("Nombre de tuiles pour ", annee_courante, ": ", length(urls_annee))
    
    # Afficher les URLs
    for (i in seq_along(urls_annee)) {
      message("  ", i, ": ", basename(urls_annee[i]))
    }
    
    # Essayer chaque tuile de cette année
    for (i in seq_along(urls_annee)) {
      url <- urls_annee[i]
      message("\n  Essai tuile ", i, "/", length(urls_annee), ": ", basename(url))
      
      r_temp <- try(terra::rast(url, vsi = TRUE), silent = TRUE)
      
      if (inherits(r_temp, "try-error")) {
        message("    ✗ Impossible de charger")
        next
      }
      
      # Transformer le polygone dans le CRS de cette tuile
      polygone_test <- sf::st_transform(polygone_wgs84, terra::crs(r_temp))
      bbox_poly_test <- sf::st_bbox(polygone_test)
      bbox_r_test <- terra::ext(r_temp)
      
      # Vérifier si le polygone est dans cette tuile
      if (bbox_poly_test[1] > bbox_r_test[2] || bbox_poly_test[3] < bbox_r_test[1] ||
          bbox_poly_test[2] > bbox_r_test[4] || bbox_poly_test[4] < bbox_r_test[3]) {
        message("    ✗ Polygone hors tuile")
        next
      }
      
      # Tester un crop sur une petite zone au centre du polygone
      centroid_test <- sf::st_coordinates(sf::st_centroid(polygone_test))
      x <- centroid_test[1]
      y <- centroid_test[2]
      test_ext <- terra::ext(x-10, x+10, y-10, y+10)
      
      r_test <- try(terra::crop(r_temp, test_ext), silent = TRUE)
      if (!inherits(r_test, "try-error")) {
        vals_test <- terra::values(r_test)
        nb_na_test <- sum(!is.na(vals_test))
        message("    Test zone 20x20m: ", nb_na_test, " pixels non-NA")
        
        if (nb_na_test > 0) {
          message("    ✓✓✓ Données valides trouvées!")
          r <- r_temp
          annee_selectionnee <- annee_courante
          break
        }
      }
      
      message("    ✗ Pas de données valides")
    }
    
    # Si on a trouvé une tuile valide, arrêter de chercher
    if (!is.null(r)) {
      break
    }
    
    message("\n  Aucune tuile valide pour l'année ", annee_courante, ", passage à l'année suivante...")
  }
  
  if (is.null(r)) {
    warning("\nAucune tuile de aucune année n'a pu être chargée avec des données valides.")
    return(NULL)
  }
  
  message("\n" , paste(rep("=", 60), collapse = ""))
  message("✓✓✓ Année ", annee_selectionnee, " sélectionnée avec succès ✓✓✓")
  message(paste(rep("=", 60), collapse = ""))
  
  message("\n" , paste(rep("=", 50), collapse = ""))
  message("Tuile sélectionnée avec succès")
  message(paste(rep("=", 50), collapse = ""))
  message("Dimensions: ", terra::nrow(r), " x ", terra::ncol(r), " pixels")
  message("Résolution: ", paste(round(terra::res(r), 4), collapse = " x "), " m")
  message("CRS: ", terra::crs(r, describe = TRUE)$name)
  message("Extent: ", paste(round(terra::ext(r)[], 2), collapse = ", "))
  
  # Transformer le polygone dans le CRS du raster
  message("\nTransformation du polygone dans le CRS du raster...")
  crs_raster <- terra::crs(r)
  message("CRS cible: ", substr(crs_raster, 1, 100), "...")
  
  polygone_raster <- sf::st_transform(polygone_wgs84, crs_raster)
  
  # Afficher les coordonnées du polygone dans les deux CRS
  centroid_wgs84 <- sf::st_centroid(polygone_wgs84)
  coords_wgs84 <- sf::st_coordinates(centroid_wgs84)
  message("Centroïde du polygone (WGS84): ", round(coords_wgs84[1], 6), ", ", round(coords_wgs84[2], 6))
  
  centroid_raster <- sf::st_centroid(polygone_raster)
  coords_raster <- sf::st_coordinates(centroid_raster)
  message("Centroïde du polygone (CRS raster): ", round(coords_raster[1], 2), ", ", round(coords_raster[2], 2))
  
  # Vérifier que le polygone est dans l'étendue du raster
  bbox_poly_r <- sf::st_bbox(polygone_raster)
  bbox_r <- terra::ext(r)
  message("\nBBox polygone (CRS raster): ", paste(round(bbox_poly_r, 2), collapse = ", "))
  message("  Format sf: xmin=", bbox_poly_r[1], ", ymin=", bbox_poly_r[2], 
          ", xmax=", bbox_poly_r[3], ", ymax=", bbox_poly_r[4])
  message("BBox raster: ", paste(round(bbox_r[], 2), collapse = ", "))
  message("  Format terra: xmin=", bbox_r[1], ", xmax=", bbox_r[2], 
          ", ymin=", bbox_r[3], ", ymax=", bbox_r[4])
  
  # Vérifier l'intersection
  # bbox_poly_r (sf): [1]=xmin, [2]=ymin, [3]=xmax, [4]=ymax
  # bbox_r (terra): [1]=xmin, [2]=xmax, [3]=ymin, [4]=ymax
  poly_xmin <- bbox_poly_r[1]
  poly_ymin <- bbox_poly_r[2]
  poly_xmax <- bbox_poly_r[3]
  poly_ymax <- bbox_poly_r[4]
  
  rast_xmin <- bbox_r[1]
  rast_xmax <- bbox_r[2]
  rast_ymin <- bbox_r[3]
  rast_ymax <- bbox_r[4]
  
  message("\nVérification de l'intersection:")
  message("  Poly X [", round(poly_xmin, 0), ", ", round(poly_xmax, 0), "] dans Raster X [", 
          round(rast_xmin, 0), ", ", round(rast_xmax, 0), "] ?")
  message("  Poly Y [", round(poly_ymin, 0), ", ", round(poly_ymax, 0), "] dans Raster Y [", 
          round(rast_ymin, 0), ", ", round(rast_ymax, 0), "] ?")
  
  # Vérifier si le polygone est entièrement hors du raster
  if (poly_xmin > rast_xmax || poly_xmax < rast_xmin ||
      poly_ymin > rast_ymax || poly_ymax < rast_ymin) {
    warning("\nLe polygone est en dehors de l'étendue du raster!")
    return(NULL)
  }
  
  message("  ✓ Le polygone est dans l'étendue du raster")
  
  # Recadrer le raster
  message("\nRecadrage du raster...")
  polygone_vect <- terra::vect(polygone_raster)
  
  # Utiliser crop avec mask=FALSE d'abord
  r_crop <- terra::crop(r, polygone_vect, snap = "out")
  message("Raster recadré: ", terra::ncol(r_crop), " x ", terra::nrow(r_crop), " pixels")
  
  # Vérifier les valeurs après crop
  vals_crop <- terra::values(r_crop)
  nb_non_na <- sum(!is.na(vals_crop))
  message("Pixels non-NA après crop: ", nb_non_na, " sur ", length(vals_crop))
  
  if (nb_non_na == 0) {
    warning("Toutes les valeurs sont NA après crop. Test avec une petite zone...")
    # Essayer de lire une petite zone autour du centroïde
    x <- coords_raster[1]
    y <- coords_raster[2]
    small_ext <- terra::ext(x-50, x+50, y-50, y+50)
    r_test <- terra::crop(r, small_ext)
    vals_test <- terra::values(r_test)
    message("Test avec zone 100x100m au centre: ", sum(!is.na(vals_test)), " pixels non-NA")
    if (sum(!is.na(vals_test)) > 0) {
      message("Valeurs dans la zone test: ", paste(head(vals_test[!is.na(vals_test)], 5), collapse = ", "))
    }
  }
  
  # Appliquer le masque
  if (nb_non_na > 0) {
    r_crop <- terra::mask(r_crop, polygone_vect)
    vals_mask <- terra::values(r_crop)
    nb_non_na_mask <- sum(!is.na(vals_mask))
    message("Pixels non-NA après mask: ", nb_non_na_mask)
  }
  
  # Transformer dans le CRS demandé si nécessaire
  if (!is.na(epsg) && epsg != crs_sortie) {
    message("\nReprojection vers EPSG:", epsg, "...")
    r_crop <- terra::project(r_crop, paste0("EPSG:", epsg))
  }
  
  # Nommer le raster avec l'année
  names(r_crop) <- ifelse(mne, "MNE", "MNT")
  terra::varnames(r_crop) <- paste0(ifelse(mne, "mne_", "mnt_"), annee_selectionnee)
  
  # Vérifier les valeurs finales
  vals <- terra::values(r_crop)
  nb_non_na_final <- sum(!is.na(vals))
  message("\n=== RÉSULTAT FINAL ===")
  message("Nombre de pixels non-NA: ", nb_non_na_final, " sur ", length(vals))
  
  if (nb_non_na_final == 0) {
    warning("Toutes les valeurs sont NA après traitement!")
  } else {
    vals_non_na <- vals[!is.na(vals)]
    message("Statistiques altitudes: min=", round(min(vals_non_na), 2), 
            " max=", round(max(vals_non_na), 2), 
            " moy=", round(mean(vals_non_na), 2))
  }
  
  # Sauvegarder si un dossier est spécifié
  if (!is.null(dossier)) {
    fichier_sortie <- file.path(
      dossier,
      paste0(ifelse(mne, "mne_", "mnt_"), annee_selectionnee, ".tif")
    )
    terra::writeRaster(r_crop, fichier_sortie, overwrite = TRUE)
    message("\nRaster sauvegardé: ", fichier_sortie)
  }
  
  return(r_crop)
}

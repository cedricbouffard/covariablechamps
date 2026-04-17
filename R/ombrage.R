#' Calculer l'ombrage d'une parcelle à partir du LiDAR et de la position du soleil
#'
#' Cette fonction calcule l'ombrage projeté (cast shadows) d'une parcelle agricole en utilisant 
#' le MNE (Modèle Numérique de Surface) issu du LiDAR et les positions du soleil calculées 
#' avec le package suncalc. Elle utilise le package rayshader pour calculer de vraies ombres 
#' projetées par les éléments de surface (arbres, bâtiments).
#'
#' @param polygone Un objet `sf` représentant la zone d'intérêt ou un chemin vers un fichier vectoriel
#' @param date Date pour laquelle calculer l'ombrage (format Date ou chaîne "YYYY-MM-DD"). Par défaut, la date du jour.
#' @param intervalle_heures Intervalle en heures entre chaque calcul (défaut: 1 heure)
#' @param dossier Dossier de sortie pour sauvegarder les résultats (optionnel)
#' @param mne Objet SpatRaster optionnel contenant le MNE déjà chargé. Si NULL, télécharge automatiquement.
#' @param tz Fuseau horaire (défaut: "America/Toronto")
#' @param zscale Facteur d'échelle pour la hauteur (défaut: 1). Utilisez une valeur plus élevée (ex: 10) si les ombres semblent trop courtes, ou plus faible (ex: 0.5) si elles semblent trop longues.
#' @param max_distance Distance maximale de projection des ombres en mètres (défaut: 1000)
#' @param lambert Si TRUE, applique l'ombrage de Lambert (défaut: TRUE)
#' @param seuil_ensoleillement Seuil pour considérer un pixel comme ensoleillé (défaut: 0.1). 
#'   Les valeurs d'ombrage > seuil sont comptées comme ensoleillées. 
#'   Valeur plus basse = plus permissif (compte plus d'heures).
#'   Valeur plus haute = plus strict (compte moins d'heures).
#'
#' @return Une liste contenant :
#'   \item{ombrage_par_heure}{SpatRaster avec une couche par heure calculée (0-1, où 0 = ombre complète, 1 = plein soleil)}
#'   \item{ombrage_moyen}{SpatRaster avec l'ombrage moyen sur la journée (0-1)}
#'   \item{heures_ensoleillement}{SpatRaster avec le nombre d'heures d'ensoleillement par pixel}
#'   \item{info_soleil}{Data.frame avec les informations sur les positions du soleil par heure}
#'   \item{mne}{Le MNE utilisé pour les calculs}
#'   \item{zscale}{Le facteur d'échelle utilisé}
#'   \item{max_distance}{La distance maximale de projection utilisée}
#'   \item{seuil_ensoleillement}{Le seuil d'ensoleillement utilisé}
#'
#' @details
#' L'ombrage est calculé en utilisant la fonction `rayshader::ray_shade()` qui effectue 
#' un lancer de rayons (raytracing) pour déterminer quels pixels sont en ombre. Contrairement 
#' au hillshade classique, cette méthode calcule les ombres projetées par les éléments élevés 
#' (arbres, bâtiments) sur les zones avoisinantes.
#' 
#' Le paramètre `zscale` permet d'ajuster l'échelle verticale : 
#' - zscale > 1 : ombres plus longues (exagère la hauteur)
#' - zscale < 1 : ombres plus courtes (minimise la hauteur)
#' - zscale = 1 : échelle réelle
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculer l'ombrage pour aujourd'hui
#' champ <- sf::st_read("champ.shp")
#' resultats <- calculer_ombrage(champ)
#'
#' # Calculer pour une date spécifique avec ajustement de l'échelle
#' resultats <- calculer_ombrage(champ, date = "2024-06-21", zscale = 2)
#'
#' # Augmenter la distance de projection des ombres
#' resultats <- calculer_ombrage(champ, date = "2024-06-21", 
#'                               zscale = 1.5, max_distance = 1000)
#'
#' # Visualiser les résultats
#' terra::plot(resultats$ombrage_moyen, main = "Ombrage moyen")
#' terra::plot(resultats$heures_ensoleillement, main = "Heures d'ensoleillement")
#' }
#'
#' @importFrom sf st_read st_crs st_transform st_centroid st_coordinates st_union
#' @importFrom terra terrain writeRaster nlyr app ifel crs project ext res ncell nrow ncol values
#' @importFrom suncalc getSunlightTimes getSunlightPosition
#' @importFrom lubridate hour minute floor_date ceiling_date hours
#' @importFrom methods is
calculer_ombrage <- function(polygone, date = Sys.Date(), intervalle_heures = 1,
                               dossier = NULL, mne = NULL, tz = "America/Toronto",
                               zscale = 1, max_distance = 1000, lambert = TRUE,
                               seuil_ensoleillement = 0.1) {

  # Verifier que suncalc est disponible
  if (!requireNamespace("suncalc", quietly = TRUE)) {
    stop("Le package 'suncalc' est requis. Installez-le avec:\n",
         "install.packages('suncalc')")
  }
  
  # Verifier que rayshader est disponible
  if (!requireNamespace("rayshader", quietly = TRUE)) {
    stop("Le package 'rayshader' est requis. Installez-le avec:\n",
         "install.packages('rayshader')")
  }

  # Convertir la date si necessaire
  if (is.character(date)) {
    date <- as.Date(date)
  }

  # Lire le polygone si c'est un chemin de fichier
  if (is.character(polygone)) {
    polygone <- sf::st_read(polygone, quiet = TRUE)
  }

  # Verifier que c'est bien un objet sf
  if (!methods::is(polygone, "sf")) {
    stop("Le polygone doit etre un objet sf ou un chemin vers un fichier vectoriel")
  }

  # Assigner WGS84 si pas de CRS defini
  if (is.na(sf::st_crs(polygone))) {
    sf::st_crs(polygone) <- 4326
  }

  # Obtenir le centroide pour les coordonnees du soleil
  polygone_wgs84 <- sf::st_transform(polygone, 4326)
  centroid <- sf::st_centroid(sf::st_union(polygone_wgs84))
  coords <- sf::st_coordinates(centroid)
  lat <- coords[2]
  lon <- coords[1]

  message("Coordonnees du centroide: ", round(lat, 6), "N, ", round(lon, 6), "E")
  message("Date: ", format(date, "%d/%m/%Y"))
  message("Parametres - zscale: ", zscale, ", max_distance: ", max_distance, "m")

  # Telecharger le MNE si non fourni
  if (is.null(mne)) {
    message("\n=== Telechargement du MNE (Modele Numerique de Surface) ===")
    mne <- telecharger_lidar(polygone, mne = TRUE, epsg = sf::st_crs(polygone)$epsg)
    if (is.null(mne)) {
      stop("Impossible d'obtenir les donnees LiDAR")
    }
  }

  # Verifier que le MNE est un SpatRaster
  if (!methods::is(mne, "SpatRaster")) {
    stop("Le MNE doit etre un objet SpatRaster")
  }

  # Obtenir les heures de lever et coucher du soleil
  message("\n=== Calcul des heures de lever et coucher du soleil ===")
  times <- suncalc::getSunlightTimes(
    date = date,
    lat = lat,
    lon = lon,
    tz = tz,
    keep = c("sunrise", "sunset", "solarNoon")
  )

  message("Lever du soleil: ", format(times$sunrise, "%H:%M"))
  message("Coucher du soleil: ", format(times$sunset, "%H:%M"))
  message("Midi solaire: ", format(times$solarNoon, "%H:%M"))

  # Creer la sequence des heures a calculer
  heure_debut <- lubridate::floor_date(times$sunrise, unit = "hour")
  heure_fin <- ceiling_date(times$sunset, unit = "hour")

  heures_seq <- seq(
    from = heure_debut,
    to = heure_fin,
    by = paste0(intervalle_heures, " hours")
  )

  message("\nHeures de calcul: ", length(heures_seq), " pas de temps")

  # Calculer la position du soleil pour chaque heure
  message("\n=== Calcul des positions du soleil ===")
  positions_soleil <- lapply(heures_seq, function(h) {
    pos <- suncalc::getSunlightPosition(
      date = h,
      lat = lat,
      lon = lon
    )
    # Convertir l'azimuth du format suncalc (sud=0, ouest=PI/2)
    # vers le format standard (nord=0, est=90)
    azimuth_deg <- (pos$azimuth * 180 / pi + 180) %% 360
    altitude_deg <- pos$altitude * 180 / pi

    list(
      heure = h,
      azimuth = azimuth_deg,
      altitude = altitude_deg,
      elevation = altitude_deg
    )
  })

  # Creer un data.frame avec les informations
  info_soleil <- data.frame(
    heure = heures_seq,
    heure_str = format(heures_seq, "%H:%M"),
    azimuth = sapply(positions_soleil, function(x) x$azimuth),
    altitude = sapply(positions_soleil, function(x) x$altitude),
    ensoleille = sapply(positions_soleil, function(x) x$altitude > 0)
  )

  # Filtrer uniquement les heures ou le soleil est au-dessus de l'horizon
  heures_enseillees <- info_soleil[info_soleil$ensoleille, ]

  if (nrow(heures_enseillees) == 0) {
    warning("Le soleil n'est pas au-dessus de l'horizon pour cette date/zone!")
    return(NULL)
  }

  message("\nHeures avec soleil au-dessus de l'horizon: ", nrow(heures_enseillees))

  # Preparer le MNE pour rayshader
  message("\n=== Preparation du MNE pour le calcul des ombres ===")
  
  # Convertir le raster en matrice pour rayshader
  message("Conversion du MNE en matrice pour rayshader...")
  mne_matrix <- rayshader::raster_to_matrix(mne)
  
  # Remplacer les NA par la valeur minimale pour eviter les problemes
  min_val <- min(mne_matrix, na.rm = TRUE)
  if (is.finite(min_val)) {
    mne_matrix[is.na(mne_matrix)] <- min_val
  } else {
    mne_matrix[is.na(mne_matrix)] <- 0
  }
  
  message("Dimensions de la matrice: ", nrow(mne_matrix), " x ", ncol(mne_matrix))
  
  # Obtenir la resolution du MNE
  res_mne <- terra::res(mne)[1]
  
  # Calculer maxsearch en pixels a partir de la distance maximale
  maxsearch_pixels <- round(max_distance / res_mne)
  message("Distance max d'ombre: ", max_distance, "m = ", maxsearch_pixels, " pixels")

  # Calculer l'ombrage pour chaque heure ensoleillee
  message("\n=== Calcul des ombres projetees avec rayshader ===")

  # Liste pour stocker les rasters d'ombrage
  liste_ombrage <- list()
  noms_couches <- c()

  for (i in seq_len(nrow(heures_enseillees))) {
    heure_info <- heures_enseillees[i, ]
    message("Calcul pour ", heure_info$heure_str,
            " (azimuth: ", round(heure_info$azimuth, 1), "",
            ", altitude: ", round(heure_info$altitude, 1), ")")

    # Calculer les ombres avec ray_shade
    # sunaltitude : angle au-dessus de l'horizon
    # sunangle : azimuth en degres (0 = nord, 90 = est, etc.)
    shadow_matrix <- tryCatch({
      rayshader::ray_shade(
        heightmap = mne_matrix,
        sunaltitude = heure_info$altitude,
        sunangle = heure_info$azimuth,
        zscale = zscale,
        maxsearch = maxsearch_pixels,
        lambert = lambert,
        progbar = FALSE
      )
    }, error = function(e) {
      warning("Erreur lors du calcul des ombres pour ", heure_info$heure_str, ": ", e$message)
      # Retourner une matrice de 1 (pas d'ombre) en cas d'erreur
      matrix(1, nrow = nrow(mne_matrix), ncol = ncol(mne_matrix))
    })
    
    # Convertir en raster terra - copier la structure du MNE original
    ombrage_heure <- terra::rast(mne)  # Copie la structure (extent, resolution, crs)
    terra::values(ombrage_heure) <- as.vector(shadow_matrix)  # Remplir avec les valeurs d'ombre
    ombrage_heure=terra::flip(ombrage_heure, direction= "horizontal")
    # Verifier que les dimensions correspondent
    if (terra::nrow(ombrage_heure) != nrow(shadow_matrix) || 
        terra::ncol(ombrage_heure) != ncol(shadow_matrix)) {
      warning("Dimensions mismatch when creating shadow raster")
    }
    
    # Remplacer les valeurs NA du MNE original par NA dans l'ombrage
    ombrage_heure <- terra::mask(ombrage_heure, mne)

    liste_ombrage[[length(liste_ombrage) + 1]] <- ombrage_heure
    noms_couches <- c(noms_couches, paste0("H", heure_info$heure_str))
  }

  # Combiner tous les rasters d'ombrage en un seul avec plusieurs couches
  if (length(liste_ombrage) > 0) {
    message("\n=== Combinaison des couches d'ombrage ===")

    # Creer un SpatRaster multi-couches
    ombrage_par_heure <- do.call(c, liste_ombrage)
    names(ombrage_par_heure) <- noms_couches

    message("Nombre de couches creees: ", terra::nlyr(ombrage_par_heure))

    # Calculer l'ombrage moyen sur la journee
    message("\n=== Calcul de l'ombrage moyen ===")
    ombrage_moyen <- terra::app(ombrage_par_heure, fun = mean, na.rm = TRUE)
    names(ombrage_moyen) <- "ombrage_moyen"

    # Calculer le nombre d'heures d'ensoleillement
    message("\n=== Calcul des heures d'ensoleillement ===")
    message("Seuil d'ensoleillement utilise: ", seuil_ensoleillement)
    # Compter le nombre de couches ou l'ombrage est > seuil (ensoleille)
    # Multiplier par l'intervalle pour avoir le nombre d'heures
    heures_ensoleillement <- terra::app(ombrage_par_heure, fun = function(x) {
      sum(x > seuil_ensoleillement, na.rm = TRUE) * intervalle_heures
    })
    names(heures_ensoleillement) <- "heures_ensoleillement"

    # Sauvegarder les resultats si un dossier est specifie
    if (!is.null(dossier)) {
      dir.create(dossier, recursive = TRUE, showWarnings = FALSE)

      fichier_ombrage_heure <- file.path(dossier,
                                          paste0("ombrage_heure_", format(date, "%Y%m%d"), ".tif"))
      fichier_ombrage_moyen <- file.path(dossier,
                                          paste0("ombrage_moyen_", format(date, "%Y%m%d"), ".tif"))
      fichier_heures_ens <- file.path(dossier,
                                       paste0("heures_ensoleillement_", format(date, "%Y%m%d"), ".tif"))

      terra::writeRaster(ombrage_par_heure, fichier_ombrage_heure, overwrite = TRUE)
      terra::writeRaster(ombrage_moyen, fichier_ombrage_moyen, overwrite = TRUE)
      terra::writeRaster(heures_ensoleillement, fichier_heures_ens, overwrite = TRUE)

      message("\n=== Fichiers sauvegardes ===")
      message("Ombrage par heure: ", fichier_ombrage_heure)
      message("Ombrage moyen: ", fichier_ombrage_moyen)
      message("Heures d'ensoleillement: ", fichier_heures_ens)
    }

    # Creer le resultat final
    resultat <- list(
      ombrage_par_heure = ombrage_par_heure,
      ombrage_moyen = ombrage_moyen,
      heures_ensoleillement = heures_ensoleillement,
      info_soleil = info_soleil,
      mne = mne,
      zscale = zscale,
      max_distance = max_distance,
      seuil_ensoleillement = seuil_ensoleillement
    )

    message("\n=== Calcule termine avec succes ===")
    message("Statistiques - Ombrage moyen: min=", round(min(terra::values(ombrage_moyen), na.rm = TRUE), 3),
            ", max=", round(max(terra::values(ombrage_moyen), na.rm = TRUE), 3))
    message("Statistiques - Heures d'ensoleillement: min=", round(min(terra::values(heures_ensoleillement), na.rm = TRUE), 1),
            ", max=", round(max(terra::values(heures_ensoleillement), na.rm = TRUE), 1))

    return(resultat)

  } else {
    warning("Aucun calcul d'ombrage n'a pu etre effectue")
    return(NULL)
  }
}

#' Visualiser l'ombrage d'une parcelle
#'
#' Cree une visualisation de l'ombrage calcule avec trois panneaux :
#' le MNE, l'ombrage moyen, et les heures d'ensoleillement.
#'
#' @param resultats_ombrage Liste retournee par `calculer_ombrage()`
#' @param couche_heure Numero de la couche horaire a afficher (defaut: 1)
#' @param titre Titre optionnel pour le graphique
#'
#' @return NULL (affiche le graphique)
#' @export
#'
#' @examples
#' \dontrun{
#' resultats <- calculer_ombrage(champ, date = "2024-06-21")
#' visualiser_ombrage(resultats)
#' }
#'
#' @importFrom terra plot nlyr
calculer_ombrage_visualisation <- function(resultats_ombrage, couche_heure = 1, titre = NULL) {

  if (is.null(resultats_ombrage)) {
    stop("Les resultats d'ombrage sont NULL")
  }

  # Creer une mise en page 2x2
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  graphics::par(mfrow = c(2, 2))

  # 1. MNE
  terra::plot(resultats_ombrage$mne, main = "MNE (Modele Numerique de Surface)")

  # 2. Ombrage a une heure specifique
  if (terra::nlyr(resultats_ombrage$ombrage_par_heure) >= couche_heure) {
    terra::plot(resultats_ombrage$ombrage_par_heure[[couche_heure]],
                main = paste("Ombrage a", names(resultats_ombrage$ombrage_par_heure)[couche_heure]))
  }

  # 3. Ombrage moyen
  terra::plot(resultats_ombrage$ombrage_moyen, main = "Ombrage moyen journalier")

  # 4. Heures d'ensoleillement
  terra::plot(resultats_ombrage$heures_ensoleillement, main = "Heures d'ensoleillement")

  # Titre global
  if (!is.null(titre)) {
    graphics::mtext(titre, side = 3, line = -1.5, outer = TRUE, cex = 1.2, font = 2)
  }

  invisible(NULL)
}

#' Calculer l'ombrage sur une periode de dates
#'
#' Cette fonction calcule l'ombrage et les heures d'ensoleillement moyennes 
#' sur une periode donnee (ex: un mois, une saison). Elle agrege les resultats
#' journaliers pour donner des statistiques sur la periode.
#'
#' @param polygone Un objet `sf` representant la zone d'interet ou un chemin vers un fichier vectoriel
#' @param date_debut Date de debut (format Date ou chaine "YYYY-MM-DD")
#' @param date_fin Date de fin (format Date ou chaine "YYYY-MM-DD")
#' @param intervalle_jours Intervalle en jours entre chaque calcul (defaut: 1 = tous les jours)
#' @param intervalle_heures Intervalle en heures entre chaque calcul journalier (defaut: 1)
#' @param dossier Dossier de sortie pour sauvegarder les resultats (optionnel)
#' @param mne Objet SpatRaster optionnel contenant le MNE deja charge
#' @param tz Fuseau horaire (defaut: "America/Toronto")
#' @param zscale Facteur d'echelle pour la hauteur (defaut: 1)
#' @param max_distance Distance maximale de projection des ombres en metres (defaut: 1000)
#' @param lambert Si TRUE, applique l'ombrage de Lambert (defaut: TRUE)
#' @param seuil_ensoleillement Seuil pour considerer un pixel comme ensoleille (defaut: 0.1)
#' @param resume_type Type de resume pour les heures d'ensoleillement : "moyenne" (defaut), "min", "max", "somme"
#'
#' @return Une liste contenant :
#'   \item{heures_ensoleillement_moyen}{SpatRaster avec les heures d'ensoleillement moyennes par jour}
#'   \item{heures_ensoleillement_min}{SpatRaster avec les heures minimales sur la periode}
#'   \item{heures_ensoleillement_max}{SpatRaster avec les heures maximales sur la periode}
#'   \item{ombrage_moyen_periode}{SpatRaster avec l'ombrage moyen sur toute la periode}
#'   \item{nb_jours_calcules}{Nombre de jours effectivement calcules}
#'   \item{dates_calculees}{Vecteur des dates calculees}
#'   \item{info_soleil}{Data.frame avec les informations sur les positions du soleil}
#'   \item{mne}{Le MNE utilise pour les calculs}
#'
#' @details
#' Cette fonction calcule les statistiques d'ensoleillement sur une periode en :
#' 1. Calculant l'ombrage pour chaque jour echantillonne dans la periode
#' 2. Agregeant les heures d'ensoleillement journalieres
#' 
#' Le parametre `resume_type` determine comment agreger les heures :
#' - "moyenne" : heures moyennes par jour (defaut)
#' - "min" : heures minimales (pire cas)
#' - "max" : heures maximales (meilleur cas)
#' - "somme" : total des heures sur toute la periode
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Calculer sur un mois complet
#' champ <- sf::st_read("champ.shp")
#' resultats <- calculer_ombrage_periode(
#'   polygone = champ,
#'   date_debut = "2024-06-01",
#'   date_fin = "2024-06-30"
#' )
#'
#' # Calculer sur une saison avec un point tous les 3 jours
#' resultats_saison <- calculer_ombrage_periode(
#'   polygone = champ,
#'   date_debut = "2024-06-01",
#'   date_fin = "2024-08-31",
#'   intervalle_jours = 3
#' )
#'
#' # Visualiser les resultats
#' terra::plot(resultats$heures_ensoleillement_moyen, 
#'             main = "Heures moyennes d'ensoleillement/jour")
#' terra::plot(resultats$heures_ensoleillement_min, 
#'             main = "Heures minimales d'ensoleillement (pire cas)")
#' }
#'
#' @importFrom sf st_read st_crs
#' @importFrom terra rast writeRaster nlyr app
#' @importFrom lubridate as_date days
#' @importFrom methods is
calculer_ombrage_periode <- function(polygone, 
                                       date_debut,
                                       date_fin,
                                       intervalle_jours = 1,
                                       intervalle_heures = 1,
                                       dossier = NULL,
                                       mne = NULL,
                                       tz = "America/Toronto",
                                       zscale = 1,
                                       max_distance = 1000,
                                       lambert = TRUE,
                                       seuil_ensoleillement = 0.1,
                                       resume_type = "moyenne") {
  
  # Verifier les packages
  if (!requireNamespace("suncalc", quietly = TRUE)) {
    stop("Le package 'suncalc' est requis.")
  }
  if (!requireNamespace("rayshader", quietly = TRUE)) {
    stop("Le package 'rayshader' est requis.")
  }
  
  # Convertir les dates
  if (is.character(date_debut)) date_debut <- as.Date(date_debut)
  if (is.character(date_fin)) date_fin <- as.Date(date_fin)
  
  # Lire le polygone si necessaire
  if (is.character(polygone)) {
    polygone <- sf::st_read(polygone, quiet = TRUE)
  }
  
  if (!methods::is(polygone, "sf")) {
    stop("Le polygone doit etre un objet sf ou un chemin vers un fichier")
  }
  
  # Creer la sequence de dates
  dates_seq <- seq(from = date_debut, to = date_fin, by = paste0(intervalle_jours, " days"))
  message("=== Calcul de l'ombrage sur ", length(dates_seq), " jours ===")
  message("Periode: ", format(date_debut, "%d/%m/%Y"), " au ", format(date_fin, "%d/%m/%Y"))
  message("Dates calculees: ", paste(format(dates_seq, "%d/%m"), collapse = ", "))
  
  # Telecharger le MNE si necessaire
  if (is.null(mne)) {
    message("\n=== Telechargement du MNE ===")
    mne <- telecharger_lidar(polygone, mne = TRUE, epsg = sf::st_crs(polygone)$epsg)
    if (is.null(mne)) stop("Impossible d'obtenir les donnees LiDAR")
  }
  
  # Liste pour stocker les heures d'ensoleillement de chaque jour
  liste_heures_jour <- list()
  liste_ombrage_moyen <- list()
  dates_calculees <- c()
  
  # Boucle sur les dates
  for (i in seq_along(dates_seq)) {
    date_courante <- dates_seq[i]
    message("\n", paste(rep("=", 60), collapse = ""))
    message("Jour ", i, "/", length(dates_seq), ": ", format(date_courante, "%d/%m/%Y"))
    message(paste(rep("=", 60), collapse = ""))
    
    # Calculer l'ombrage pour ce jour
    resultat_jour <- tryCatch({
      calculer_ombrage(
        polygone = polygone,
        date = date_courante,
        intervalle_heures = intervalle_heures,
        dossier = NULL,  # Ne pas sauvegarder les resultats intermediaires
        mne = mne,
        tz = tz,
        zscale = zscale,
        max_distance = max_distance,
        lambert = lambert,
        seuil_ensoleillement = seuil_ensoleillement
      )
    }, error = function(e) {
      warning("Erreur pour le ", format(date_courante, "%d/%m/%Y"), ": ", e$message)
      NULL
    })
    
    if (!is.null(resultat_jour)) {
      liste_heures_jour[[length(liste_heures_jour) + 1]] <- resultat_jour$heures_ensoleillement
      liste_ombrage_moyen[[length(liste_ombrage_moyen) + 1]] <- resultat_jour$ombrage_moyen
      dates_calculees <- c(dates_calculees, as.character(date_courante))
      message("Jour calcule avec succes")
    } else {
      message("Echec du calcul pour ce jour")
    }
  }
  
  # Verifier qu'on a des resultats
  if (length(liste_heures_jour) == 0) {
    stop("Aucun jour n'a pu etre calcule")
  }
  
  message("\n", paste(rep("=", 60), collapse = ""))
  message("=== Agregation des resultats ===")
  message(paste(rep("=", 60), collapse = ""))
  message("Nombre de jours calcules: ", length(liste_heures_jour))
  
  # Combiner tous les rasters d'heures
  heures_stack <- do.call(c, liste_heures_jour)
  ombrage_stack <- do.call(c, liste_ombrage_moyen)
  
  # Calculer les statistiques selon le type de resume
  message("\nCalcul des statistiques (type: ", resume_type, ")...")
  
  if (resume_type == "moyenne") {
    # Heures moyennes par jour
    heures_resultat <- terra::app(heures_stack, fun = mean, na.rm = TRUE)
    nom_couche <- "heures_moyennes_par_jour"
  } else if (resume_type == "min") {
    # Heures minimales (pire cas)
    heures_resultat <- terra::app(heures_stack, fun = min, na.rm = TRUE)
    nom_couche <- "heures_minimales"
  } else if (resume_type == "max") {
    # Heures maximales (meilleur cas)
    heures_resultat <- terra::app(heures_stack, fun = max, na.rm = TRUE)
    nom_couche <- "heures_maximales"
  } else if (resume_type == "somme") {
    # Somme totale sur la periode
    heures_resultat <- terra::app(heures_stack, fun = sum, na.rm = TRUE)
    nom_couche <- "heures_totales"
  } else {
    stop("Type de resume non reconnu: ", resume_type, 
". Utilisez 'moyenne', 'min', 'max' ou 'somme'")
  }
  
  names(heures_resultat) <- nom_couche
  
  # Calculer min, max et moyenne sur la periode (toujours utiles)
  heures_min <- terra::app(heures_stack, fun = min, na.rm = TRUE)
  heures_max <- terra::app(heures_stack, fun = max, na.rm = TRUE)
  heures_moy <- terra::app(heures_stack, fun = mean, na.rm = TRUE)
  
  names(heures_min) <- "heures_min_periode"
  names(heures_max) <- "heures_max_periode"
  names(heures_moy) <- "heures_moy_periode"
  
  # Ombrage moyen sur toute la periode
  ombrage_moyen_periode <- terra::app(ombrage_stack, fun = mean, na.rm = TRUE)
  names(ombrage_moyen_periode) <- "ombrage_moyen_periode"
  
  # Sauvegarder si dossier specifie
  if (!is.null(dossier)) {
    dir.create(dossier, recursive = TRUE, showWarnings = FALSE)
    
    fichier_heures <- file.path(dossier, paste0("heures_", resume_type, "_", 
                                                 format(date_debut, "%Y%m%d"), "_", 
                                                 format(date_fin, "%Y%m%d"), ".tif"))
    fichier_stats <- file.path(dossier, paste0("heures_stats_", 
                                                format(date_debut, "%Y%m%d"), "_", 
                                                format(date_fin, "%Y%m%d"), ".tif"))
    fichier_ombrage <- file.path(dossier, paste0("ombrage_moyen_", 
                                                  format(date_debut, "%Y%m%d"), "_", 
                                                  format(date_fin, "%Y%m%d"), ".tif"))
    
    terra::writeRaster(heures_resultat, fichier_heures, overwrite = TRUE)
    terra::writeRaster(c(heures_min, heures_max, heures_moy), fichier_stats, overwrite = TRUE)
    terra::writeRaster(ombrage_moyen_periode, fichier_ombrage, overwrite = TRUE)
    
    message("\n=== Fichiers sauvegardes ===")
    message("Heures (", resume_type, "): ", fichier_heures)
    message("Statistiques (min/max/moy): ", fichier_stats)
    message("Ombrage moyen: ", fichier_ombrage)
  }
  
  # Creer le resultat final
  resultat <- list(
    heures_ensoleillement_moyen = heures_moy,
    heures_ensoleillement_min = heures_min,
    heures_ensoleillement_max = heures_max,
    heures_ensoleillement_resume = heures_resultat,
    ombrage_moyen_periode = ombrage_moyen_periode,
    nb_jours_calcules = length(liste_heures_jour),
    dates_calculees = as.Date(dates_calculees),
    resume_type = resume_type,
    periode = list(debut = date_debut, fin = date_fin),
    mne = mne
  )
  
  message("\n=== Calcule termine avec succes ===")
  message("Statistiques sur la periode:")
  message("- Heures min: ", round(min(terra::values(heures_min), na.rm = TRUE), 1), 
          " a max: ", round(max(terra::values(heures_max), na.rm = TRUE), 1))
  message("- Heures moyennes par jour: ", round(mean(terra::values(heures_moy), na.rm = TRUE), 1))
  message("- Ombrage moyen periode: min=", round(min(terra::values(ombrage_moyen_periode), na.rm = TRUE), 3),
          ", max=", round(max(terra::values(ombrage_moyen_periode), na.rm = TRUE), 3))
  
  return(resultat)
}

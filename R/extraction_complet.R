#' Extraire l'ensemble des covariables pour un champ
#'
#' Cette méta-fonction orchestre l'extraction complète des covariables
#' pour un polygone donné: terrain, pédologie, arbres, ombrage et vent.
#' Elle retourne également un raster multi-bandes consolidé avec toutes
#' les covariables alignées sur le MNT.
#'
#' @param polygone Un objet `sf` représentant le champ/parcelle
#' @param buffer Distance du buffer en mètres (défaut: 100)
#' @param covariables Liste des covariables à calculer. Par défaut, toutes sont activées:
#'   - "terrain": MNT, pente, aspect, géomorphons
#'   - "pedologie": Extraction et désagrégation des sols
#'   - "lidar_ponctuel": Nuage de points LiDAR
#'   - "arbres": Détection et classification des arbres/haies
#'   - "ombrage": Calcul de l'ombrage (1er mai - 31 août)
#'   - "vent_dominant": Rose des vents et direction dominante
#'   - "distances_vent": Distances amont/aval (fetch de vent)
#'   - "distance_arbres": Distance euclidienne aux arbres
#' @param dossier Dossier de sortie pour sauvegarder les résultats (optionnel)
#' @param date_debut_ombrage Date de début pour le calcul d'ombrage (défaut: "2024-05-01")
#' @param date_fin_ombrage Date de fin pour le calcul d'ombrage (défaut: "2024-08-31")
#' @param intervalle_jours_ombrage Intervalle en jours pour l'ombrage (défaut: 14)
#' @param progress_bar Afficher une barre de progression (défaut: TRUE)
#' @param ... Arguments supplémentaires passés aux fonctions internes
#'
#' @return Une liste structurée contenant tous les résultats:
#' \describe{
#'   \item{polygone}{Le polygone d'origine}
#'   \item{polygone_buffer}{Le polygone avec buffer}
#'   \item{terrain}{Liste avec MNT, pente, aspect, géomorphons}
#'   \item{pedologie}{Liste avec sols extraits et désagrégation}
#'   \item{lidar_ponctuel}{Résultat du LiDAR ponctuel}
#'   \item{arbres}{Arbres détectés et classifiés}
#'   \item{ombrage}{Résultats du calcul d'ombrage}
#'   \item{vent}{Rose des vents et direction dominante}
#'   \item{distances_vent}{Distances amont/aval avec fetch de vent}
#'   \item{distance_arbres}{Distance euclidienne aux arbres}
#'   \item{raster_consolide}{Raster multi-bandes avec toutes les covariables}
#'   \item{noms_bandes}{Noms des bandes du raster consolidé}
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Extraction complète
#' champ <- sf::st_read("mon_champ.shp")
#' resultats <- extraire_covariables_complet(champ, buffer = 100)
#'
#' # Accéder au raster consolidé
#' raster <- resultats$raster_consolide
#' names(raster)  # Voir les noms des bandes
#'
#' # Extraction partielle (sans ombrage)
#' resultats <- extraire_covariables_complet(
#'   champ,
#'   buffer = 100,
#'   covariables = list(terrain = TRUE, pedologie = TRUE, ombrage = FALSE)
#' )
#' }
#'
#' @importFrom sf st_buffer st_transform st_crs st_union st_bbox
#' @importFrom terra crop mask project resample rast nlyr
#' @importFrom methods is
extraire_covariables_complet <- function(
    polygone,
    buffer = 100,
    covariables = list(
      terrain = TRUE,
      pedologie = TRUE,
      lidar_ponctuel = TRUE,
      arbres = TRUE,
      ombrage = TRUE,
      vent_dominant = TRUE,
      distances_vent = TRUE,
      distance_arbres = TRUE
    ),
    dossier = NULL,
    date_debut_ombrage = "2024-05-01",
    date_fin_ombrage = "2024-08-31",
    intervalle_jours_ombrage = 14,
    progress_bar = TRUE,
    ...
) {

  # Vérifier l'entrée
  if (!methods::is(polygone, "sf")) {
    stop("Le polygone doit être un objet sf")
  }

  if (is.na(sf::st_crs(polygone))) {
    stop("Le polygone doit avoir un CRS défini")
  }

  # Initialiser les résultats
  resultats <- list(
    polygone = polygone,
    covariables_calculees = list(),
    timing = list()
  )

  # Déterminer le CRS projeté pour le buffer
  polygone_wgs84 <- sf::st_transform(polygone, 4326)
  centroid <- sf::st_coordinates(sf::st_centroid(sf::st_union(polygone_wgs84)))
  lon <- centroid[1]
  lat <- centroid[2]

  # Calculer la zone UTM
  utm_zone <- floor((lon + 180) / 6) + 1
  crs_projete <- ifelse(lat >= 0, 32600 + utm_zone, 32700 + utm_zone)

  message("\n", paste(rep("=", 70), collapse = ""))
  message("EXTRACTION COMPLÈTE DES COVARIABLES")
  message(paste(rep("=", 70), collapse = ""))
  message("CRS projeté: EPSG:", crs_projete)
  message("Buffer: ", buffer, " m")
  message("Intervalle ombrage: ", intervalle_jours_ombrage, " jours")
  message("Covariables demandées: ", paste(names(covariables)[unlist(covariables)], collapse = ", "))

  # Calculer le buffer
  debut_buffer <- Sys.time()
  message("\n", paste(rep("-", 70), collapse = ""))
  message("ÉTAPE 1: Calcul du buffer")
  message(paste(rep("-", 70), collapse = ""))

  polygone_projete <- sf::st_transform(polygone, crs_projete)
  polygone_buffer <- sf::st_buffer(polygone_projete, dist = buffer)
  polygone_buffer_wgs84 <- sf::st_transform(polygone_buffer, 4326)

  resultats$polygone_buffer <- polygone_buffer
  resultats$polygone_buffer_wgs84 <- polygone_buffer_wgs84
  resultats$crs_projete <- crs_projete

  fin_buffer <- Sys.time()
  resultats$timing$buffer <- as.numeric(difftime(fin_buffer, debut_buffer, units = "secs"))
  message("✓ Buffer calculé en ", round(resultats$timing$buffer, 1), " secondes")

  # 1. TERRAIN
  if (isTRUE(covariables$terrain)) {
    debut_terrain <- Sys.time()
    message("\n", paste(rep("-", 70), collapse = ""))
    message("ÉTAPE 2: Extraction des covariables terrain")
    message(paste(rep("-", 70), collapse = ""))

    tryCatch({
      # Télécharger le MNT
      message("\n--- Téléchargement du MNT ---")
      mnt <- telecharger_lidar(polygone_buffer_wgs84, mne = FALSE, epsg = crs_projete)

      if (!is.null(mnt)) {
        # Calculer les covariables terrain
        message("\n--- Calcul des covariables ---")
        cov_terrain <- extraire_covariables_terrain(
          polygone = polygone_buffer_wgs84,
          dossier = dossier,
          epsg = crs_projete,
          ...
        )

        # Recadrer au polygone d'origine (sans buffer)
        mnt_champ <- terra::crop(mnt, terra::vect(polygone_projete))
        mnt_champ <- terra::mask(mnt_champ, terra::vect(polygone_projete))

        resultats$terrain <- list(
          mnt = mnt,
          mnt_champ = mnt_champ,
          pente = cov_terrain$pente,
          aspect = cov_terrain$aspect,
          geomorphons = cov_terrain$geomorphons
        )

        resultats$covariables_calculees$terrain <- TRUE
        message("✓ Covariables terrain extraites")
      } else {
        warning("Impossible d'obtenir le MNT")
        resultats$covariables_calculees$terrain <- FALSE
      }
    }, error = function(e) {
      warning("Erreur lors de l'extraction terrain: ", conditionMessage(e))
      resultats$covariables_calculees$terrain <- FALSE
    })

    fin_terrain <- Sys.time()
    resultats$timing$terrain <- as.numeric(difftime(fin_terrain, debut_terrain, units = "secs"))
  }

  # 2. PÉDOLOGIE
  if (isTRUE(covariables$pedologie)) {
    debut_pedologie <- Sys.time()
    message("\n", paste(rep("-", 70), collapse = ""))
    message("ÉTAPE 3: Extraction et désagrégation de la pédologie")
    message(paste(rep("-", 70), collapse = ""))

    tryCatch({
      # Extraire les sols avec le FGB
      message("\n--- Extraction des données pédologiques ---")
      sols <- lire_pedologie(polygone_buffer_wgs84)

      if (!is.null(sols) && nrow(sols) > 0) {
        # Transformer dans le CRS projeté
        sols_projete <- sf::st_transform(sols, crs_projete)

        # Vérifier qu'on a un MNT pour la désagrégation
        if (!is.null(resultats$terrain$mnt)) {
          message("\n--- Désagrégation des séries de sols ---")

          # Préparer la table des séries (à adapter selon la structure des données)
          # Par défaut, on utilise la fonction proba_serie avec les données disponibles
          desaggregation <- proba_et_classement_serie_quota_ilr(
            polygones_sf = sols_projete,
            table_series = NULL,  # À définir selon les données
            mnt = resultats$terrain$mnt,
            ...
          )

          resultats$pedologie <- list(
            sols = sols,
            sols_projete = sols_projete,
            desaggregation = desaggregation
          )

          resultats$covariables_calculees$pedologie <- TRUE
          message("✓ Pédologie extraite et désagrégée")
        } else {
          warning("MNT non disponible pour la désagrégation pédologique")
          resultats$pedologie <- list(sols = sols, sols_projete = sols_projete)
          resultats$covariables_calculees$pedologie <- "partiel"
        }
      } else {
        warning("Aucune donnée pédologique trouvée")
        resultats$covariables_calculees$pedologie <- FALSE
      }
    }, error = function(e) {
      warning("Erreur lors de l'extraction pédologique: ", conditionMessage(e))
      resultats$covariables_calculees$pedologie <- FALSE
    })

    fin_pedologie <- Sys.time()
    resultats$timing$pedologie <- as.numeric(difftime(fin_pedologie, debut_pedologie, units = "secs"))
  }

  # 3. LIDAR PONCTUEL
  if (isTRUE(covariables$lidar_ponctuel)) {
    debut_lidar <- Sys.time()
    message("\n", paste(rep("-", 70), collapse = ""))
    message("ÉTAPE 4: Extraction du LiDAR ponctuel")
    message(paste(rep("-", 70), collapse = ""))

    tryCatch({
      lidar_points <- telecharger_lidar_ponctuel(
        polygone = polygone_buffer_wgs84,
        buffer = 0,  # Déjà bufferisé
        dossier = dossier,
        metriques = TRUE,
        ...
      )

      if (!is.null(lidar_points)) {
        resultats$lidar_ponctuel <- lidar_points
        resultats$covariables_calculees$lidar_ponctuel <- TRUE
        message("✓ LiDAR ponctuel extrait: ",
                if (!is.null(lidar_points$metriques)) lidar_points$metriques$nb_points else "N/A",
                " points")
      } else {
        warning("Impossible d'obtenir le LiDAR ponctuel")
        resultats$covariables_calculees$lidar_ponctuel <- FALSE
      }
    }, error = function(e) {
      warning("Erreur lors de l'extraction LiDAR ponctuel: ", conditionMessage(e))
      resultats$covariables_calculees$lidar_ponctuel <- FALSE
    })

    fin_lidar <- Sys.time()
    resultats$timing$lidar_ponctuel <- as.numeric(difftime(fin_lidar, debut_lidar, units = "secs"))
  }

  # 4. ARBRES
  if (isTRUE(covariables$arbres) && isTRUE(resultats$covariables_calculees$lidar_ponctuel)) {
    debut_arbres <- Sys.time()
    message("\n", paste(rep("-", 70), collapse = ""))
    message("ÉTAPE 5: Détection et classification des arbres")
    message(paste(rep("-", 70), collapse = ""))

    tryCatch({
      if (!is.null(resultats$lidar_ponctuel$nuage_points)) {
        arbres <- extraire_classifier_haies_lidar(
          nuage_points = resultats$lidar_ponctuel$nuage_points,
          ...
        )

        resultats$arbres <- arbres
        resultats$covariables_calculees$arbres <- TRUE

        if (!is.null(arbres$trees_sf)) {
          message("✓ Arbres détectés: ", nrow(arbres$trees_sf), " individus")
          if ("classe" %in% names(arbres$trees_sf)) {
            table_classes <- table(arbres$trees_sf$classe)
            message("   Classification:")
            for (cl in names(table_classes)) {
              message("     - ", cl, ": ", table_classes[cl])
            }
          }
        }
      } else {
        warning("Nuage de points non disponible pour la détection d'arbres")
        resultats$covariables_calculees$arbres <- FALSE
      }
    }, error = function(e) {
      warning("Erreur lors de la détection des arbres: ", conditionMessage(e))
      resultats$covariables_calculees$arbres <- FALSE
    })

    fin_arbres <- Sys.time()
    resultats$timing$arbres <- as.numeric(difftime(fin_arbres, debut_arbres, units = "secs"))
  } else if (isTRUE(covariables$arbres)) {
    warning("Étape arbres ignorée car LiDAR ponctuel non disponible")
    resultats$covariables_calculees$arbres <- FALSE
  }

  # 5. OMBRAGE
  if (isTRUE(covariables$ombrage)) {
    debut_ombrage <- Sys.time()
    message("\n", paste(rep("-", 70), collapse = ""))
    message("ÉTAPE 6: Calcul de l'ombrage (", date_debut_ombrage, " à ", date_fin_ombrage, ")")
    message(paste(rep("-", 70), collapse = ""))

    tryCatch({
      # Utiliser le MNE si disponible, sinon le télécharger
      mne <- if (!is.null(resultats$terrain$mnt)) {
        message("Utilisation du MNT comme MNE (approximation)")
        resultats$terrain$mnt
      } else NULL

      ombrage <- calculer_ombrage_periode(
        polygone = polygone_projete,
        date_debut = date_debut_ombrage,
        date_fin = date_fin_ombrage,
        intervalle_jours = intervalle_jours_ombrage,
        dossier = dossier,
        mne = mne,
        ...
      )

      if (!is.null(ombrage)) {
        resultats$ombrage <- ombrage
        resultats$covariables_calculees$ombrage <- TRUE
        message("✓ Ombrage calculé sur ", ombrage$nb_jours_calcules, " jours")
        message("   Heures moyennes d'ensoleillement: ",
                round(mean(terra::values(ombrage$heures_ensoleillement_moyen), na.rm = TRUE), 1),
                "h/jour")
      } else {
        warning("Échec du calcul d'ombrage")
        resultats$covariables_calculees$ombrage <- FALSE
      }
    }, error = function(e) {
      warning("Erreur lors du calcul d'ombrage: ", conditionMessage(e))
      resultats$covariables_calculees$ombrage <- FALSE
    })

    fin_ombrage <- Sys.time()
    resultats$timing$ombrage <- as.numeric(difftime(fin_ombrage, debut_ombrage, units = "secs"))
  }

  # 6. VENT DOMINANT
  if (isTRUE(covariables$vent_dominant)) {
    debut_vent <- Sys.time()
    message("\n", paste(rep("-", 70), collapse = ""))
    message("ÉTAPE 7: Analyse du vent dominant")
    message(paste(rep("-", 70), collapse = ""))

    tryCatch({
      # Obtenir la rose des vents
      rose <- obtenir_rose_vents(
        polygone = polygone_wgs84,
        ...
      )

      if (!is.null(rose)) {
        # Calculer la direction dominante
        direction_dominante <- rose$directions[which.max(rose$wd_pct)]

        resultats$vent <- list(
          rose_vents = rose,
          direction_dominante = direction_dominante,
          direction_dominante_nom = direction_cardinale(direction_dominante)
        )
        resultats$covariables_calculees$vent_dominant <- TRUE

        message("✓ Vent dominant: ", round(direction_dominante, 0), "° (",
                resultats$vent$direction_dominante_nom, ")")
        message("   Vitesse moyenne: ", round(mean(rose$wd_avg), 1), " m/s")
      } else {
        warning("Impossible d'obtenir les données de vent")
        resultats$covariables_calculees$vent_dominant <- FALSE
      }
    }, error = function(e) {
      warning("Erreur lors de l'analyse du vent: ", conditionMessage(e))
      resultats$covariables_calculees$vent_dominant <- FALSE
    })

    fin_vent <- Sys.time()
    resultats$timing$vent_dominant <- as.numeric(difftime(fin_vent, debut_vent, units = "secs"))
  }

  # 7. DISTANCES VENT (amont/aval)
  if (isTRUE(covariables$distances_vent) && isTRUE(resultats$covariables_calculees$arbres)) {
    debut_distances_vent <- Sys.time()
    message("\n", paste(rep("-", 70), collapse = ""))
    message("ÉTAPE 8: Calcul des distances amont/aval")
    message(paste(rep("-", 70), collapse = ""))

    tryCatch({
      if (!is.null(resultats$arbres$trees_sf) && nrow(resultats$arbres$trees_sf) > 0) {
        # Utiliser la direction du vent dominante si disponible
        angle_vent <- if (!is.null(resultats$vent$direction_dominante)) {
          resultats$vent$direction_dominante
        } else {
          0  # Nord par défaut
        }

        distances_vent <- calculer_distances_amont_aval(
          arbres_sf = resultats$arbres$trees_sf,
          angle_vent = angle_vent,
          champ_bbox = polygone_projete,
          ...
        )

        resultats$distances_vent <- distances_vent
        resultats$covariables_calculees$distances_vent <- TRUE
        message("✓ Distances amont/aval calculées (direction: ", round(angle_vent, 0), "°)")
      } else {
        warning("Pas d'arbres détectés pour calculer les distances de vent")
        resultats$covariables_calculees$distances_vent <- FALSE
      }
    }, error = function(e) {
      warning("Erreur lors du calcul des distances de vent: ", conditionMessage(e))
      resultats$covariables_calculees$distances_vent <- FALSE
    })

    fin_distances_vent <- Sys.time()
    resultats$timing$distances_vent <- as.numeric(difftime(fin_distances_vent, debut_distances_vent, units = "secs"))
  } else if (isTRUE(covariables$distances_vent)) {
    warning("Étape distances_vent ignorée car arbres non disponibles")
    resultats$covariables_calculees$distances_vent <- FALSE
  }

  # 8. DISTANCE AUX ARBRES
  if (isTRUE(covariables$distance_arbres) && isTRUE(resultats$covariables_calculees$arbres)) {
    debut_distance_arbres <- Sys.time()
    message("\n", paste(rep("-", 70), collapse = ""))
    message("ÉTAPE 9: Calcul de la distance aux arbres")
    message(paste(rep("-", 70), collapse = ""))

    tryCatch({
      if (!is.null(resultats$arbres$trees_sf) && nrow(resultats$arbres$trees_sf) > 0) {
        distance_arbres <- calculer_distance_arbres(
          arbres_sf = resultats$arbres$trees_sf,
          champ_bbox = polygone_projete,
          ...
        )

        resultats$distance_arbres <- distance_arbres
        resultats$covariables_calculees$distance_arbres <- TRUE
        message("✓ Distance aux arbres calculée")
      } else {
        warning("Pas d'arbres détectés pour calculer la distance")
        resultats$covariables_calculees$distance_arbres <- FALSE
      }
    }, error = function(e) {
      warning("Erreur lors de la distance aux arbres: ", conditionMessage(e))
      resultats$covariables_calculees$distance_arbres <- FALSE
    })

    fin_distance_arbres <- Sys.time()
    resultats$timing$distance_arbres <- as.numeric(difftime(fin_distance_arbres, debut_distance_arbres, units = "secs"))
  } else if (isTRUE(covariables$distance_arbres)) {
    warning("Étape distance_arbres ignorée car arbres non disponibles")
    resultats$covariables_calculees$distance_arbres <- FALSE
  }

  # 10. CONSOLIDATION - CRÉER LE RASTER MULTI-BANDES
  message("\n", paste(rep("-", 70), collapse = ""))
  message("ÉTAPE 10: Consolidation des covariables")
  message(paste(rep("-", 70), collapse = ""))

  debut_consolidation <- Sys.time()

  tryCatch({
    message("  [DEBUG] Appel de consolider_covariables...")
    raster_consolide <- consolider_covariables(
      resultats = resultats,
      polygone = polygone_projete,
      dossier = dossier
    )
    message("  [DEBUG] consolider_covariables terminé, raster_consolide est NULL: ", is.null(raster_consolide))

    if (!is.null(raster_consolide)) {
      resultats$raster_consolide <- raster_consolide
      resultats$noms_bandes <- names(raster_consolide)
      message("✓ Raster consolidé créé avec ", terra::nlyr(raster_consolide), " bandes")
      message("   Bandes: ", paste(names(raster_consolide), collapse = ", "))
    } else {
      warning("Impossible de créer le raster consolidé (retour NULL)")
    }
  }, error = function(e) {
    warning("Erreur lors de la consolidation: ", conditionMessage(e))
  })

  fin_consolidation <- Sys.time()
  resultats$timing$consolidation <- as.numeric(difftime(fin_consolidation, debut_consolidation, units = "secs"))

  # Résumé final
  fin_total <- Sys.time()
  resultats$timing$total <- as.numeric(difftime(fin_total, debut_buffer, units = "secs"))

  message("\n", paste(rep("=", 70), collapse = ""))
  message("RÉSUMÉ DE L'EXTRACTION")
  message(paste(rep("=", 70), collapse = ""))

  cov_calc <- resultats$covariables_calculees
  message("Covariables calculées avec succès:")
  for (nom in names(cov_calc)) {
    status <- if (isTRUE(cov_calc[[nom]])) "✓" else if (cov_calc[[nom]] == "partiel") "◐" else "✗"
    time <- if (!is.null(resultats$timing[[nom]])) paste0(" (", round(resultats$timing[[nom]], 1), "s)") else ""
    message("  ", status, " ", nom, time)
  }

  if (!is.null(resultats$raster_consolide)) {
    message("\n✓ Raster consolidé disponible dans resultats$raster_consolide")
    message("  Dimensions: ", terra::nrow(resultats$raster_consolide), " x ", terra::ncol(resultats$raster_consolide))
    message("  Bandes: ", terra::nlyr(resultats$raster_consolide))
  }

  message("\nTemps total: ", round(resultats$timing$total, 1), " secondes (",
          round(resultats$timing$total / 60, 1), " minutes)")
  message(paste(rep("=", 70), collapse = ""))

  # Sauvegarder si un dossier est spécifié
  if (!is.null(dossier)) {
    message("\nSauvegarde des résultats dans: ", dossier)
    dir.create(dossier, recursive = TRUE, showWarnings = FALSE)

    # Sauvegarder le raster consolidé
    if (!is.null(resultats$raster_consolide)) {
      terra::writeRaster(resultats$raster_consolide,
                         file.path(dossier, "covariables_consolide.tif"),
                         overwrite = TRUE)
      message("✓ Raster consolidé sauvegardé: covariables_consolide.tif")
    }

    # Sauvegarder les arbres
    if (!is.null(resultats$arbres$trees_sf)) {
      sf::st_write(resultats$arbres$trees_sf, file.path(dossier, "arbres.gpkg"), delete_dsn = TRUE, quiet = TRUE)
    }

    # Sauvegarder le résumé
    resume <- data.frame(
      covariable = names(cov_calc),
      calculee = unlist(cov_calc),
      timing = sapply(names(cov_calc), function(n) resultats$timing[[n]] %||% NA)
    )
    write.csv(resume, file.path(dossier, "resume_extraction.csv"), row.names = FALSE)

    message("✓ Résultats sauvegardés")
  }

  return(resultats)
}

#' Consolider les covariables en un raster multi-bandes
#'
#' Cette fonction interne aligne toutes les covariables sur la grille du MNT,
#' les projette dans le même CRS, et les assemble en un raster multi-bandes.
#'
#' @param resultats Liste des résultats de l'extraction
#' @param polygone Polygone pour le découpage final
#' @param dossier Dossier de sortie (optionnel)
#'
#' @return Un SpatRaster multi-bandes ou NULL
#' @noRd
consolider_covariables <- function(resultats, polygone, dossier = NULL) {

  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Package 'terra' requis")
  }

  # Vérifier qu'on a au moins le MNT comme référence
  ref <- NULL
  if (!is.null(resultats$terrain$mnt_champ)) {
    ref <- resultats$terrain$mnt_champ
    message("  Utilisation de mnt_champ comme référence")
  } else if (!is.null(resultats$terrain$mnt)) {
    ref <- resultats$terrain$mnt
    message("  Utilisation de mnt comme référence")
  }
  
  if (is.null(ref)) {
    warning("MNT non disponible (mnt_champ ou mnt), impossible de consolider")
    return(NULL)
  }
  
  # S'assurer que la référence est dans le bon CRS
  if (is.na(terra::crs(ref))) {
    warning("Le MNT n'a pas de CRS défini")
    return(NULL)
  }
  message("  Référence (MNT): ", terra::nrow(ref), " x ", terra::ncol(ref), " pixels")

  # Liste pour stocker les couches
  couches <- list()
  noms_couches <- c()

  # Fonction helper pour ajouter une couche - retourne la liste mise à jour
  ajouter_couche <- function(couches_list, noms_vec, nom, raster_source, ref_raster) {
    message("  Alignement de ", nom, "...")
    
    if (is.null(raster_source)) {
      message("    ", nom, " non disponible - rempli avec NA")
      couches_list[[nom]] <- terra::rast(ref_raster) * NA
      noms_vec <- c(noms_vec, nom)
      return(list(couches = couches_list, noms = noms_vec))
    }
    
    aligned <- tryCatch({
      aligner_sur_ref(raster_source, ref_raster)
    }, error = function(e) {
      message("    ✗ Erreur d'alignement: ", conditionMessage(e))
      NULL
    })
    
    if (!is.null(aligned)) {
      couches_list[[nom]] <- aligned
      noms_vec <- c(noms_vec, nom)
      message("    ✓ ", nom, " aligné")
    } else {
      message("    ✗ Échec de l'alignement - rempli avec NA")
      couches_list[[nom]] <- terra::rast(ref_raster) * NA
      noms_vec <- c(noms_vec, nom)
    }
    
    return(list(couches = couches_list, noms = noms_vec))
  }

  # 1. ASPECT
  result <- ajouter_couche(couches, noms_couches, "aspect", resultats$terrain$aspect, ref)
  couches <- result$couches
  noms_couches <- result$noms

  # 2. PENTE
  result <- ajouter_couche(couches, noms_couches, "pente", resultats$terrain$pente, ref)
  couches <- result$couches
  noms_couches <- result$noms

  # 3. GÉOMORPHONS
  if (!is.null(resultats$terrain$geomorphons)) {
    message("  Préparation des géomorphons...")
    if (is.list(resultats$terrain$geomorphons)) {
      geom <- resultats$terrain$geomorphons$moyen
      message("    Utilisation de l'échelle 'moyen'")
    } else {
      geom <- resultats$terrain$geomorphons
      message("    Utilisation de l'échelle unique")
    }
    result <- ajouter_couche(couches, noms_couches, "geomorphons", geom, ref)
    couches <- result$couches
    noms_couches <- result$noms
  } else {
    message("  geomorphons non disponible - rempli avec NA")
    couches[["geomorphons"]] <- terra::rast(ref) * NA
    noms_couches <- c(noms_couches, "geomorphons")
  }

  # 4. TEXTURE DES SOLS (sable, limon, argile) - toujours ajouter même si NA
  message("  Préparation des textures de sol...")
  message("    [DEBUG] Création des couches de texture...")
  
  # Créer des rasters vides avec les mêmes dimensions que ref
  sable_rast <- terra::rast(terra::ext(ref), resolution = terra::res(ref), crs = terra::crs(ref))
  terra::values(sable_rast) <- NA
  
  limon_rast <- terra::rast(terra::ext(ref), resolution = terra::res(ref), crs = terra::crs(ref))
  terra::values(limon_rast) <- NA
  
  argile_rast <- terra::rast(terra::ext(ref), resolution = terra::res(ref), crs = terra::crs(ref))
  terra::values(argile_rast) <- NA
  
  couches[["sable"]] <- sable_rast
  couches[["limon"]] <- limon_rast
  couches[["argile"]] <- argile_rast
  noms_couches <- c(noms_couches, "sable", "limon", "argile")
  
  message("    [DEBUG] sable: ", terra::nrow(couches[["sable"]]), " x ", terra::ncol(couches[["sable"]]))
  message("    [DEBUG] limon: ", terra::nrow(couches[["limon"]]), " x ", terra::ncol(couches[["limon"]]))
  message("    [DEBUG] argile: ", terra::nrow(couches[["argile"]]), " x ", terra::ncol(couches[["argile"]]))
  message("    ✓ Textures de sol ajoutées (valeurs NA)")

  # 5. DISTANCE AUX ARBRES
  message("  [DEBUG] Début section distance_arbres...")
  if (!is.null(resultats$distance_arbres) && !is.null(resultats$distance_arbres$distance_buffer)) {
    result <- ajouter_couche(couches, noms_couches, "distance_arbres", resultats$distance_arbres$distance_buffer, ref)
    couches <- result$couches
    noms_couches <- result$noms
  } else {
    message("  distance_arbres non disponible - rempli avec NA")
    dist_rast <- terra::rast(terra::ext(ref), resolution = terra::res(ref), crs = terra::crs(ref))
    terra::values(dist_rast) <- NA
    couches[["distance_arbres"]] <- dist_rast
    noms_couches <- c(noms_couches, "distance_arbres")
  }
  message("  [DEBUG] Fin section distance_arbres")

  # 6. DISTANCES AMONT/AVAL
  message("  [DEBUG] Début section distances_vent...")
  if (!is.null(resultats$distances_vent)) {
    if (!is.null(resultats$distances_vent$amont)) {
      result <- ajouter_couche(couches, noms_couches, "distance_amont", resultats$distances_vent$amont, ref)
      couches <- result$couches
      noms_couches <- result$noms
    } else {
      message("  distance_amont non disponible - rempli avec NA")
      amont_rast <- terra::rast(terra::ext(ref), resolution = terra::res(ref), crs = terra::crs(ref))
      terra::values(amont_rast) <- NA
      couches[["distance_amont"]] <- amont_rast
      noms_couches <- c(noms_couches, "distance_amont")
    }

    if (!is.null(resultats$distances_vent$aval)) {
      result <- ajouter_couche(couches, noms_couches, "distance_aval", resultats$distances_vent$aval, ref)
      couches <- result$couches
      noms_couches <- result$noms
    } else {
      message("  distance_aval non disponible - rempli avec NA")
      aval_rast <- terra::rast(terra::ext(ref), resolution = terra::res(ref), crs = terra::crs(ref))
      terra::values(aval_rast) <- NA
      couches[["distance_aval"]] <- aval_rast
      noms_couches <- c(noms_couches, "distance_aval")
    }
  } else {
    message("  distances_vent non disponible - rempli avec NA")
    amont_rast <- terra::rast(terra::ext(ref), resolution = terra::res(ref), crs = terra::crs(ref))
    terra::values(amont_rast) <- NA
    aval_rast <- terra::rast(terra::ext(ref), resolution = terra::res(ref), crs = terra::crs(ref))
    terra::values(aval_rast) <- NA
    couches[["distance_amont"]] <- amont_rast
    couches[["distance_aval"]] <- aval_rast
    noms_couches <- c(noms_couches, "distance_amont", "distance_aval")
  }
  message("  [DEBUG] Fin section distances_vent")

  # 7. HEURES D'ENSOLEILLEMENT
  message("  [DEBUG] Début section heures_ensoleillement...")
  if (!is.null(resultats$ombrage) && !is.null(resultats$ombrage$heures_ensoleillement_moyen)) {
    result <- ajouter_couche(couches, noms_couches, "heures_ensoleillement", resultats$ombrage$heures_ensoleillement_moyen, ref)
    couches <- result$couches
    noms_couches <- result$noms
  } else {
    message("  heures_ensoleillement non disponible - rempli avec NA")
    ens_rast <- terra::rast(terra::ext(ref), resolution = terra::res(ref), crs = terra::crs(ref))
    terra::values(ens_rast) <- NA
    couches[["heures_ensoleillement"]] <- ens_rast
    noms_couches <- c(noms_couches, "heures_ensoleillement")
  }
  message("  [DEBUG] Fin section heures_ensoleillement")

  # Assembler le raster multi-bandes
  message("  Assemblage du raster multi-bandes...")
  message("    Nombre de couches: ", length(couches))
  message("    Noms: ", paste(noms_couches, collapse = ", "))

  if (length(couches) == 0) {
    warning("Aucune couche à assembler")
    return(NULL)
  }

  # Vérifier que toutes les couches sont valides
  couches_valides <- list()
  noms_valides <- c()
  for (i in seq_along(couches)) {
    if (!is.null(couches[[i]]) && inherits(couches[[i]], "SpatRaster")) {
      couches_valides[[length(couches_valides) + 1]] <- couches[[i]]
      noms_valides <- c(noms_valides, noms_couches[i])
    } else {
      message("    ⚠ Couche ", noms_couches[i], " invalide, ignorée")
    }
  }

  if (length(couches_valides) == 0) {
    warning("Aucune couche valide à assembler")
    return(NULL)
  }

  message("    Couches valides: ", length(couches_valides), "/", length(couches))

  # Créer le raster
  raster_consolide <- terra::rast(couches_valides)
  names(raster_consolide) <- noms_valides
  message("    ✓ Raster créé avec ", terra::nlyr(raster_consolide), " bandes")

  # Découper avec le polygone
  message("  Découpage final avec le polygone...")
  tryCatch({
    raster_consolide <- terra::crop(raster_consolide, terra::vect(polygone))
    raster_consolide <- terra::mask(raster_consolide, terra::vect(polygone))
    message("    ✓ Raster découpé: ", terra::nrow(raster_consolide), " x ", terra::ncol(raster_consolide))
  }, error = function(e) {
    warning("Erreur lors du découpage: ", conditionMessage(e))
  })

  message("  [DEBUG] Retour de consolider_covariables - raster_consolide est NULL: ", is.null(raster_consolide))
  if (!is.null(raster_consolide)) {
    message("  [DEBUG] Nombre de bandes: ", terra::nlyr(raster_consolide))
  }
  
  return(raster_consolide)
}

#' Aligner un raster sur une référence (CRS et résolution)
#'
#' @param r Raster à aligner
#' @param ref Raster de référence
#' @return Raster aligné
#' @noRd
aligner_sur_ref <- function(r, ref) {
  if (is.null(r)) {
    warning("Raster d'entrée NULL")
    return(NULL)
  }
  
  if (is.null(ref)) {
    warning("Raster de référence NULL")
    return(NULL)
  }
  
  # Vérifier les CRS
  crs_r <- terra::crs(r)
  crs_ref <- terra::crs(ref)
  
  if (is.na(crs_r) || is.na(crs_ref)) {
    warning("CRS manquant - r: ", !is.na(crs_r), ", ref: ", !is.na(crs_ref))
    return(NULL)
  }
  
  # Projeter si nécessaire
  if (crs_r != crs_ref) {
    message("    Projection nécessaire...")
    r <- terra::project(r, ref)
  }

  # Déterminer la méthode de resampling
  is_categorical <- !is.null(terra::levels(r)) || terra::is.factor(r)
  method <- if (is_categorical) "near" else "bilinear"
  
  # Resampler sur la grille de référence
  r_resampled <- terra::resample(r, ref, method = method)

  return(r_resampled)
}

#' Helper pour l'opérateur %||%
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Convertir une direction en degrés vers le nom cardinal
#' @noRd
direction_cardinale <- function(angle) {
  dirs <- c("N", "N-NE", "NE", "E-NE", "E", "E-SE", "SE", "S-SE",
            "S", "S-SO", "SO", "O-SO", "O", "O-NO", "NO", "N-NO")
  idx <- floor((angle + 11.25) / 22.5) %% 16 + 1
  dirs[idx]
}

#' Créer une visualisation synthétique des covariables extraites
#'
#' @param resultats Liste retournée par extraire_covariables_complet()
#' @param type Type de visualisation: "terrain", "ombrage", "vent", "arbres", "distances", "consolide", ou "tous"
#'
#' @return NULL (affiche les graphiques)
#' @export
#'
visualiser_covariables <- function(resultats, type = c("tous", "terrain", "ombrage", "vent", "arbres", "distances", "consolide")) {
  type <- match.arg(type)

  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))

  if (type %in% c("terrain", "tous") && !is.null(resultats$terrain)) {
    graphics::par(mfrow = c(2, 2))
    if (!is.null(resultats$terrain$mnt_champ)) {
      terra::plot(resultats$terrain$mnt_champ, main = "MNT")
    }
    if (!is.null(resultats$terrain$pente)) {
      terra::plot(resultats$terrain$pente, main = "Pente")
    }
    if (!is.null(resultats$terrain$aspect)) {
      terra::plot(resultats$terrain$aspect, main = "Aspect")
    }
    if (!is.null(resultats$terrain$geomorphons)) {
      if (is.list(resultats$terrain$geomorphons)) {
        terra::plot(resultats$terrain$geomorphons$moyen, main = "Géomorphons")
      } else {
        terra::plot(resultats$terrain$geomorphons, main = "Géomorphons")
      }
    }
  }

  if (type %in% c("ombrage", "tous") && !is.null(resultats$ombrage)) {
    graphics::par(mfrow = c(1, 2))
    if (!is.null(resultats$ombrage$heures_ensoleillement_moyen)) {
      terra::plot(resultats$ombrage$heures_ensoleillement_moyen, main = "Heures d'ensoleillement")
    }
    if (!is.null(resultats$ombrage$ombrage_moyen_periode)) {
      terra::plot(resultats$ombrage$ombrage_moyen_periode, main = "Ombrage moyen")
    }
  }

  if (type %in% c("vent", "tous") && !is.null(resultats$vent$rose_vents)) {
    print(tracer_rose_vents_stacked(resultats$vent$rose_vents))
  }

  if (type %in% c("arbres", "tous") && !is.null(resultats$arbres$trees_sf)) {
    plot(sf::st_geometry(resultats$polygone), col = "lightgreen", main = "Arbres détectés")
    cols <- c("individuel" = "darkgreen", "haie_brise_vent" = "blue",
              "foret" = "darkred", "petit_groupe" = "orange", "incertain" = "gray")
    classes <- resultats$arbres$trees_sf$classe
    points_col <- cols[classes]
    points_col[is.na(points_col)] <- "black"
    plot(sf::st_geometry(resultats$arbres$trees_sf), col = points_col, pch = 19, cex = 0.5, add = TRUE)
    legend("topright", legend = names(cols), col = cols, pch = 19)
  }

  if (type %in% c("distances", "tous") && !is.null(resultats$distances_vent)) {
    graphics::par(mfrow = c(2, 2))
    if (!is.null(resultats$distances_vent$amont)) {
      terra::plot(resultats$distances_vent$amont, main = "Distance amont (protection)")
    }
    if (!is.null(resultats$distances_vent$aval)) {
      terra::plot(resultats$distances_vent$aval, main = "Distance aval (exposition)")
    }
    if (!is.null(resultats$distances_vent$totale)) {
      terra::plot(resultats$distances_vent$totale, main = "Distance totale")
    }
    if (!is.null(resultats$distance_arbres$distance_buffer)) {
      terra::plot(resultats$distance_arbres$distance_buffer, main = "Distance aux arbres")
    }
  }

  if (type %in% c("consolide", "tous") && !is.null(resultats$raster_consolide)) {
    n_bandes <- terra::nlyr(resultats$raster_consolide)
    if (n_bandes <= 4) {
      graphics::par(mfrow = c(2, 2))
    } else if (n_bandes <= 6) {
      graphics::par(mfrow = c(2, 3))
    } else if (n_bandes <= 9) {
      graphics::par(mfrow = c(3, 3))
    } else {
      graphics::par(mfrow = c(3, 4))
    }

    for (i in 1:min(n_bandes, 12)) {
      terra::plot(resultats$raster_consolide[[i]], main = names(resultats$raster_consolide)[i])
    }
  }

  invisible(NULL)
}

#' Vérifier la disponibilité des données LiDAR
#'
#' Vérifie si des données LiDAR sont disponibles pour une zone donnée
#' et retourne des informations sur les années disponibles.
#'
#' @param polygone Un objet `sf` représentant la zone d'intérêt ou un chemin de fichier
#' @param mne Logique. Si TRUE, vérifie la disponibilité du MNE. Sinon, du MNT.
#'
#' @return Un data.frame avec les années et nombre de tuiles disponibles
#' @export
#'
#' @examples
#' \dontrun{
#' info <- verifier_disponibilite_lidar(champ)
#' print(info)
#' }
#'
#' @importFrom sf st_read st_crs st_transform st_union st_bbox
#' @importFrom rstac stac stac_search get_request items_datetime
#' @importFrom lubridate as_datetime year
verifier_disponibilite_lidar <- function(polygone, mne = FALSE) {
  
  if (is.character(polygone)) {
    polygone <- sf::st_read(polygone, quiet = TRUE)
  }
  
  if (is.na(sf::st_crs(polygone))) {
    sf::st_crs(polygone) <- 4326
  }
  
  polygone <- polygone |> sf::st_transform(4326) |> sf::st_union()
  
  stac_query <- rstac::stac("https://datacube.services.geo.ca/stac/api/", force_version = TRUE) |>
    rstac::stac_search(
      collections = "hrdem-lidar",
      bbox = as.numeric(sf::st_bbox(polygone)),
      limit = 100
    ) |>
    rstac::get_request()
  
  # Vérifier si des résultats sont disponibles (plusieurs méthodes)
  nb_resultats <- 0
  if (!is.null(stac_query$numberMatched)) {
    nb_resultats <- stac_query$numberMatched
  } else if (!is.null(stac_query$numberReturned)) {
    nb_resultats <- stac_query$numberReturned
  } else if (!is.null(stac_query$features)) {
    nb_resultats <- length(stac_query$features)
  }
  
  if (nb_resultats == 0) {
    message("Aucune donnée LiDAR disponible pour cette zone.")
    return(NULL)
  }
  
  datetimes <- lubridate::as_datetime(rstac::items_datetime(stac_query))
  annees <- lubridate::year(datetimes)
  
  # Compter les tuiles par année
  table_annees <- table(annees)
  
  resultat <- data.frame(
    annee = as.numeric(names(table_annees)),
    nb_tuiles = as.numeric(table_annees)
  )
  
  resultat <- resultat[order(resultat$annee, decreasing = TRUE), ]
  rownames(resultat) <- NULL
  
  return(resultat)
}

#' Obtenir les labels des géomorphons
#'
#' Retourne un vecteur nommé avec les labels des classes de géomorphons.
#'
#' @return Un vecteur caractère nommé
#' @export
#'
#' @examples
#' labels <- labels_geomorphons()
#' print(labels)
labels_geomorphons <- function() {
  c(
    "1" = "Plat",
    "2" = "Pic",
    "3" = "Crête",
    "4" = "Épaulement",
    "5" = "Éperon",
    "6" = "Pente",
    "7" = "Creux",
    "8" = "Pied de pente",
    "9" = "Vallée",
    "10" = "Fosse"
  )
}

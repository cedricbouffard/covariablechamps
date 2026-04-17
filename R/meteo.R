#' Obtenir la rose des vents depuis NASA POWER
#'
#' Récupère les données de rose des vents (distribution des directions et vitesses)
#' depuis l'API NASA POWER pour le centroïde d'un champ.
#'
#' @param polygone Un objet `sf` représentant le champ ou un vecteur c(longitude, latitude)
#' @param date_debut Date de début au format "YYYYMMDD" (défaut: "20230101")
#' @param date_fin Date de fin au format "YYYYMMDD" (défaut: "20231231")
#' @param hauteur Hauteur des mesures: "10m" ou "50m" (défaut: "10m")
#'
#' @return Une liste contenant:
#' \item{data}{Les données brutes de l'API}
#' \item{directions}{Vecteur des 16 directions (0-337.5°)}
#' \item{wd_pct}{Pourcentages de fréquence par direction}
#' \item{wd_avg}{Vitesse moyenne du vent par direction (m/s)}
#' \item{classes}{Matrice des classes de vent par direction}
#' \item{all_classes}{Pourcentages globaux par classe de vent}
#' \item{coordonnees}{Coordonnées du point (longitude, latitude)}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Pour un champ
#' champ <- sf::st_read("champ.shp")
#' rose <- obtenir_rose_vents(champ)
#'
#' # Pour des coordonnées spécifiques
#' rose <- obtenir_rose_vents(c(-71.055, 46.648))
#' }
#'
#' @importFrom sf st_read st_crs st_transform st_centroid st_coordinates st_union st_is_longlat
#' @importFrom httr GET content http_status
#' @importFrom jsonlite fromJSON
obtenir_rose_vents <- function(polygone,
                                date_debut = "20230101",
                                date_fin = "20231231",
                                hauteur = "10m") {

  # Extraire les coordonnées
  if (is.numeric(polygone) && length(polygone) == 2) {
    # C'est déjà un vecteur de coordonnées c(lon, lat)
    lon <- polygone[1]
    lat <- polygone[2]
  } else if (is.character(polygone)) {
    # C'est un chemin de fichier
    polygone <- sf::st_read(polygone, quiet = TRUE)
    coords <- obtenir_centroide(polygone)
    lon <- coords[1]
    lat <- coords[2]
  } else if (inherits(polygone, "sf")) {
    # C'est un objet sf
    coords <- obtenir_centroide(polygone)
    lon <- coords[1]
    lat <- coords[2]
  } else {
    stop("polygone doit être un objet sf, un chemin de fichier, ou un vecteur c(lon, lat)")
  }

  # Valider la hauteur
  if (!hauteur %in% c("10m", "50m")) {
    stop("hauteur doit être '10m' ou '50m'")
  }

  # Paramètre de l'API
  param <- ifelse(hauteur == "10m", "WR10M", "WR50M")

  # Construire l'URL
  base_url <- "https://power.larc.nasa.gov/api/application/windrose/point"
  url <- paste0(base_url,
                "?start=", date_debut,
                "&end=", date_fin,
                "&latitude=", lat,
                "&longitude=", lon,
                "&units=metric",
                "&time-standard=utc")

  message("Récupération des données de vent depuis NASA POWER...")
  message("Coordonnées: ", round(lon, 4), ", ", round(lat, 4))
  message("Période: ", date_debut, " à ", date_fin)

  # Faire la requête
  response <- httr::GET(url)

  # Vérifier le statut
  if (httr::http_status(response)$category != "Success") {
    stop("Erreur lors de la récupération des données: ",
         httr::http_status(response)$message)
  }

  # Parser le JSON
  data <- httr::content(response, "text", encoding = "UTF-8")
  json_data <- jsonlite::fromJSON(data)

  # Extraire les données de la rose des vents
  wind_data <- json_data$properties$parameter[[param]]

  # Directions (sauf "ALL")
  directions <- names(wind_data)
  directions <- directions[directions != "ALL"]
  directions_num <- as.numeric(directions)

  # Extraire les pourcentages et vitesses moyennes
  wd_pct <- sapply(directions, function(d) wind_data[[d]]$WD_PCT)
  wd_avg <- sapply(directions, function(d) wind_data[[d]]$WD_AVG)

  # Extraire les classes
  classes_matrix <- t(sapply(directions, function(d) {
    unlist(wind_data[[d]]$CLASSES)
  }))

  # Pourcentages globaux par classe
  all_classes <- wind_data$ALL

  # Créer le résultat
  resultat <- list(
    data = json_data,
    directions = directions_num,
    wd_pct = wd_pct,
    wd_avg = wd_avg,
    classes = classes_matrix,
    all_classes = all_classes,
    coordonnees = c(longitude = lon, latitude = lat),
    hauteur = hauteur,
    periode = c(debut = date_debut, fin = date_fin)
  )

  message("Données récupérées avec succès!")
  return(resultat)
}

#' Obtenir le centroïde d'un polygone en WGS84
#' @noRd
obtenir_centroide <- function(polygone) {
  # S'assurer que le CRS est défini
  if (is.na(sf::st_crs(polygone))) {
    sf::st_crs(polygone) <- 4326
  }

  # Transformer en WGS84 si nécessaire
  polygone_wgs84 <- sf::st_transform(polygone, 4326)

  # Calculer le centroïde
  centroid <- sf::st_centroid(sf::st_union(polygone_wgs84))
  coords <- sf::st_coordinates(centroid)

  return(c(coords[1], coords[2]))  # lon, lat
}

#' Créer un graphique radar de la rose des vents
#'
#' Visualise la distribution des directions du vent sous forme de graphique radar.
#' Le Nord est affiché en haut par défaut.
#'
#' @param rose_vents Résultat de la fonction `obtenir_rose_vents()`
#' @param type Type de visualisation: "pct" (fréquence), "avg" (vitesse moyenne)
#' @param afficher_legende Logique. Si TRUE, affiche la légende
#' @param palette Couleur de remplissage (défaut: "steelblue")
#' @param ajustement_orientation Angle d'ajustement en degrés pour aligner avec l'orientation du champ (défaut: 0)
#'
#' @return Un objet ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' champ <- sf::st_read("champ.shp")
#' rose <- obtenir_rose_vents(champ)
#' graphique <- tracer_rose_vents(rose)
#' print(graphique)
#'
#' # Ajuster avec l'orientation du champ (ex: 45°)
#' graphique <- tracer_rose_vents(rose, ajustement_orientation = 45)
#' }
#'
#' @importFrom ggplot2 ggplot geom_polygon geom_line coord_polar scale_x_continuous
#' @importFrom ggplot2 theme_minimal labs theme element_text element_blank
#' @importFrom dplyr tibble
#' @importFrom tidyr pivot_longer
#' @importFrom rlang .data
tracer_rose_vents <- function(rose_vents,
                              type = c("pct", "avg", "both"),
                              afficher_legende = TRUE,
                              palette = "steelblue",
                              ajustement_orientation = 0) {
  
  type <- match.arg(type)
  
  stopifnot(
    requireNamespace("ggplot2", quietly = TRUE),
    requireNamespace("dplyr", quietly = TRUE),
    requireNamespace("scales", quietly = TRUE)
  )
  if (type == "both") stopifnot(requireNamespace("tidyr", quietly = TRUE))
  
  # Directions FR (ordre NASA: 0..337.5 par pas de 22.5)
  dir_names <- c(
    "Nord","N-NE","NE","E-NE","Est","E-SE","SE","S-SE",
    "Sud","S-SO","SO","O-SO","Ouest","O-NO","NO","N-NO"
  )
  
  # ensure order by angle
  df <- df |>
    dplyr::arrange(angle) |>
    dplyr::mutate(idx = dplyr::row_number())
  
  # map to radians explicitly (centers of bins)
  n_dir <- nrow(df)
  bin  <- 2 * pi / n_dir
  df <- df |>
    dplyr::mutate(theta = (idx - 1) * bin)
  
  # close polygon at 2*pi
  df_closed <- dplyr::bind_rows(
    df,
    df[1, ] |> dplyr::mutate(theta = 2 * pi)
  )
  
  # same start logic as stacked: -bin/2 + orientation
  start_angle <- 0 - bin / 2 + (ajustement_orientation * pi / 180)
  subtitle_commune <- paste(
    "Hauteur:", rose_vents$hauteur,
    "| Période:", rose_vents$periode["debut"], "à", rose_vents$periode["fin"]
  )
  caption_commune <- paste(
    "Source: NASA POWER | Coordonnées:",
    round(rose_vents$coordonnees["longitude"], 4), ",",
    round(rose_vents$coordonnees["latitude"], 4)
  )
  
  if (type == "pct") {
    
    p <- ggplot2::ggplot(df_closed, ggplot2::aes(x = theta, y = pct, group = 1)) +
      ggplot2::geom_polygon(fill = palette, alpha = 0.35, color = palette, linewidth = 1) +
      ggplot2::geom_point(color = palette, size = 1.8) +
      ggplot2::coord_radial(
        start = 0,
        expand = FALSE,
        inner.radius = 0.10,
        r.axis.inside = 0,
        reverse = "none"
      ) +
      ggplot2::scale_x_continuous(
        breaks = (0:(n_dir - 1)) * bin,
        labels = dir_names,
        limits = c(0, 2 * pi),
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(labels = scales::label_percent(scale = 1)) +
      ggplot2::labs(
        title = "Rose des vents – Fréquence des directions",
        subtitle = subtitle_commune,
        x = NULL,
        y = "Fréquence (%)",
        caption = caption_commune
      )
    
  } else if (type == "avg") {
    
    p <- ggplot2::ggplot(df_closed, ggplot2::aes(x = theta, y = avg, group = 1)) +
      ggplot2::geom_polygon(fill = palette, alpha = 0.35, color = palette, linewidth = 1) +
      ggplot2::geom_point(color = palette, size = 1.8) +
      ggplot2::coord_radial(
        start = 0,
        expand = FALSE,
        inner.radius = 0.10,
        r.axis.inside = 0,
        reverse = "none"
      ) +
      ggplot2::scale_x_continuous(
        breaks = (0:(n_dir - 1)) * bin,
        labels = dir_names,
        limits = c(0, 2 * pi),
        expand = c(0, 0)
      ) +
      ggplot2::scale_y_continuous(labels = scales::label_number(suffix = " m/s")) +
      ggplot2::labs(
        title = "Rose des vents – Vitesse moyenne par direction",
        subtitle = subtitle_commune,
        x = NULL,
        y = "Vitesse moyenne (m/s)",
        caption = caption_commune
      )
    
    
  } 
  
  p +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 10, face = "bold"),
      axis.text.y = if (afficher_legende) ggplot2::element_text(size = 9) else ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      legend.position = if (afficher_legende) "right" else "none",
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle = ggplot2::element_text(size = 10)
    )
}

#' Créer une rose des vents complète avec barres empilées
#'
#' Crée une visualisation traditionnelle de rose des vents avec les classes
#' de vitesse empilées. Le Nord est affiché en haut par défaut.
#'
#' @param rose_vents Résultat de la fonction `obtenir_rose_vents()`
#' @param palette Palette de couleurs (vecteur de 10 couleurs)
#' @param ajustement_orientation Angle d'ajustement en degrés pour aligner avec l'orientation du champ (défaut: 0)
#'
#' @return Un objet ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' rose <- obtenir_rose_vents(champ)
#' graphique <- tracer_rose_vents_stacked(rose)
#'
#' # Ajuster avec l'orientation du champ (ex: 45°)
#' graphique <- tracer_rose_vents_stacked(rose, ajustement_orientation = 45)
#' }
#'
#' @importFrom ggplot2 ggplot geom_bar coord_polar scale_fill_manual
#' @importFrom ggplot2 theme_minimal labs theme element_text
tracer_rose_vents_stacked <- function(rose_vents,
                                      palette = NULL,
                                      ajustement_orientation = 0) {
  
  stopifnot(
    requireNamespace("ggplot2", quietly = TRUE),
    requireNamespace("dplyr", quietly = TRUE),
    requireNamespace("tidyr", quietly = TRUE)
  )
  
  if (is.null(palette)) {
    palette <- c(
      "#4575b4", "#74add1", "#abd9e9", "#e0f3f8",
      "#fee090", "#fdae61", "#f46d43", "#d73027",
      "#a50026", "#7f0000"
    )
  }
  
  dir_names <- c(
    "Nord","N-NE","NE","E-NE","Est","E-SE","SE","S-SE",
    "Sud","S-SO","SO","O-SO","Ouest","O-NO","NO","N-NO"
  )
  
  # ---- classes long format ----
  classes_df <- as.data.frame(rose_vents$classes, check.names = FALSE)
  classes_df$direction <- factor(dir_names, levels = dir_names)
  
  df_long <- tidyr::pivot_longer(
    classes_df,
    cols = -direction,
    names_to = "classe",
    values_to = "pct"
  )
  
  # ---- class labels ----
  hauteur_param <- ifelse(rose_vents$hauteur == "10m", "WR10M", "WR50M")
  classes_info <- rose_vents$data$messages[[1]]$CLASSES[[hauteur_param]]
  
  if (!is.null(classes_info)) {
    class_labels <- unlist(classes_info)
    names(class_labels) <- names(classes_info)
  } else {
    class_labels <- setNames(
      c(" 0.0 –  5.4"," 5.4 – 10.8","10.8 - 15.8","15.8 – 18.4","18.4 – 20.2",
        "20.2 – 21.6","21.6 – 23.0","23.0 – 25.2","25.2 – 33.8","33.8 +"),
      paste0("CLASS_", 1:10)
    )
  }
  
  df_long$classe <- factor(
    df_long$classe,
    levels = names(class_labels),
    labels = class_labels
  )
  
  # ---- radial coord parameters ----
  n_dir <- length(dir_names)
  bin <- 2 * pi / n_dir
  
  start_angle <- 0-bin/2 + (ajustement_orientation * pi / 180)
  ggplot2::ggplot(
    df_long,
    ggplot2::aes(x = direction, y = pct, fill = classe)
  ) +
    ggplot2::geom_col(width = 1, color = "white", linewidth = 0.25) +
    
    ggplot2::coord_radial(
      start = start_angle,
      expand = FALSE,
      inner.radius = 0.1,
      r.axis.inside = 1 ,   # <- axe radial à l’intérieur
      reverse = "none"      # <- remplace direction=1 (horaire). Si c'est inversé: "none"
    ) +
    
    ggplot2::scale_fill_manual(values = palette, name = "Vitesse du vent (km/h)") +
    ggplot2::scale_y_continuous(labels = scales::label_percent(scale  = 1)) +
    ggplot2::labs(
      title = paste0("Fréquence (%) des catégories de vents – ", rose_vents$hauteur),
      subtitle = paste(
        "Période:", lubridate::as_date(rose_vents$periode["debut"]), "à", lubridate::as_date(rose_vents$periode["fin"])
      ),
      x = NULL,
      y = "Fréquence (%)",
      caption = paste(
        "NASA POWER |",
        round(rose_vents$coordonnees["longitude"], 4),
        round(rose_vents$coordonnees["latitude"], 4)
      )
    ) +
    
    ggplot2::theme_minimal() +
    ggplot2::theme(
      # panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(size = 10, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 9),
      axis.title.y = ggplot2::element_blank(),
      legend.position = "right",
      plot.title = ggplot2::element_text(face = "bold", size = 14)
    )
}


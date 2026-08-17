#' Télécharger et traiter les données pédologiques pour un champ
#'
#' Cette fonction télécharge les polygones de couverture pédologique du Québec,
#' les filtre pour l'emprise du champ, et joint les données de texture et de
#' proportions de séries (PPS) incluses dans le package.
#'
#' @param champ Un objet `sf` représentant l'emprise du champ.
#' @param url_pedologie URL du fichier FlatGeobuf de pédologie (défaut: geoqc).
#' @param path_texture Chemin vers le fichier RDS de texture (défaut: data interne).
#' @param path_pps Chemin vers le fichier RDS de PPS (défaut: data interne).
#'
#' @return Une liste contenant:
#' \itemize{
#'   \item `polygones`: Un objet `sf` des polygones pédologiques intersectant le champ.
#'   \item `table_series`: Un `data.frame` formaté pour `proba_et_classement_serie_quota_ilr()`.
#' }
#' @export
#'
#' @importFrom sf st_read st_crs st_transform st_bbox st_as_text st_geometry st_as_sf st_union st_centroid st_convex_hull st_as_sfc
#' @importFrom dplyr inner_join select mutate filter .data
telecharger_pedologie_quebec <- function(champ, 
                                          url_pedologie = "https://storage.googleapis.com/geoqc/Pedologie/couverture_pedologique.fgb",
                                          path_texture = system.file("data", "texture.rds", package = "covariablechamps"),
                                          path_pps = system.file("data", "couverture_pps.rds", package = "covariablechamps")) {
  
  if (!requireNamespace("sf", quietly = TRUE)) stop("Le package 'sf' requis.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Le package 'dplyr' requis.")
  
  # 1. Préparer le filtre WKT sur l'emprise du champ
  champ_wgs84 <- sf::st_transform(champ, 4326)
  bbox <- sf::st_bbox(champ_wgs84)
  bbox_poly <- sf::st_as_sfc(bbox)
  
  # 2. Télécharger les polygones via vsicurl
  message("Téléchargement des polygones pédologiques (emprise champ)...")
  path_vsi <- paste0("/vsicurl/", url_pedologie)
  
  # Premier passage : polygones intersectant le champ
  polygones <- sf::st_read(path_vsi, wkt_filter = sf::st_as_text(sf::st_geometry(champ_wgs84)), quiet = TRUE)  
  if (nrow(polygones) == 0) {
    warning("Aucun polygone pédologique trouvé pour cette emprise.")
    return(NULL)
  }
  
  # 2b. Deuxième passage pour inclure les voisins
  message("Extraction des polygones voisins...")
  bbox_total_poly <- sf::st_as_sf(sf::st_union(polygones)) |> sf::st_transform(3857) |> sf::st_buffer(1) |> sf::st_transform(4326)
  polygones <- sf::st_read(path_vsi, wkt_filter = sf::st_as_text(sf::st_geometry(bbox_total_poly)), quiet = TRUE)
  
  # 3. Charger les tables de données internes
  message("Chargement des données de texture et PPS...")
  if (path_texture == "" || !file.exists(path_texture)) path_texture <- system.file("data", "texture.rds", package = "covariablechamps")
  if (path_pps == "" || !file.exists(path_pps)) path_pps <- system.file("data", "couverture_pps.rds", package = "covariablechamps")
  
  # Fallback dev
  if (!file.exists(path_texture)) path_texture <- "data/texture.rds"
  if (!file.exists(path_pps)) path_pps <- "data/couverture_pps.rds"
  
  if (!file.exists(path_texture)) stop("Fichier texture introuvable.")
  if (!file.exists(path_pps)) stop("Fichier PPS introuvable.")
  
  texture <- readRDS(path_texture)
  pps <- readRDS(path_pps)
  
  # 4. Joindre les données
  # Détecter la colonne de liaison par contenu
  col_poly <- NULL
  sample_ids <- unique(pps[["Code polygone"]])
  for (cn in names(polygones)) {
    if (cn == "geometry") next
    if (any(as.character(polygones[[cn]]) %in% sample_ids)) {
      col_poly <- cn
      break
    }
  }
  
  # Fallback par nom
  if (is.null(col_poly)) {
    candidats <- c("CODE_POLY", "Code polygone", "ID_POLY", "code_poly", "ID_UNIT_PED")
    col_poly <- intersect(names(polygones), candidats)[1]
  }
  
  if (is.null(col_poly)) {
    warning("Impossible de détecter la colonne d'identifiant polygone.")
    ids_telecharges <- character(0)
  } else {
    ids_telecharges <- unique(as.character(polygones[[col_poly]]))
  }
  
  table_series <- pps %>%
    dplyr::filter(.data[["Code polygone"]] %in% ids_telecharges) %>%
    dplyr::inner_join(texture, by = "Composante") %>%
    dplyr::select(
      Code.polygone = "Code polygone",
      "Composante",
      "Pourcentage",
      "Sable",
      "Limon",
      "Argile"
    )

  # S'assurer que les polygones ont une colonne 'Code.polygone'
  if (!is.null(col_poly)) {
    polygones$Code.polygone <- as.character(polygones[[col_poly]])
  }
    
  return(list(
    polygones = polygones,
    table_series = table_series
  ))
}

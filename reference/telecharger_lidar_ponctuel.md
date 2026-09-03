# Télécharger les données LiDAR ponctuelles (COPC) pour une zone

Cette fonction télécharge les données LiDAR ponctuelles au format COPC
(Cloud Optimized Point Cloud) depuis CanElevation (Canada) ou Données
Québec pour une zone d'intérêt donnée avec un buffer.

## Usage

``` r
telecharger_lidar_ponctuel(
  polygone,
  buffer = 50,
  source = "auto",
  dossier = NULL,
  metriques = TRUE,
  annee = NULL,
  toutes_annees = FALSE
)
```

## Arguments

- polygone:

  Un objet \`sf\` représentant la zone d'intérêt ou un chemin vers un
  fichier vectoriel

- buffer:

  Distance du buffer en mètres (défaut: 50)

- source:

  Source des données: "auto" (défaut), "canelevation", ou
  "donneesquebec"

- dossier:

  Dossier de sortie pour sauvegarder les fichiers (optionnel)

- métriques:

  Logique. Si TRUE, calcule les métriques de hauteur

- annee:

  Optionnel. Année spécifique à télécharger (ex: 2018). Si fourni,
  ignore \`toutes_annees\`.

- toutes_annees:

  Logique. Si TRUE, télécharge toutes les années disponibles (données
  historiques de Données Québec) et retourne une liste nommée par année.
  Défaut: FALSE.

## Value

Une liste contenant le nuage de points (objet LAS) et les métriques
calculées, ou une liste de ces listes (une par année) si \`toutes_annees
= TRUE\`.

## Examples

``` r
if (FALSE) { # \dontrun{
# Extraire le LiDAR ponctuel avec un buffer de 50m
champ <- sf::st_read("champ.shp")
lidar_points <- telecharger_lidar_ponctuel(champ, buffer = 50)

# Extraire une année spécifique
lidar_2018 <- telecharger_lidar_ponctuel(champ, annee = 2018)

# Extraire toutes les années disponibles (séparées par année)
lidar_toutes <- telecharger_lidar_ponctuel(champ, toutes_annees = TRUE)

# Visualiser
plot(lidar_points$nuage_points)
print(lidar_points$metriques)
} # }
```

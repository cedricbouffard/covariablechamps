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
  metriques = TRUE
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

## Value

Une liste contenant le nuage de points (objet LAS) et les métriques
calculées

## Examples

``` r
if (FALSE) { # \dontrun{
# Extraire le LiDAR ponctuel avec un buffer de 50m
champ <- sf::st_read("champ.shp")
lidar_points <- telecharger_lidar_ponctuel(champ, buffer = 50)

# Visualiser
plot(lidar_points$nuage_points)
print(lidar_points$metriques)
} # }
```

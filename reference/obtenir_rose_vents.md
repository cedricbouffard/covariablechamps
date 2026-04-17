# Obtenir la rose des vents depuis NASA POWER

Récupère les données de rose des vents (distribution des directions et
vitesses) depuis l'API NASA POWER pour le centroïde d'un champ.

## Usage

``` r
obtenir_rose_vents(
  polygone,
  date_debut = "20230101",
  date_fin = "20231231",
  hauteur = "10m"
)
```

## Arguments

- polygone:

  Un objet \`sf\` représentant le champ ou un vecteur c(longitude,
  latitude)

- date_debut:

  Date de début au format "YYYYMMDD" (défaut: "20230101")

- date_fin:

  Date de fin au format "YYYYMMDD" (défaut: "20231231")

- hauteur:

  Hauteur des mesures: "10m" ou "50m" (défaut: "10m")

## Value

Une liste contenant:

- data:

  Les données brutes de l'API

- directions:

  Vecteur des 16 directions (0-337.5°)

- wd_pct:

  Pourcentages de fréquence par direction

- wd_avg:

  Vitesse moyenne du vent par direction (m/s)

- classes:

  Matrice des classes de vent par direction

- all_classes:

  Pourcentages globaux par classe de vent

- coordonnees:

  Coordonnées du point (longitude, latitude)

## Examples

``` r
if (FALSE) { # \dontrun{
# Pour un champ
champ <- sf::st_read("champ.shp")
rose <- obtenir_rose_vents(champ)

# Pour des coordonnées spécifiques
rose <- obtenir_rose_vents(c(-71.055, 46.648))
} # }
```

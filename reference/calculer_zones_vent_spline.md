# Calculer les zones de vent avec une spline pour chaque haie

Crée des zones de vent (demi-lunes) derrière chaque haie. Les zones sont
construites par facteurs de la hauteur H de la haie (ex: 1H, 2H, ...).

## Usage

``` r
calculer_zones_vent_spline(
  haies_rectangles,
  direction_vent = 225,
  facteur_hauteur = 1:40,
  champ_centroid = NULL,
  n_points = 50
)
```

## Arguments

- haies_rectangles:

  Objet sf avec les haies rectangulaires (colonnes: x_center, y_center,
  hauteur_p95, etc.)

- direction_vent:

  Direction du vent en degrés (degrés géographiques, 0=Nord, 90=Est)
  (défaut: 225)

- facteur_hauteur:

  Vecteur des facteurs H à calculer (défaut: 1:40)

- champ_centroid:

  Centroïde du champ pour déterminer le sens de protection (aval/amont)
  (défaut: NULL)

- n_points:

  Nombre de points pour le spline de l'apex (défaut: 50)

## Value

Un objet sf avec les polygones des zones de vent. Attributs:

- cluster, n_arbres, hauteur_p95, largeur, longueur, angle_haie_deg

- facteur_h: multiple de la hauteur H pour cette zone

- direction_vent: direction utilisée

- distance_H: distance H calculée pour ce facteur

- orientation_protection: "amont" ou "aval" selon la position du champ

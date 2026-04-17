# Calculer les zones d'influence du vent sur les haies avec une forme de demi-lune

A partir des rectangles de haies, calcule des zones de
protection/d'influence en forme de demi-lune dans le sens du vent
dominant. La forme part des deux extrémités de la haie et s'étend en
courbe dans le sens du vent avec une distance H (multiple de la
hauteur), créant une forme spline arrondie.

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

  Objet sf avec les rectangles des haies (résultat de
  extraire_classifier_haies_lidar)

- direction_vent:

  Direction du vent dominant en degrés (0-360, 0 = Nord, 90 = Est, etc.)

- facteur_hauteur:

  Vecteur de multiplicateurs de la hauteur pour calculer les distances H
  (ex: 1:40)

- champ_centroid:

  Point central du champ (sf POINT ou vecteur c(x, y)) pour déterminer
  le sens de la protection

- n_points:

  Nombre de points pour la courbe spline (défaut: 50)

## Value

sf POLYGON avec les zones de protection pour tous les facteurs.
Attributs: - cluster, n_arbres, hauteur_p95, largeur, longueur,
angle_deg (de la haie) - facteur_h: le multiple de H utilisé pour cette
zone (1, 2, 3, etc.) - direction_vent: direction utilisée - distance_H:
distance H calculée pour ce facteur - orientation_protection: "amont" ou
"aval" selon la position du champ

## Details

Cette fonction permet de créer plusieurs couches de zones avec
différents facteurs de hauteur (1H, 2H, 3H, etc. jusqu'à max_H).

# Calculer les distances aux arbres selon la direction du vent

Calculer les distances aux arbres selon la direction du vent

## Usage

``` r
calculer_distances_vent(
  arbres_sf,
  angle_vent,
  champ_bbox,
  resolution = 2,
  max_distance = 100,
  ouverture_angulaire = 45
)
```

## Arguments

- arbres_sf:

  Objet sf POINT

- angle_vent:

  Angle du vent en degrés (0-360)

- champ_bbox:

  Objet sf avec le contour du champ

- resolution:

  Résolution du raster en mètres

- max_distance:

  Distance maximale à calculer

- ouverture_angulaire:

  Angle d'ouverture en degrés

## Value

Liste avec les distances

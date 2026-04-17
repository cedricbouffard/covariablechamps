# Calculer les distances amont/aval avec buffer et lissage

Calculer les distances amont/aval avec buffer et lissage

## Usage

``` r
calculer_distances_amont_aval(
  arbres_sf,
  angle_vent,
  champ_bbox,
  resolution = 2,
  buffer_arbre = 3,
  angle_focal = 45,
  max_distance = 200,
  taille_lissage = 7
)
```

## Arguments

- arbres_sf:

  Objet sf POINT avec les arbres

- angle_vent:

  Angle du vent en degrés (0=Nord, 90=Est)

- champ_bbox:

  Objet sf avec le contour du champ

- resolution:

  Résolution du raster en mètres

- buffer_arbre:

  Rayon du buffer autour des arbres (m)

- angle_focal:

  Angle focal en degrés

- max_distance:

  Distance maximale

- taille_lissage:

  Taille de la fenêtre de lissage (cellules)

## Value

Liste avec les rasters amont et aval

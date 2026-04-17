# Calculer la distance aux arbres avec buffer

Calcule pour chaque cellule la distance euclidienne au buffer autour des
arbres les plus proches.

## Usage

``` r
calculer_distance_arbres(
  arbres_sf,
  champ_bbox,
  resolution = 2,
  buffer_arbre = 3,
  max_distance = 200,
  taille_lissage = 5
)
```

## Arguments

- arbres_sf:

  Objet sf POINT avec les arbres

- champ_bbox:

  Objet sf avec le contour du champ

- resolution:

  Résolution du raster en mètres

- buffer_arbre:

  Rayon du buffer autour des arbres (m)

- max_distance:

  Distance maximale à calculer

- taille_lissage:

  Taille de la fenêtre de lissage (cellules)

## Value

Liste avec les rasters

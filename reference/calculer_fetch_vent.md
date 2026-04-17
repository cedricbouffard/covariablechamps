# Créer une carte de fetch de vent avec effet directionnel (elliptique)

Crée une carte montrant la distance aux arbres dans la direction du
vent, avec un effet de buffer elliptique aligné avec le vent.

## Usage

``` r
calculer_fetch_vent(
  arbres_sf,
  angle_vent,
  champ_bbox,
  resolution = 2,
  max_fetch = 200,
  coef_ellipse = 3
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

- max_fetch:

  Distance maximale

- coef_ellipse:

  Ratio d'élongation (2 = 2x plus long dans la direction du vent)

## Value

Liste avec les rasters

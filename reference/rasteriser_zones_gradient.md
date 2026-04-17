# Rasteriser les zones de vent avec le facteur H minimum par pixel

Convertit les zones de vent (demi-lunes avec multiples facteurs H) en
raster. Chaque pixel contient le facteur H minimum parmi toutes les
zones qui couvrent ce pixel. Par exemple, si un pixel est couvert par
les zones 1H, 3H et 5H, il aura la valeur 1.

## Usage

``` r
rasteriser_zones_gradient(zones_sf, resolution = 1, extent_bbox = NULL)
```

## Arguments

- zones_sf:

  Objet sf avec les zones de vent (résultat de
  calculer_zones_vent_spline)

- resolution:

  Résolution du raster en mètres (défaut: 1)

- extent_bbox:

  Bounding box optionnel (objet bbox ou NULL pour calculer
  automatiquement)

## Value

SpatRaster avec les valeurs de facteur H minimum pour chaque pixel

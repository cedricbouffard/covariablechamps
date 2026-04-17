# Rasteriser les zones de vent avec le facteur H minimum par pixel (version corrigée)

Cette version utilise une approche différente qui garantit le calcul
correct du minimum H par pixel.

## Usage

``` r
rasteriser_zones_gradient_v2(zones_sf, resolution = 1, extent_bbox = NULL)
```

## Arguments

- zones_sf:

  Objet sf avec les zones de vent

- resolution:

  Résolution du raster en mètres (défaut: 1)

- extent_bbox:

  Bounding box optionnel

## Value

SpatRaster avec les valeurs de facteur H minimum pour chaque pixel

# Fusionner et disjoindre les zones de vent par facteur H

Prend les zones brutes et retourne une couche où chaque facteur H est
fusionné et rendu disjoint des facteurs plus petits (effet de
couronnes).

## Usage

``` r
fusionner_zones_vent(zones_sf)
```

## Arguments

- zones_sf:

  Résultat de calculer_zones_vent_spline

## Value

Un objet sf avec une ligne par facteur H, géométries fusionnées

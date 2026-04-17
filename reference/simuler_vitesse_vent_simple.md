# Simuler la vitesse du vent basée sur la distance simple aux arbres

Cette fonction est utilisée avec le résultat de
[`calculer_distance_arbres()`](https://votrenom.github.io/covariablechamps/reference/calculer_distance_arbres.md).
Pour les distances directionnelles (amont/aval), utilisez plutôt
[`simuler_vitesse_vent()`](https://votrenom.github.io/covariablechamps/reference/simuler_vitesse_vent.md)
qui prend le résultat de
[`calculer_distances_amont_aval()`](https://votrenom.github.io/covariablechamps/reference/calculer_distances_amont_aval.md).

## Usage

``` r
simuler_vitesse_vent_simple(
  dist_result,
  vitesse_ref = 5,
  coef_protection = 0.5
)
```

## Arguments

- dist_result:

  Résultat de calculer_distance_arbres()

- vitesse_ref:

  Vitesse de référence (m/s)

- coef_protection:

  Coefficient de protection (0-1)

## Value

Liste avec: vitesse (raster), vitesse_ref, coef_protection

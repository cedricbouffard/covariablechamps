# Récapitulatif des fonctions distance_arbres

Ce module fournit des fonctions pour calculer et visualiser les
distances aux arbres.

## Details

- calculer_distance_arbres(): Distance euclidienne simple aux arbres

- calculer_distances_amont_aval(): Distance directionnelle (amont/aval)

- visualiser_distance_arbres(): Visualiser la distance simple

- visualiser_distances_vent(): Visualiser la distance directionnelle

- simuler_vitesse_vent_simple(): Simuler la vitesse du vent (distance
  simple)

- simuler_vitesse_vent(): Simuler la vitesse du vent (amont/aval)

## Examples

``` r
if (FALSE) { # \dontrun{
# Distance simple
dist <- calculer_distance_arbres(arbres, champ, buffer_arbre = 3)
visualiser_distance_arbres(dist, type = "buffer")

# Vitesse du vent (distance simple)
vitesse <- simuler_vitesse_vent_simple(dist, vitesse_ref = 5, coef_protection = 0.5)

# Distance directionnelle (amont/aval)
dist_dir <- calculer_distances_amont_aval(arbres, 245, champ, buffer_arbre = 3)
visualiser_distances_vent(dist_dir, type = "comparaison")

# Vitesse du vent (direction du vent)
vitesse <- simuler_vitesse_vent(dist_dir, vitesse_ref = 5)
} # }
```

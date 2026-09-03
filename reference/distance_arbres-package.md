# Récapitulatif des fonctions distance_arbres

Ce module fournit des fonctions pour calculer et visualiser les
distances aux arbres.

## Details

- calculer_distance_arbres(): Distance euclidienne simple aux arbres

- calculer_distances_vent(): Distance directionnelle (amont/aval)

- visualiser_distance_arbres(): Visualiser la distance simple

- tracer_carte_vent(): Visualiser la distance directionnelle

- simuler_vitesse_vent_simple(): Simuler la vitesse du vent (distance
  simple)

- simuler_vitesse_vent_fetch(): Simuler la vitesse du vent (fetch)

## Examples

``` r
if (FALSE) { # \dontrun{
# Distance simple
dist <- calculer_distance_arbres(arbres, champ, buffer_arbre = 3)
visualiser_distance_arbres(dist, type = "buffer")

# Vitesse du vent (distance simple)
vitesse <- simuler_vitesse_vent_simple(dist, vitesse_ref = 5, coef_protection = 0.5)

# Distance directionnelle (amont/aval)
dist_dir <- calculer_distances_vent(arbres, 245, champ, buffer_arbre = 3)
tracer_carte_vent(dist_dir, type = "les_deux")

# Fetch de vent et vitesse associée
fetch <- calculer_fetch_vent(arbres, 245, champ, max_fetch = 200)
vitesse <- simuler_vitesse_vent_fetch(fetch, vitesse_ref = 5)
} # }
```

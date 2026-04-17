# Classifier les distances aux bordures

Crée des classes de distance aux bordures du champ selon des seuils
personnalisables. Cette fonction permet de catégoriser les zones du
champ en fonction de leur proximité aux bordures, utile pour:

## Usage

``` r
classifier_distances_bordures(
  dist_result,
  seuils_long = c(10, 25, 50, 100),
  seuils_large = c(5, 15, 30, 50),
  labels_long = NULL,
  labels_large = NULL
)
```

## Arguments

- dist_result:

  Liste: Résultat de
  [`calculer_distance_bordures_orientee`](https://votrenom.github.io/covariablechamps/reference/calculer_distance_bordures_orientee.md).

- seuils_long:

  Numeric vector: Seuils de classification (m) pour la distance dans le
  sens long. Défaut: c(10, 25, 50, 100).

- seuils_large:

  Numeric vector: Seuils de classification (m) pour la distance dans le
  sens large. Défaut: c(5, 15, 30, 50).

- labels_long:

  Character vector: Noms des classes pour le sens long. Doit avoir
  length(seuils_long) + 1 éléments. Défaut: c("bordure_long",
  "proche_long", "intermediaire_long", "eloigne_long", "centre_long").

- labels_large:

  Character vector: Noms des classes pour le sens large. Défaut:
  c("bordure_large", "proche_large", "intermediaire_large",
  "eloigne_large", "centre_large").

## Value

Liste contenant:

- classe_long:

  SpatRaster: Classification selon la distance sens long

- classe_large:

  SpatRaster: Classification selon la distance sens large

- classe_combinee:

  SpatRaster: Classification combinée (long \* 10 + large) permettant
  d'identifier chaque combinaison unique

- seuils_long:

  Les seuils utilisés pour le sens long

- seuils_large:

  Les seuils utilisés pour le sens large

- labels_long:

  Les labels utilisés pour le sens long

- labels_large:

  Les labels utilisés pour le sens large

- table_classes:

  Data.frame avec la correspondance des codes

## Details

\- Définir des zones de bordure vs zones centrales - Stratifier les
échantillonnages - Identifier les zones d'influence des haies - Créer
des masques pour l'analyse

\## Interprétation des classes

Les classes sont numérotées de 1 (plus proche des bordures) à N (plus
éloigné). La classe combinée encode les deux informations: dizaines =
classe_long, unités = classe_large. Par exemple, 23 = classe 2 en long,
classe 3 en large.

\## Choix des seuils

Les seuils par défaut sont adaptés à des champs agricoles typiques: -
Sens long: 10m (bordure immédiate), 25m (zone tampon), 50m (transition),
100m - Sens large: 5m (bordure), 15m (tampon), 30m (transition), 50m

Adaptez ces seuils selon la taille de vos champs et l'effet étudié.

## See also

[`calculer_distance_bordures_orientee`](https://votrenom.github.io/covariablechamps/reference/calculer_distance_bordures_orientee.md)
pour calculer les distances,
[`visualiser_classes_bordures`](https://votrenom.github.io/covariablechamps/reference/visualiser_classes_bordures.md)
pour visualiser les classes

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculer les distances
dist <- calculer_distance_bordures_orientee(champ_poly = champ)

# Classifier avec les seuils par défaut
classes <- classifier_distances_bordures(dist)

# Visualiser
plot(classes$classe_long, main = "Classes sens long")
plot(classes$classe_large, main = "Classes sens large")

# Seuils personnalisés pour un petit champ
classes_petit <- classifier_distances_bordures(
  dist,
  seuils_long = c(5, 10, 20),
  seuils_large = c(3, 8, 15),
  labels_long = c("bord", "proche", "milieu", "centre"),
  labels_large = c("bord", "proche", "milieu", "centre")
)
} # }
```

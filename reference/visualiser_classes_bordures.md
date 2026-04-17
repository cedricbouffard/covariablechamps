# Visualiser les classes de distance aux bordures

Crée une visualisation cartographique des classes de distance aux
bordures. Utilise une palette de couleurs catégorielle pour distinguer
les classes.

## Usage

``` r
visualiser_classes_bordures(
  classes_result,
  type = c("long", "large", "combinee"),
  titre = NULL
)
```

## Arguments

- classes_result:

  Liste: Résultat de
  [`classifier_distances_bordures`](https://votrenom.github.io/covariablechamps/reference/classifier_distances_bordures.md).

- type:

  Character: Type de visualisation.

  "long"

  :   Classes selon la distance sens long

  "large"

  :   Classes selon la distance sens large

  "combinee"

  :   Classes combinées (toutes les combinaisons)

- titre:

  Character: Titre personnalisé (optionnel).

## Value

Un objet ggplot2

## See also

[`classifier_distances_bordures`](https://votrenom.github.io/covariablechamps/reference/classifier_distances_bordures.md)

## Examples

``` r
if (FALSE) { # \dontrun{
dist <- calculer_distance_bordures_orientee(champ_poly = champ)
classes <- classifier_distances_bordures(dist)
visualiser_classes_bordures(classes, type = "long")
} # }
```

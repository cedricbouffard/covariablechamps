# Visualiser les distances aux bordures orientées

Crée une visualisation cartographique des distances aux bordures selon
l'orientation du champ. Génère des cartes de chaleur montrant les
gradients de distance depuis les bordures du champ.

## Usage

``` r
visualiser_distances_bordures(
  dist_result,
  type = c("long", "large", "min", "comparaison"),
  titre = NULL
)
```

## Arguments

- dist_result:

  Liste: Résultat de
  [`calculer_distance_bordures_orientee`](https://votrenom.github.io/covariablechamps/reference/calculer_distance_bordures_orientee.md).
  Doit contenir les rasters distance_long, distance_large et
  distance_min.

- type:

  Character: Type de visualisation à produire.

  "long"

  :   Distance dans le sens de la longueur du champ

  "large"

  :   Distance dans le sens de la largeur du champ

  "min"

  :   Distance minimale à n'importe quelle bordure

  "comparaison"

  :   Les trois visualisations côte à côte

- titre:

  Character: Titre personnalisé pour le graphique (optionnel). Si NULL,
  un titre automatique est généré selon le type.

## Value

\- Si type != "comparaison": Un objet ggplot2 - Si type ==
"comparaison": Une liste nommée avec trois ggplot2 (long, large, min)
que vous pouvez arranger avec patchwork ou cowplot

## Details

La palette de couleurs utilisée va du bleu (distances faibles, proche
des bordures) au rouge (distances élevées, centre du champ).

Pour combiner les trois graphiques de "comparaison", utilisez:

    library(patchwork)
    plots <- visualiser_distances_bordures(dist, "comparaison")
    plots$long + plots$large + plots$min

## See also

[`calculer_distance_bordures_orientee`](https://votrenom.github.io/covariablechamps/reference/calculer_distance_bordures_orientee.md)
pour calculer les distances,
[`classifier_distances_bordures`](https://votrenom.github.io/covariablechamps/reference/classifier_distances_bordures.md)
pour classifier les distances

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculer les distances
dist <- calculer_distance_bordures_orientee(champ_poly = champ)

# Visualiser une seule distance
visualiser_distances_bordures(dist, type = "long")

# Visualiser les trois
plots <- visualiser_distances_bordures(dist, type = "comparaison")

# Avec patchwork
library(patchwork)
plots$long + plots$large + plots$min + plot_layout(ncol = 3)
} # }
```

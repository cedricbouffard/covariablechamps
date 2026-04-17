# Créer une rose des vents complète avec barres empilées

Crée une visualisation traditionnelle de rose des vents avec les classes
de vitesse empilées. Le Nord est affiché en haut par défaut.

## Usage

``` r
tracer_rose_vents_stacked(
  rose_vents,
  palette = NULL,
  ajustement_orientation = 0
)
```

## Arguments

- rose_vents:

  Résultat de la fonction \`obtenir_rose_vents()\`

- palette:

  Palette de couleurs (vecteur de 10 couleurs)

- ajustement_orientation:

  Angle d'ajustement en degrés pour aligner avec l'orientation du champ
  (défaut: 0)

## Value

Un objet ggplot2

## Examples

``` r
if (FALSE) { # \dontrun{
rose <- obtenir_rose_vents(champ)
graphique <- tracer_rose_vents_stacked(rose)

# Ajuster avec l'orientation du champ (ex: 45°)
graphique <- tracer_rose_vents_stacked(rose, ajustement_orientation = 45)
} # }
```

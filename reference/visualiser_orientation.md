# Visualiser l'orientation d'un champ

Crée une carte montrant le champ avec sa bounding box orientée et
l'angle principal

## Usage

``` r
visualiser_orientation(polygone, orientation = NULL, afficher_angles = TRUE)
```

## Arguments

- polygone:

  Un objet \`sf\` représentant le champ

- orientation:

  Résultat de \`calculer_orientation_champ()\` (optionnel)

- afficher_angles:

  Logique. Si TRUE, affiche les valeurs des angles sur la carte

## Value

Un objet ggplot2

## Examples

``` r
if (FALSE) { # \dontrun{
champ <- sf::st_read("champ.shp")
visualiser_orientation(champ)
} # }
```

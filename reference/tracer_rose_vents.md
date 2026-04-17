# Créer un graphique radar de la rose des vents

Visualise la distribution des directions du vent sous forme de graphique
radar. Le Nord est affiché en haut par défaut.

## Usage

``` r
tracer_rose_vents(
  rose_vents,
  type = c("pct", "avg", "both"),
  afficher_legende = TRUE,
  palette = "steelblue",
  ajustement_orientation = 0
)
```

## Arguments

- rose_vents:

  Résultat de la fonction \`obtenir_rose_vents()\`

- type:

  Type de visualisation: "pct" (fréquence), "avg" (vitesse moyenne)

- afficher_legende:

  Logique. Si TRUE, affiche la légende

- palette:

  Couleur de remplissage (défaut: "steelblue")

- ajustement_orientation:

  Angle d'ajustement en degrés pour aligner avec l'orientation du champ
  (défaut: 0)

## Value

Un objet ggplot2

## Examples

``` r
if (FALSE) { # \dontrun{
champ <- sf::st_read("champ.shp")
rose <- obtenir_rose_vents(champ)
graphique <- tracer_rose_vents(rose)
print(graphique)

# Ajuster avec l'orientation du champ (ex: 45°)
graphique <- tracer_rose_vents(rose, ajustement_orientation = 45)
} # }
```

# Visualiser les distances amont/aval avec les flèches de vent

Visualiser les distances amont/aval avec les flèches de vent

## Usage

``` r
tracer_carte_vent(
  distances,
  type = c("les_deux", "amont", "aval", "total"),
  titre = NULL,
  n_fleches = 5,
  taille_fleches = 0.2
)
```

## Arguments

- distances:

  Résultat de calculer_distances_vent()

- type:

  Type: "amont", "aval", "total" ou "les_deux"

- titre:

  Titre du graphique

- n_fleches:

  Nombre de flèches

- taille_fleches:

  Taille des flèches

## Value

Un objet ggplot2

## Examples

``` r
if (FALSE) { # \dontrun{
carte <- tracer_carte_vent(distances, type = "amont")
print(carte)
} # }
```

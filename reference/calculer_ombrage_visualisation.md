# Visualiser l'ombrage d'une parcelle

Cree une visualisation de l'ombrage calcule avec trois panneaux : le
MNE, l'ombrage moyen, et les heures d'ensoleillement.

## Usage

``` r
calculer_ombrage_visualisation(
  resultats_ombrage,
  couche_heure = 1,
  titre = NULL
)
```

## Arguments

- resultats_ombrage:

  Liste retournee par \`calculer_ombrage()\`

- couche_heure:

  Numero de la couche horaire a afficher (defaut: 1)

- titre:

  Titre optionnel pour le graphique

## Value

NULL (affiche le graphique)

## Examples

``` r
if (FALSE) { # \dontrun{
resultats <- calculer_ombrage(champ, date = "2024-06-21")
visualiser_ombrage(resultats)
} # }
```

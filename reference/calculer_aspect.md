# Calculer l'aspect (orientation) à partir d'un MNT

Calcule l'aspect (orientation en degrés, 0=Nord, 90=Est, 180=Sud,
270=Ouest) à partir d'un modèle numérique de terrain.

## Usage

``` r
calculer_aspect(mnt, filename = NULL)
```

## Arguments

- mnt:

  Un objet \`SpatRaster\` représentant le MNT

- filename:

  Chemin de fichier pour sauvegarder le résultat (optionnel)

## Value

Un objet \`SpatRaster\` contenant l'aspect en degrés

## Examples

``` r
if (FALSE) { # \dontrun{
mnt <- telecharger_lidar(champ)
aspect <- calculer_aspect(mnt)
} # }
```

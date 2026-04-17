# Calculer la pente à partir d'un MNT

Calcule la pente en degrés à partir d'un modèle numérique de terrain
(MNT).

## Usage

``` r
calculer_pente(mnt, filename = NULL)
```

## Arguments

- mnt:

  Un objet \`SpatRaster\` représentant le MNT

- filename:

  Chemin de fichier pour sauvegarder le résultat (optionnel)

## Value

Un objet \`SpatRaster\` contenant la pente en degrés

## Examples

``` r
if (FALSE) { # \dontrun{
mnt <- telecharger_lidar(champ)
pente <- calculer_pente(mnt)
} # }
```

# Vérifier la disponibilité des données LiDAR

Vérifie si des données LiDAR sont disponibles pour une zone donnée et
retourne des informations sur les années disponibles.

## Usage

``` r
verifier_disponibilite_lidar(polygone, mne = FALSE)
```

## Arguments

- polygone:

  Un objet \`sf\` représentant la zone d'intérêt ou un chemin de fichier

- mne:

  Logique. Si TRUE, vérifie la disponibilité du MNE. Sinon, du MNT.

## Value

Un data.frame avec les années et nombre de tuiles disponibles

## Examples

``` r
if (FALSE) { # \dontrun{
info <- verifier_disponibilite_lidar(champ)
print(info)
} # }
```

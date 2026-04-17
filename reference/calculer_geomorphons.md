# Calculer les géomorphons à partir d'un MNT

Utilise le package rgeomorphon pour classifier les formes de terrain
selon la méthode des géomorphons (Jasiewicz et Stepinski, 2013).

## Usage

``` r
calculer_geomorphons(
  mnt,
  search_radius = 100,
  skip_radius = 0,
  flatness_threshold = 0.05,
  filename = NULL,
  multi_scale = FALSE
)
```

## Arguments

- mnt:

  Un objet \`SpatRaster\` représentant le MNT

- search_radius:

  Rayon de recherche en mètres (défaut: 100)

- skip_radius:

  Rayon à ignorer autour du pixel central en mètres (défaut: 0)

- flatness_threshold:

  Seuil de platitude en pourcent de pente (défaut: 0.05)

- filename:

  Chemin de fichier pour sauvegarder le résultat (optionnel)

- multi_scale:

  Logique. Si TRUE, calcule aux échelles petite, moyenne et grande

## Value

Un objet \`SpatRaster\` ou une liste de 3 rasters si multi_scale=TRUE

## Details

Les géomorphons identifient 10 formes de terrain avec des labels en
français: 1 = Plat, 2 = Pic, 3 = Crête, 4 = Épaulement, 5 = Éperon, 6 =
Pente, 7 = Creux, 8 = Pied de pente, 9 = Vallée, 10 = Fosse

## Examples

``` r
if (FALSE) { # \dontrun{
mnt <- telecharger_lidar(champ)
geom <- calculer_geomorphons(mnt, search_radius = 50)

# Calculer aux 3 échelles automatiquement
geom_multi <- calculer_geomorphons(mnt, multi_scale = TRUE)
} # }
```

# Calculer les probabilités et le classement des séries avec quota ILR

Cette fonction calcule les probabilités d'appartenance aux séries de sol
pour chaque pixel en utilisant une combinaison de:

- Texture du sol (proportions sable/limon/argile)

- Modèle numérique de terrain (MNT)

- Voisinage spatial (lissage)

Elle effectue ensuite une assignation par quota optimisée en utilisant
la transformation ILR (Isometric Log-Ratio) pour comparer les
compositions.

## Usage

``` r
proba_et_classement_serie_quota_ilr(
  polygones_sf,
  table_series,
  mnt,
  id_col = "Code.polygone",
  serie_col = "Composante",
  poids_col = "Pourcentage",
  sable_col = "Sable",
  limon_col = "Limon",
  argile_col = "Argile",
  grid_m = 20,
  sigma_voisin = 60,
  alpha_voisin = 8,
  beta_mnt = 0.2,
  lambda_base = 0.25,
  base_eps = 1e-06,
  sim_scale = c("zscore", "none"),
  sim_clip = 3,
  tempdir = NULL,
  write_intermediate = TRUE,
  verbose = TRUE,
  progress = TRUE
)
```

## Arguments

- polygones_sf:

  Un objet \`sf\` contenant les polygones (champs/parcelles)

- table_series:

  Un \`data.frame\` avec les informations de série par polygone

- mnt:

  Un objet \`SpatRaster\` (MNT/DEM)

- id_col:

  Nom de la colonne identifiant les polygones dans \`table_series\`

- serie_col:

  Nom de la colonne indiquant la série de sol

- poids_col:

  Nom de la colonne avec le poids/proportion de la série

- sable_col:

  Nom de la colonne avec le pourcentage de sable

- limon_col:

  Nom de la colonne avec le pourcentage de limon

- argile_col:

  Nom de la colonne avec le pourcentage d'argile

- grid_m:

  Résolution de la grille coarse en mètres (défaut: 20)

- sigma_voisin:

  Écart-type du noyau gaussien pour le voisinage en mètres (défaut: 60)

- alpha_voisin:

  Pondération de la similarité de voisinage (défaut: 8)

- beta_mnt:

  Pondération de l'effet du MNT sur la finesse (défaut: 0.2)

- lambda_base:

  Pondération de la probabilité de base (défaut: 0.25)

- base_eps:

  Epsilon pour éviter le log(0) (défaut: 1e-6)

- sim_scale:

  Échelle de normalisation: "zscore" ou "none" (défaut: "zscore")

- sim_clip:

  Valeur de truncation pour le z-score (défaut: 3)

- tempdir:

  Répertoire temporaire pour terra (défaut: NULL)

- write_intermediate:

  Écrire les rasters intermédiaires (défaut: TRUE)

- verbose:

  Afficher les messages de progression (défaut: TRUE)

- progress:

  Afficher la barre de progression (défaut: TRUE)

## Value

Une liste contenant:

- \`prob\`: Raster des probabilités par série (SpatRaster)

- \`ilr_mean\`: Raster des moyennes ILR (2 couches)

- \`dist\`: Raster des distances ILR par série

- \`id_r\`: Raster des identifiants de polygone

- \`closest_index\`: Raster avec l'index de la série assignée

- \`closest_label\`: Raster avec les étiquettes de série

- \`series\`: Vecteur des noms de séries

## Examples

``` r
if (FALSE) { # \dontrun{
resultat <- proba_et_classement_serie_quota_ilr(
  polygones_sf = champs_sf,
  table_series = table_series,
  mnt = dem_raster
)
} # }
```

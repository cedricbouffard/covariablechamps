# Calculer la distance aux bordures selon l'orientation du champ

Calcule pour chaque point ou cellule la distance aux bordures du champ
en distinguant le sens de la longueur (axe principal) et le sens de la
largeur (axe perpendiculaire). Cette fonction est utile pour analyser
l'effet de bordure dans les parcelles agricoles, notamment pour:

## Usage

``` r
calculer_distance_bordures_orientee(
  points_sf = NULL,
  champ_poly = NULL,
  resolution = 2,
  buffer = 50
)
```

## Arguments

- points_sf:

  Objet sf POINT ou SpatRaster avec les positions à évaluer. Si NULL
  (défaut), un raster est créé automatiquement sur l'emprise du champ.

- champ_poly:

  Objet sf POLYGON représentant le contour du champ. Requis. Doit avoir
  un CRS défini (projeté de préférence).

- resolution:

  Résolution du raster en mètres (défaut: 2). Utilisé uniquement si
  points_sf est NULL.

- buffer:

  Buffer autour du champ en mètres (défaut: 50). Définit la zone de
  calcul autour du champ.

## Value

Une liste contenant:

- distance_long:

  SpatRaster ou colonne sf: Distance (m) dans le sens de la longueur du
  champ (axe principal). Représente la composante de la distance
  projetée sur l'axe le plus long du champ.

- distance_large:

  SpatRaster ou colonne sf: Distance (m) dans le sens de la largeur du
  champ (axe perpendiculaire). Représente la composante projetée sur
  l'axe le plus court.

- distance_min:

  SpatRaster ou colonne sf: Distance euclidienne minimale (m) à
  n'importe quelle bordure du champ.

- orientation:

  Liste: Résultat de
  [`calculer_orientation_champ`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_orientation_champ.md),
  contenant l'angle principal, les dimensions et la géométrie du MBR.

- champ_buffer:

  sf: Zone tampon autour du champ utilisée pour les calculs.

En mode points (points_sf fourni), la sortie inclut `points` au lieu des
rasters distance\_\*, avec les distances comme colonnes.

## Details

\- Quantifier l'influence des haies brise-vent selon leur orientation -
Étudier les gradients de rendement depuis les bordures - Identifier les
zones d'ombrage potentiel - Planifier les prélèvements d'échantillons de
sol

\## Algorithme

La fonction procède en plusieurs étapes:

1\. \*\*Orientation\*\*: Calcule l'orientation principale du champ via
le Minimum Bounding Rectangle (MBR), déterminant l'axe long et l'axe
large.

2\. \*\*Extraction des bordures\*\*: Convertit le polygone en lignes
pour calculer les distances.

3\. \*\*Calcul des distances\*\*: Pour chaque point: - Trouve la bordure
la plus proche (distance euclidienne) - Calcule le vecteur vers cette
bordure - Projette ce vecteur sur les axes long et large

\## Interprétation des résultats

\- \*\*distance_long faible\*\*: Point proche d'une bordure
perpendiculaire à l'axe principal (extrémités courtes du champ) -
\*\*distance_large faible\*\*: Point proche d'une bordure parallèle à
l'axe principal (côtés longs du champ) - Les deux distances ensemble
permettent d'identifier la position relative dans le champ

## See also

[`calculer_orientation_champ`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_orientation_champ.md)
pour le calcul de l'orientation,
[`classifier_distances_bordures`](https://cedricbouffard.github.io/covariablechamps/reference/classifier_distances_bordures.md)
pour classifier les distances,
[`visualiser_distances_bordures`](https://cedricbouffard.github.io/covariablechamps/reference/visualiser_distances_bordures.md)
pour la visualisation

## Examples

``` r
if (FALSE) { # \dontrun{
library(sf)
library(terra)

# Charger un champ
champ <- st_read("mon_champ.shp")

# Mode raster: créer une grille sur le champ
dist_bordures <- calculer_distance_bordures_orientee(
  champ_poly = champ,
  resolution = 2,
  buffer = 100
)

# Visualiser les résultats
plot(dist_bordures$distance_long, main = "Distance sens long")
plot(dist_bordures$distance_large, main = "Distance sens large")

# Afficher l'orientation
cat("Angle principal:", dist_bordures$orientation$angle, "degrés\n")
cat("Dimensions:", dist_bordures$orientation$longueur, "x", 
    dist_bordures$orientation$largeur, "m\n")

# Mode points: calculer pour des positions spécifiques
points <- st_sample(champ, 100)
points <- st_as_sf(points)
dist_points <- calculer_distance_bordures_orientee(
  points_sf = points,
  champ_poly = champ
)

# Les distances sont dans le sf retourné
head(dist_points$points)
} # }
```

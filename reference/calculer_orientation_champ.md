# Calculer l'orientation principale d'un champ

Calcule l'orientation principale (azimut) d'un champ agricole à partir
de sa géométrie. Utilise la bounding box orientée ou les moments
d'inertie pour déterminer l'angle principal.

## Usage

``` r
calculer_orientation_champ(
  polygone,
  methode = "mbr",
  unite = "degres",
  orientation = "geographique"
)
```

## Arguments

- polygone:

  Un objet \`sf\` représentant le champ ou un chemin vers un fichier
  vectoriel

- methode:

  Méthode de calcul: "mbr" (Minimum Bounding Rectangle, défaut) ou "pca"
  (analyse en composantes principales)

- unite:

  Unité de sortie: "degres" (défaut) ou "radians"

- orientation:

  Référence d'orientation: "geographique" (0=Nord, 90=Est, défaut) ou
  "mathematique" (0=Est, 90=Nord)

## Value

Une liste contenant:

- angle:

  L'angle principal en degrés ou radians

- angle_perpendiculaire:

  L'angle perpendiculaire (±90°)

- longueur:

  La longueur selon l'axe principal

- largeur:

  La largeur selon l'axe perpendiculaire

- rapport_aspect:

  Le rapport longueur/largeur

- geometry:

  La géométrie de la bounding box orientée

## Examples

``` r
if (FALSE) { # \dontrun{
champ <- sf::st_read("champ.shp")
orientation <- calculer_orientation_champ(champ)
print(paste("Orientation principale:", round(orientation$angle, 1), "degrés"))

# Avec visualisation
plot(champ)
plot(orientation$geometry, add = TRUE, border = "red", lwd = 2)
} # }
```

# Calculer l'orientation d'un champ

Détermine l'orientation principale d'un polygone de champ selon la
méthode du Minimum Bounding Rectangle (MBR) ou de l'analyse en
composantes principales (PCA).

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

  Objet sf (polygone) ou chemin vers un fichier vectoriel

- methode:

  Méthode de calcul: "mbr" (défaut) ou "pca"

- unite:

  Unité de l'angle: "degres" (défaut) ou "radians"

- orientation:

  Référence d'orientation: "geographique" (0=Nord, 90=Est, défaut) ou
  "mathematique" (0=Est, 90=Nord)

## Value

Une liste contenant:

- angle: L'angle principal en degrés ou radians

- angle_perpendiculaire: L'angle perpendiculaire (±90°)

- longueur: La longueur selon l'axe principal

- largeur: La largeur selon l'axe perpendiculaire

- rapport_aspect: Le rapport longueur/largeur

- geometry: La géométrie de la bounding box orientée

## Details

Le polygone est projeté en UTM (zone détectée automatiquement) pour
garantir des angles conformes et des dimensions métriques. Si le
polygone n'a pas de CRS, les coordonnées sont traitées comme des
coordonnées planaires locales.

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

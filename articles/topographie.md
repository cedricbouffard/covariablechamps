# Analyse Topographique des Champs Agricoles

``` r
library(covariablechamps)
library(sf)
library(terra)
library(ggplot2)
```

## Introduction

L’analyse topographique est essentielle pour comprendre le relief d’un
champ agricole. Le package `covariablechamps` fournit plusieurs outils
pour analyser la topographie:

- **Pente**: Inclinaison du terrain en degrés
- **Aspect**: Orientation du terrain (Nord, Sud, Est, Ouest)
- **Géomorphons**: Classification des formes de terrain

Ces covariables influencent: - Le drainage des sols - L’exposition au
soleil et au vent - La sensibilité à l’érosion - Le choix des cultures

## Chargement du champ M2

Le package inclut un champ d’exemple (`M2`) situé au Québec.

``` r
champ <- st_read(system.file("extdata", "M2.shp", package = "covariablechamps"))
#> Reading layer `M2' from data source 
#>   `/home/runner/work/_temp/Library/covariablechamps/extdata/M2.shp' 
#>   using driver `ESRI Shapefile'
#> Simple feature collection with 1 feature and 65 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.06012 ymin: 46.64605 xmax: -71.05268 ymax: 46.65118
#> Geodetic CRS:  WGS 84

ggplot() +
  geom_sf(data = champ, fill = "lightgreen", alpha = 0.5) +
  theme_minimal() +
  labs(title = "Champ M2",
       subtitle = "Champ d'exemple inclus dans le package")
```

![](topographie_files/figure-html/charger-champ-1.png)

## Téléchargement du MNT depuis LiDAR

Le MNT (Modèle Numérique de Terrain) est téléchargé depuis les données
LiDAR.

**Note**: Cette opération nécessite des données LiDAR réelles.

``` r
# Télécharger le MNT depuis LiDAR
mnt <- telecharger_lidar(
  polygone = champ,
  dossier = "output/lidar",
  mne = FALSE  # FALSE = MNT (sans arbres)
)

# Visualiser le MNT
plot(mnt, main = "Modèle Numérique de Terrain (MNT)")
plot(sf::st_geometry(champ), add = TRUE, border = "black", lwd = 2)
```

## Calcul de la pente

La fonction
[`calculer_pente()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_pente.md)
calcule l’inclinaison du terrain en degrés.

``` r
pente <- calculer_pente(mnt)

plot(pente, main = "Pente (degrés)",
     col = hcl.colors(100, "Reds"))
plot(sf::st_geometry(champ), add = TRUE, border = "black", lwd = 2)
```

### Distribution des pentes

``` r
vals_pente <- terra::values(pente)
vals_pente <- vals_pente[!is.na(vals_pente)]

hist(vals_pente, breaks = 30, main = "Distribution des pentes",
     xlab = "Pente (degrés)", ylab = "Fréquence",
     col = "lightblue", border = "white")

cat("Statistiques de la pente:\n")
cat(sprintf("  Min: %.1f°\n", min(vals_pente)))
cat(sprintf("  Max: %.1f°\n", max(vals_pente)))
cat(sprintf("  Moyenne: %.1f°\n", mean(vals_pente)))
cat(sprintf("  Médiane: %.1f°\n", median(vals_pente)))
```

## Calcul de l’aspect (orientation)

La fonction
[`calculer_aspect()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_aspect.md)
calcule l’orientation du terrain.

``` r
aspect <- calculer_aspect(mnt)

plot(aspect, main = "Aspect (degrés - direction du Nord)",
     col = hcl.colors(100, "Cyclical"))
plot(sf::st_geometry(champ), add = TRUE, border = "black", lwd = 2)
```

### Interprétation de l’aspect

L’aspect est exprimé en degrés: - 0° ou 360° = Nord - 90° = Est - 180° =
Sud - 270° = Ouest

Les valeurs manquantes (NA) indiquent les zones plates.

## Classification en géomorphons

Les géomorphons classifient le terrain en 10 formes:

| Code | Nom        | Description                |
|------|------------|----------------------------|
| 1    | Plat       | Surface horizontale        |
| 2    | Pic        | Point culminant            |
| 3    | Crête      | Ligne de partage           |
| 4    | Épaulement | Transition crête-pente     |
| 5    | Éperon     | Protubérance sur une pente |
| 6    | Pente      | Surface inclinée           |
| 7    | Creux      | Dépression sur une pente   |
| 8    | Pied       | Base d’une pente           |
| 9    | Vallée     | Ligne de thalweg           |
| 10   | Fosse      | Point bas                  |

**Note**: Le calcul des géomorphons nécessite le package `rgeomorphon`.

## Extraction complète des covariables terrain

La fonction
[`extraire_covariables_terrain()`](https://cedricbouffard.github.io/covariablechamps/reference/extraire_covariables_terrain.md)
calcule toutes les covariables en une seule commande:

``` r
result <- extraire_covariables_terrain(
  polygone = champ,
  dossier = "output/terrain"
)

plot(result$mnt, main = "MNT")
plot(result$pente, main = "Pente")
plot(result$aspect, main = "Aspect")
```

## Workflow complet

``` r
# 1. Charger le package
library(covariablechamps)

# 2. Charger le champ M2
champ <- st_read(system.file("extdata", "M2.shp", package = "covariablechamps"))

# 3. Télécharger le MNT depuis LiDAR
mnt <- telecharger_lidar(champ)

# 4. Calculer la pente
pente <- calculer_pente(mnt)

# 5. Calculer l'aspect
aspect <- calculer_aspect(mnt)

# 6. Statistiques
cat("Pente moyenne:", mean(terra::values(pente), na.rm = TRUE), "degrés\n")

# 7. Sauvegarder
terra::writeRaster(pente, "pente.tif")
terra::writeRaster(aspect, "aspect.tif")
```

## Applications agricoles

### Drainage et pente

Les zones à forte pente sont généralement: - Mieux drainées (moins de
saturation) - Plus sensibles à l’érosion - Plus difficiles à cultiver

### Exposition et aspect

L’orientation du terrain affecte: - L’ensoleillement (sud = plus
chaud) - L’exposition aux vents dominants - Le gel tardif au printemps

### Planification

Combinez la topographie avec d’autres données pour: - Identifier les
zones à risque d’érosion - Optimiser le drainage - Planifier les
rotations de cultures

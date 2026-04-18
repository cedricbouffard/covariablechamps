# Analyse Pédologique et Séries de Sols

``` r
library(covariablechamps)
library(sf)
library(terra)
library(ggplot2)
library(dplyr)
```

## Introduction

L’analyse pédologique concerne l’étude des sols et leur distribution
spatiale. La connaissance des séries de sols est essentielle pour:

- Évaluer le drainage et la fertilité
- Planifier l’irrigation
- Choisir les cultures adaptées
- Gérer les risques d’érosion

Le package `covariablechamps` fournit la fonction
[`proba_et_classement_serie_quota_ilr()`](https://cedricbouffard.github.io/covariablechamps/reference/proba_et_classement_serie_quota_ilr.md)
pour désagéger les données pédologiques.

## Concepts clés

### Séries de sols

Une **série de sols** est une unité de classification basée sur: - La
texture du sol (proportions sable, limon, argile) - Le drainage - La
topographie - Le matériau parental

### Données requises

Pour utiliser
[`proba_et_classement_serie_quota_ilr()`](https://cedricbouffard.github.io/covariablechamps/reference/proba_et_classement_serie_quota_ilr.md),
vous avez besoin de:

1.  **Polygones des champs** avec les séries de sols identifiées (du
    gouvernement du Québec)
2.  **Table des séries** avec les compositions granulométriques
3.  **MNT** pour intégrer l’effet topographique

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

![](pedologie_files/figure-html/charger-champ-1.png)

## Workflow de désagrégation des sols

**Note**: Les opérations ci-dessous nécessitent des données pédologiques
réelles (polygones de séries de sols du gouvernement).

### Préparer les données

``` r
# 1. Charger les polygones de sols (source: gouvernement du Québec)
polygones_sols <- st_read("sols_poly.shp")

# 2. Table des séries avec composition granulométrique
table_series <- read.csv("table_sols.csv")

# 3. Vérifier la concordance
head(polygones_sols)
head(table_series)
```

### Table des séries de sols

La table des séries doit contenir:

| Colonne         | Description                             |
|-----------------|-----------------------------------------|
| `Code.polygone` | Identifiant reliant à polygones_sols    |
| `Composante`    | Nom de la série de sols                 |
| `Pourcentage`   | Proportion de la série dans le polygone |
| `Sable`         | Pourcentage de sable (0-100)            |
| `Limon`         | Pourcentage de limon (0-100)            |
| `Argile`        | Pourcentage d’argile (0-100)            |

Exemple:

``` r
table_series <- data.frame(
  Code.polygone = 1:3,
  Composante = c("Soulange", "Kamouraska", "Saint-André"),
  Pourcentage = c(60, 75, 45),
  Sable = c(30, 45, 55),
  Limon = c(45, 35, 30),
  Argile = c(25, 20, 15)
)
print(table_series)
```

## Triangle textural

Le triangle textural montre la classification des sols selon les
proportions de sable, limon et argile.

| Texture                 | Sable  | Limon  | Argile |
|-------------------------|--------|--------|--------|
| Argileuse               | 0-45   | 0-40   | 55-100 |
| Limoneuse               | 0-45   | 50-100 | 0-55   |
| Sableuse                | 50-100 | 0-50   | 0-50   |
| Franco-argilo-limoneuse | 20-45  | 30-50  | 30-45  |

## Calcul de la désagrégation

La fonction
[`proba_et_classement_serie_quota_ilr()`](https://cedricbouffard.github.io/covariablechamps/reference/proba_et_classement_serie_quota_ilr.md)
effectue la désagrégation:

``` r
# Charger le MNT (via LiDAR)
mnt <- telecharger_lidar(champ)

# Effectuer la désagrégation
resultat <- proba_et_classement_serie_quota_ilr(
  polygones_sf = polygones_sols,
  table_series = table_series,
  mnt = mnt,
  id_col = "Code.polygone",
  serie_col = "Composante",
  poids_col = "Pourcentage",
  sable_col = "Sable",
  limon_col = "Limon",
  argile_col = "Argile"
)

# Visualiser les probabilités
plot(resultat$prob)

# Visualiser la classification finale
plot(resultat$closest_label)
```

### Paramètres de la fonction

| Paramètre      | Description                            | Valeur par défaut |
|----------------|----------------------------------------|-------------------|
| `grid_m`       | Résolution de la grille (m)            | 20                |
| `sigma_voisin` | Écart-type du noyau gaussien (m)       | 60                |
| `alpha_voisin` | Pondération de similarité de voisinage | 8                 |
| `beta_mnt`     | Pondération de l’effet MNT             | 0.2               |
| `lambda_base`  | Pondération de la probabilité de base  | 0.25              |

## Interprétation des résultats

### Raster de probabilités

Chaque couche du raster `prob` représente la probabilité d’appartenance
à une série:

- Valeurs proches de 1 = forte probabilité
- Valeurs proches de 0 = faible probabilité

### Classification finale

Le raster `closest_label` assigne chaque pixel à la série la plus
probable.

## Applications agricoles

### drainage

| Série       | drainage         |
|-------------|------------------|
| Soulange    | Bon              |
| Kamouraska  | Modéré imparfait |
| Saint-André | Bon à modéré     |

### Aptitudes culturales

- Séries bien drainées: cultures sensibles au froid
- Séries à drainage modéré: pacage, forêts

### Gestion de l’eau

- Identifier les zones à risque de saturation
- Planifier les systèmes de drainage

## Références

- Commission de protection des terres agricoles du Québec (CPTAQ)
- Agence de développement du Canada (AAFC) - Bases de données des sols

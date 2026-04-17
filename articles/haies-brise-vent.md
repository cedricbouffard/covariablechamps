# Detection et analyse des haies brise-vent

``` r
library(covariablechamps)
library(sf)
library(terra)
library(ggplot2)
```

## Introduction

Les haies brise-vent jouent un rôle important dans la protection des
cultures contre le vent. Ce guide présente les fonctions du package
`covariablechamps` pour détecter, classifier et analyser les haies à
partir des données LiDAR.

## Données d’exemple

Pour cet article, nous créons des données synthétiques représentant un
champ avec des haies.

``` r
# Créer un champ
coords_champ <- matrix(c(0, 0, 300, 0, 300, 200, 0, 200, 0, 0), ncol = 2, byrow = TRUE)
champ <- sf::st_polygon(list(coords_champ))
champ <- sf::st_sfc(champ, crs = 32618)
champ <- sf::st_sf(geometry = champ)

# Créer des haies comme lignes droites
haie_ouest <- sf::st_sfc(
  sf::st_linestring(matrix(c(-5, 30, -5, 170), ncol = 2, byrow = TRUE)),
  crs = 32618
)
haie_interieure <- sf::st_sfc(
  sf::st_linestring(matrix(c(50, 100, 150, 100), ncol = 2, byrow = TRUE)),
  crs = 32618
)

# Créer un sf avec deux géométries
haies <- sf::st_sf(
  id = 1:2,
  geometry = c(haie_ouest, haie_interieure)
)

# Convertir les haies en POINTS pour les fonctions de distance
set.seed(42)
points_haie <- lapply(1:2, function(i) {
  g <- haies$geometry[[i]]
  coords <- sf::st_coordinates(g)
  n <- max(3, floor(sf::st_length(g) / 10))
  if (n > 1) {
    seq_idx <- seq(1, nrow(coords), length.out = n)
  } else {
    seq_idx <- 1
  }
  pts <- coords[seq_idx, , drop = FALSE]
  data.frame(
    id = rep(i, nrow(pts)),
    x = pts[, 1] + rnorm(nrow(pts), 0, 0.5),
    y = pts[, 2] + rnorm(nrow(pts), 0, 0.5)
  )
})
points_haie <- do.call(rbind, points_haie)

arbres_haies <- sf::st_sf(
  id = points_haie$id,
  geometry = sf::st_sfc(
    lapply(1:nrow(points_haie), function(i) {
      sf::st_point(c(points_haie$x[i], points_haie$y[i]))
    }),
    crs = 32618
  )
)

ggplot() +
  geom_sf(data = champ, fill = "lightgreen", alpha = 0.3, col = "darkgreen") +
  geom_sf(data = haies, col = "darkgreen", lwd = 2) +
  geom_sf(data = arbres_haies, col = "forestgreen", size = 2) +
  ggtitle("Champ avec haies brise-vent") +
  theme_minimal()
```

![](haies-brise-vent_files/figure-html/donnees-exemple-1.png)

## Étape 1: Calculer les zones de protection

Les haies brise-vent créent des zones de protection dont l’étendue
dépend de la hauteur de la haie.

``` r
# Direction du vent dominante (en degrés)
direction_vent <- 270  # Vent d'ouest

# Créer un raster de zones de vent à partir des distances
distances_haies <- calculer_distance_arbres(
  arbres_sf = arbres_haies,
  champ_bbox = champ,
  resolution = 5,
  buffer_arbre = 5,
  max_distance = 100
)

# Visualiser les zones de protection
visualiser_distance_arbres(distances_haies, type = "buffer") +
  ggtitle("Zones de protection contre le vent",
          subtitle = "Distance aux haies (plus sombre = plus protégé)")
```

![](haies-brise-vent_files/figure-html/zones-protection-1.png)

### Comprendre les zones de protection

La protection offerte par une haie diminue avec la distance:

- **Zone 1-2H**: Protection maximale (réduction du vent \> 50%)
- **Zone 2-5H**: Protection modérée (réduction 20-50%)
- **Zone 5-10H**: Protection faible (réduction \< 20%)

où H est la hauteur de la haie.

## Étape 2: Distance directionnelle (amont/aval)

``` r
dist_dir_haies <- calculer_distances_amont_aval(
  arbres_sf = arbres_haies,
  angle_vent = direction_vent,
  champ_bbox = champ,
  resolution = 5,
  buffer_arbre = 5,
  angle_focal = 30,
  max_distance = 100
)

viz <- visualiser_distances_vent(dist_dir_haies, type = "comparaison")
viz$amont +
  ggtitle("Zone AMONT (face au vent)",
          subtitle = "Vent vient de la haie vers le champ") +
  theme_minimal()
```

![](haies-brise-vent_files/figure-html/distances-amont-aval-1.png)

``` r

viz$aval +
  ggtitle("Zone AVAL (sous le vent)",
          subtitle = "Vent va de l'autre côté de la haie") +
  theme_minimal()
```

![](haies-brise-vent_files/figure-html/distances-amont-aval-2.png)

## Étape 3: Fetch de vent

``` r
fetch_haies <- calculer_fetch_vent(
  arbres_sf = arbres_haies,
  angle_vent = direction_vent,
  champ_bbox = champ,
  resolution = 5,
  max_fetch = 100,
  coef_ellipse = 3
)

viz_fetch <- visualiser_fetch(fetch_haies, type = "comparaison")
viz_fetch$simple +
  ggtitle("Fetch simple (cercle)") +
  theme_minimal()
```

![](haies-brise-vent_files/figure-html/fetch-haies-1.png)

``` r

viz_fetch$elliptique +
  ggtitle("Fetch elliptique (aligné vent)",
          subtitle = "Effet de voile plus important dans la direction du vent") +
  theme_minimal()
```

![](haies-brise-vent_files/figure-html/fetch-haies-2.png)

## Étape 4: Rasterisation pour analyse

Pour des analyses spatiales plus poussées, les zones peuvent être
rasterisées.

``` r
# La rasterisation utilise le résultat des distances
raster_dist <- distances_haies$distance_buffer
plot(raster_dist, main = "Raster des distances aux haies")
```

![](haies-brise-vent_files/figure-html/rasteriser-1.png)

### Gradient continu

Le gradient montre la diminution progressive de la protection:

``` r
# Créer un gradient de protection (1 = protection max, 0 = pas de protection)
raster_protection <- distances_haies$distance_buffer / distances_haies$max_distance
raster_protection <- 1 - raster_protection

# Visualiser
plot(raster_protection, main = "Gradient de protection",
     col = hcl.colors(100, "Blues"))
```

![](haies-brise-vent_files/figure-html/gradient-1.png)

## Étape 5: Simulation de la vitesse du vent

Estimez la réduction de vitesse du vent due aux haies.

``` r
# Avec les distances simples
vitesse_simple <- simuler_vitesse_vent_simple(
  dist_result = distances_haies,
  vitesse_ref = 5,
  coef_protection = 0.6
)

# Visualiser directement avec terra
plot(vitesse_simple$vitesse, main = "Vitesse du vent (m/s)",
     col = hcl.colors(100, "RdBu"))
```

![](haies-brise-vent_files/figure-html/simulation-vent-1.png)

## Étape 6: Intégration avec les données météo

Combinez avec les roses des vents pour une analyse complète.

``` r
# Obtenir la rose des vents (requiert connexion internet)
rose <- obtenir_rose_vents(
  lat = 46.8,
  lon = -71.2,
  date_debut = "2023-01-01",
  date_fin = "2023-12-31"
)

# Visualiser
tracer_rose_vents(rose)
```

**Note**: La fonction
[`obtenir_rose_vents()`](https://cedricbouffard.github.io/covariablechamps/reference/obtenir_rose_vents.md)
nécessite une connexion internet pour accéder aux données NASA POWER.

## Comparaison des méthodes de protection

``` r
# Préparer les données
distances_haies_vent <- calculer_distances_vent(
  arbres_sf = arbres_haies,
  angle_vent = direction_vent,
  champ_bbox = champ,
  resolution = 5,
  max_distance = 100,
  ouverture_angulaire = 45
)

# Visualiser les différentes couches
par(mfrow = c(2, 2), mar = c(3, 3, 3, 3))

plot(distances_haies$distance_buffer, main = "Distance simple",
     col = hcl.colors(100, "Terrain"))
plot(distances_haies_vent$distance_amont, main = "Amont",
     col = hcl.colors(100, "Blues"))
plot(distances_haies_vent$distance_aval, main = "Aval",
     col = hcl.colors(100, "Reds"))
plot(distances_haies_vent$distance_totale, main = "Totale",
     col = hcl.colors(100, "viridis"))
```

![](haies-brise-vent_files/figure-html/comparaison-1.png)

## Bonnes pratiques

1.  **Validation terrain**: Validez toujours les détections avec des
    observations terrain
2.  **Résolution**: Choisissez une résolution adaptée à vos objectifs
    (1-5m pour les haies)
3.  **Buffer**: Utilisez un buffer suffisant pour capturer les haies
    autour du champ
4.  **Saison**: Les données LiDAR de saison végétative donnent de
    meilleurs résultats
5.  **Angle focal**: Ajustez selon la perméabilité de la haie (plus
    serrée = angle plus faible)

## Workflow complet

``` r
library(covariablechamps)
library(sf)
library(terra)

# 1. Charger le champ
champ <- st_read("champ.gpkg")

# 2. Charger ou détecter les haies
haies <- st_read("haies.shp")

# 3. Définir la direction du vent (270 = ouest)
direction_vent <- 270

# 4. Calculer les distances
dist <- calculer_distances_amont_aval(
  arbres_sf = haies,
  angle_vent = direction_vent,
  champ_bbox = champ,
  buffer_arbre = 3
)

# 5. Visualiser
visualiser_distances_vent(dist, type = "comparaison")

# 6. Simuler la vitesse
vitesse <- simuler_vitesse_vent(
  result = dist,
  vitesse_ref = 5,
  coef_amont = 0.5,
  coef_aval = 0.3
)

# 7. Cartographier
tracer_carte_vent(distances = dist, type = "les_deux")
```

## Références

- Heisler, G.M., Dewalle, D.R. (1988). Effects of windbreak structure on
  wind flow. Agriculture, Ecosystems & Environment.
- Brandle, J.R., et al. (2004). Windbreaks in North American
  Agricultural Systems. Agroforestry Systems.

# Extraire et classifier les arbres LiDAR autour d'un champ

Pipeline complet: 1) DTM (rasterize_terrain) 2) Normalisation (Z - DTM)
3) CHM (rasterize_canopy) 4) Segmentation (lidaRtRee) 5) Extraction
arbres (lidaRtRee) 6) Clustering + classification: foret /
haie_brise_vent / individuel 7) Export des rectangles des haies
(largeur, longueur, angle)

## Usage

``` r
extraire_classifier_haies_lidar(
  nuage_points,
  res_dtm = 1,
  res_chm = 0.25,
  hmin = 1,
  eps_dbscan = 6,
  minPts_dbscan = 3,
  seuil_aspect = 6,
  seuil_largeur_max = 12,
  seuil_linearity = 8,
  seuil_largeur_foret = 20,
  seuil_n_foret = 30
)
```

## Arguments

- nuage_points:

  Objet LAS (lidR) - typiquement l\$nuage_points

- res_dtm:

  Resolution DTM (m)

- res_chm:

  Resolution CHM (m)

- hmin:

  Hauteur minimale (m) pour la segmentation / extraction

- eps_dbscan:

  Paramètre eps DBSCAN (m) pour regrouper les arbres en clusters

- minPts_dbscan:

  minPts DBSCAN

- seuil_aspect:

  Seuil L/W minimal pour haie (ex: 6)

- seuil_largeur_max:

  Largeur max (m) d'une haie (ex: 12)

- seuil_linearity:

  Linéarité PCA min pour haie (ex: 8)

- seuil_largeur_foret:

  Largeur (m) indicative de forêt (ex: 20)

- seuil_n_foret:

  Nombre d'arbres indicatif de forêt (ex: 30)

## Value

Une liste: - dtm: SpatRaster DTM - chm: SpatRaster CHM - segms:
segmentation (lidaRtRee) - trees: output brut
lidaRtRee::tree_extraction - trees_sf: sf points avec attributs +
cluster + classe - cluster_stats: table des métriques par cluster -
haies_rectangles: sf POLYGON avec les rectangles des haies (largeur,
longueur, angle)

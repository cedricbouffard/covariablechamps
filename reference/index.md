# Package index

## Telechargement LiDAR

Fonctions pour telecharger les donnees LiDAR depuis les services
gouvernementaux

- [`telecharger_lidar()`](https://cedricbouffard.github.io/covariablechamps/reference/telecharger_lidar.md)
  : Télécharger les données LiDAR pour une zone donnée
- [`telecharger_lidar_ponctuel()`](https://cedricbouffard.github.io/covariablechamps/reference/telecharger_lidar_ponctuel.md)
  : Télécharger les données LiDAR ponctuelles (COPC) pour une zone
- [`verifier_disponibilite_lidar()`](https://cedricbouffard.github.io/covariablechamps/reference/verifier_disponibilite_lidar.md)
  : Vérifier la disponibilité des données LiDAR
- [`vider_cache_lidar()`](https://cedricbouffard.github.io/covariablechamps/reference/vider_cache_lidar.md)
  : Vider le cache LiDAR

## Analyse du terrain

Fonctions pour calculer les covariables terrain a partir du MNT

- [`calculer_pente()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_pente.md)
  : Calculer la pente à partir d'un MNT
- [`calculer_aspect()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_aspect.md)
  : Calculer l'aspect (orientation) à partir d'un MNT
- [`calculer_geomorphons()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_geomorphons.md)
  : Calculer les géomorphons à partir d'un MNT
- [`extraire_covariables_terrain()`](https://cedricbouffard.github.io/covariablechamps/reference/extraire_covariables_terrain.md)
  : Extraire les covariables terrain complètes
- [`labels_geomorphons()`](https://cedricbouffard.github.io/covariablechamps/reference/labels_geomorphons.md)
  : Obtenir les labels des géomorphons

## Orientation et distances aux bordures

Fonctions pour analyser l’orientation des champs et calculer les
distances aux bordures

- [`calculer_orientation_champ()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_orientation_champ.md)
  : Calculer l'orientation principale d'un champ
- [`visualiser_orientation()`](https://cedricbouffard.github.io/covariablechamps/reference/visualiser_orientation.md)
  : Visualiser l'orientation d'un champ
- [`calculer_distance_bordures_orientee()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_distance_bordures_orientee.md)
  : Calculer la distance aux bordures selon l'orientation du champ
- [`classifier_distances_bordures()`](https://cedricbouffard.github.io/covariablechamps/reference/classifier_distances_bordures.md)
  : Classifier les distances aux bordures
- [`visualiser_distances_bordures()`](https://cedricbouffard.github.io/covariablechamps/reference/visualiser_distances_bordures.md)
  : Visualiser les distances aux bordures orientées
- [`visualiser_classes_bordures()`](https://cedricbouffard.github.io/covariablechamps/reference/visualiser_classes_bordures.md)
  : Visualiser les classes de distance aux bordures

## Detection des arbres et haies

Fonctions pour detecter, classifier et analyser les arbres et haies
brise-vent

- [`extraire_classifier_haies_lidar()`](https://cedricbouffard.github.io/covariablechamps/reference/extraire_classifier_haies_lidar.md)
  : Extraire et classifier les arbres LiDAR autour d'un champ
- [`extraire_ligne_centrale_haies()`](https://cedricbouffard.github.io/covariablechamps/reference/extraire_ligne_centrale_haies.md)
  : Extraire la ligne centrale d'un rectangle de haie
- [`calculer_zones_vent_spline()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_zones_vent_spline.md)
  : Calculer les zones d'influence du vent sur les haies avec une forme
  de demi-lune
- [`rasteriser_zones_gradient()`](https://cedricbouffard.github.io/covariablechamps/reference/rasteriser_zones_gradient.md)
  : Rasteriser les zones de vent avec le facteur H minimum par pixel
- [`rasteriser_zones_gradient_v2()`](https://cedricbouffard.github.io/covariablechamps/reference/rasteriser_zones_gradient_v2.md)
  : Rasteriser les zones de vent avec le facteur H minimum par pixel
  (version corrigée)

## Distance aux arbres

Fonctions pour calculer les distances aux arbres et l’effet du vent

- [`calculer_distance_arbres()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_distance_arbres.md)
  : Calculer la distance aux arbres avec buffer
- [`calculer_distances_amont_aval()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_distances_amont_aval.md)
  : Calculer les distances amont/aval avec buffer et lissage
- [`visualiser_distance_arbres()`](https://cedricbouffard.github.io/covariablechamps/reference/visualiser_distance_arbres.md)
  : Visualiser la distance aux arbres
- [`visualiser_distances_vent()`](https://cedricbouffard.github.io/covariablechamps/reference/visualiser_distances_vent.md)
  : Visualiser les distances amont/aval
- [`simuler_vitesse_vent_simple()`](https://cedricbouffard.github.io/covariablechamps/reference/simuler_vitesse_vent_simple.md)
  : Simuler la vitesse du vent basée sur la distance simple aux arbres
- [`simuler_vitesse_vent()`](https://cedricbouffard.github.io/covariablechamps/reference/simuler_vitesse_vent.md)
  : Simuler la vitesse du vent
- [`calculer_distances_vent()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_distances_vent.md)
  : Calculer les distances aux arbres selon la direction du vent
- [`calculer_fetch_vent()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_fetch_vent.md)
  : Créer une carte de fetch de vent avec effet directionnel
  (elliptique)
- [`visualiser_fetch()`](https://cedricbouffard.github.io/covariablechamps/reference/visualiser_fetch.md)
  : Visualiser le fetch de vent
- [`distance_arbres-package`](https://cedricbouffard.github.io/covariablechamps/reference/distance_arbres-package.md)
  : Récapitulatif des fonctions distance_arbres

## Visualisation du vent

Fonctions pour visualiser les cartes de vent

- [`tracer_carte_vent()`](https://cedricbouffard.github.io/covariablechamps/reference/tracer_carte_vent.md)
  : Visualiser les distances amont/aval avec les flèches de vent

## Calcul de l’ombrage

Fonctions pour calculer l’ombrage des parcelles selon la position du
soleil

- [`calculer_ombrage()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_ombrage.md)
  : Calculer l'ombrage d'une parcelle à partir du LiDAR et de la
  position du soleil
- [`calculer_ombrage_periode()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_ombrage_periode.md)
  : Calculer l'ombrage sur une periode de dates
- [`calculer_ombrage_visualisation()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_ombrage_visualisation.md)
  : Visualiser l'ombrage d'une parcelle

## Donnees meteorologiques

Fonctions pour obtenir et visualiser les donnees de vent

- [`obtenir_rose_vents()`](https://cedricbouffard.github.io/covariablechamps/reference/obtenir_rose_vents.md)
  : Obtenir la rose des vents depuis NASA POWER
- [`tracer_rose_vents()`](https://cedricbouffard.github.io/covariablechamps/reference/tracer_rose_vents.md)
  : Créer un graphique radar de la rose des vents
- [`tracer_rose_vents_stacked()`](https://cedricbouffard.github.io/covariablechamps/reference/tracer_rose_vents_stacked.md)
  : Créer une rose des vents complète avec barres empilées

## Pedologie et sols

Fonctions pour l’extraction et la desagregation des donnees pedologiques

- [`proba_et_classement_serie_quota_ilr()`](https://cedricbouffard.github.io/covariablechamps/reference/proba_et_classement_serie_quota_ilr.md)
  : Calculer les probabilités et le classement des séries avec quota ILR

## Extraction complete

Fonctions d’orchestration pour extraire toutes les covariables

- [`extraire_covariables_complet()`](https://cedricbouffard.github.io/covariablechamps/reference/extraire_covariables_complet.md)
  : Extraire l'ensemble des covariables pour un champ
- [`visualiser_covariables()`](https://cedricbouffard.github.io/covariablechamps/reference/visualiser_covariables.md)
  : Créer une visualisation synthétique des covariables extraites

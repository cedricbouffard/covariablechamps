# Changelog

## covariablechamps 0.1.0

### Nouvelles fonctionnalités

#### Module Terrain

- [`telecharger_lidar()`](https://cedricbouffard.github.io/covariablechamps/reference/telecharger_lidar.md):
  Télécharge les données LiDAR (MNT ou MNE) depuis le DataCube du Canada
  pour une zone d’intérêt donnée. Nouveau paramètre `toutes = TRUE` pour
  télécharger toutes les années disponibles dans une liste nommée par
  année.
- [`telecharger_lidar_ponctuel()`](https://cedricbouffard.github.io/covariablechamps/reference/telecharger_lidar_ponctuel.md):
  Nouveaux paramètres `annee` et `toutes_annees = TRUE` pour télécharger
  une année spécifique ou toutes les années disponibles (couche
  historique de Données Québec) dans une liste séparée par année.
- [`verifier_disponibilite_lidar()`](https://cedricbouffard.github.io/covariablechamps/reference/verifier_disponibilite_lidar.md):
  Vérifie les années de couverture LiDAR disponibles pour une zone.
- [`calculer_pente()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_pente.md):
  Calcule la pente en degrés à partir d’un MNT.
- [`calculer_aspect()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_aspect.md):
  Calcule l’aspect (orientation) en degrés à partir d’un MNT.
- [`calculer_geomorphons()`](https://cedricbouffard.github.io/covariablechamps/reference/calculer_geomorphons.md):
  Classifie les formes de terrain avec la méthode des géomorphons.
- [`extraire_covariables_terrain()`](https://cedricbouffard.github.io/covariablechamps/reference/extraire_covariables_terrain.md):
  Fonction principale qui extrait toutes les covariables terrain en une
  seule commande.
- [`labels_geomorphons()`](https://cedricbouffard.github.io/covariablechamps/reference/labels_geomorphons.md):
  Retourne les labels des 10 classes de géomorphons.

### Corrections de bogues

- [`telecharger_lidar()`](https://cedricbouffard.github.io/covariablechamps/reference/telecharger_lidar.md):
  Correction du paramètre `recent` qui était ignoré. Il est maintenant
  possible de télécharger les données les plus anciennes en spécifiant
  `recent = FALSE`.
- [`telecharger_lidar_ponctuel()`](https://cedricbouffard.github.io/covariablechamps/reference/telecharger_lidar_ponctuel.md):
  Correction de l’erreur de fusion des nuages de points lorsque des
  tuiles ont des attributs différents (RGB, extra bytes). Les attributs
  non communs sont maintenant retirés et les nuages sont reprojetés vers
  un CRS commun avant la fusion.
- CI/CD: Correction de la résolution des dépendances rlas/lidR archivées
  sur le CRAN.
- Documentation: Ajout des dépendances manquantes (`viridis`,
  `patchwork`) pour la génération du site.

### Documentation

- Vignette complète en français: “Extraction des covariables terrain”
- Documentation pkgdown configurée
- Tests unitaires pour toutes les fonctions

### Notes

Première version du package. Ce module Terrain constitue la base pour
les futurs modules (sol, orientation, arbres).

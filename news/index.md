# Changelog

## covariablechamps 0.1.0

### Nouvelles fonctionnalités

#### Module Terrain

- [`telecharger_lidar()`](https://cedricbouffard.github.io/covariablechamps/reference/telecharger_lidar.md):
  Télécharge les données LiDAR (MNT ou MNE) depuis le DataCube du Canada
  pour une zone d’intérêt donnée.
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

### Documentation

- Vignette complète en français: “Extraction des covariables terrain”
- Documentation pkgdown configurée
- Tests unitaires pour toutes les fonctions

### Notes

Première version du package. Ce module Terrain constitue la base pour
les futurs modules (sol, orientation, arbres).

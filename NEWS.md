# covariablechamps 0.1.0

## Nouvelles fonctionnalités

### Module Terrain

- `telecharger_lidar()`: Télécharge les données LiDAR (MNT ou MNE) depuis le DataCube du Canada pour une zone d'intérêt donnée.
- `verifier_disponibilite_lidar()`: Vérifie les années de couverture LiDAR disponibles pour une zone.
- `calculer_pente()`: Calcule la pente en degrés à partir d'un MNT.
- `calculer_aspect()`: Calcule l'aspect (orientation) en degrés à partir d'un MNT.
- `calculer_geomorphons()`: Classifie les formes de terrain avec la méthode des géomorphons.
- `extraire_covariables_terrain()`: Fonction principale qui extrait toutes les covariables terrain en une seule commande.
- `labels_geomorphons()`: Retourne les labels des 10 classes de géomorphons.

## Documentation

- Vignette complète en français: "Extraction des covariables terrain"
- Documentation pkgdown configurée
- Tests unitaires pour toutes les fonctions

## Notes

Première version du package. Ce module Terrain constitue la base pour les futurs modules (sol, orientation, arbres).

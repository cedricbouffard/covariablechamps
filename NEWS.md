# covariablechamps 0.1.0

## Nouvelles fonctionnalités

### Module Terrain

- `telecharger_lidar()`: Télécharge les données LiDAR (MNT ou MNE) depuis le DataCube du Canada pour une zone d'intérêt donnée. Nouveau paramètre `toutes = TRUE` pour télécharger toutes les années disponibles dans une liste nommée par année.
- `verifier_disponibilite_lidar()`: Vérifie les années de couverture LiDAR disponibles pour une zone.
- `calculer_pente()`: Calcule la pente en degrés à partir d'un MNT.
- `calculer_aspect()`: Calcule l'aspect (orientation) en degrés à partir d'un MNT.
- `calculer_geomorphons()`: Classifie les formes de terrain avec la méthode des géomorphons.
- `extraire_covariables_terrain()`: Fonction principale qui extrait toutes les covariables terrain en une seule commande.
- `labels_geomorphons()`: Retourne les labels des 10 classes de géomorphons.

## Corrections de bogues

- `telecharger_lidar()`: Correction du paramètre `recent` qui était ignoré. Il est maintenant possible de télécharger les données les plus anciennes en spécifiant `recent = FALSE`.
- `telecharger_lidar_ponctuel()`: Correction de l'erreur de fusion des nuages de points lorsque des tuiles ont des attributs différents (RGB, extra bytes). Les attributs non communs sont maintenant retirés avant la fusion.
- CI/CD: Correction de la résolution des dépendances rlas/lidR archivées sur le CRAN.
- Documentation: Ajout des dépendances manquantes (`viridis`, `patchwork`) pour la génération du site.

## Documentation

- Vignette complète en français: "Extraction des covariables terrain"
- Documentation pkgdown configurée
- Tests unitaires pour toutes les fonctions

## Notes

Première version du package. Ce module Terrain constitue la base pour les futurs modules (sol, orientation, arbres).

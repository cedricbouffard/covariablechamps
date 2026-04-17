# covariablechamps

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

## Overview

Le package `covariablechamps` permet d'extraire et de calculer des covariables pour des champs agricoles au Québec, à partir de diverses sources de données (LiDAR, pédologiques, etc.).

## Fonctionnalités

### Module Terrain (disponible)
- Téléchargement automatique des données LiDAR depuis le DataCube du Canada
- Calcul de la pente (degrés)
- Calcul de l'aspect (orientation)
- Classification des formes de terrain avec les géomorphons

### Module Ombrage (disponible)
- Calcul des **ombres projetées** (cast shadows) d'une parcelle à partir du MNE (Modèle Numérique de Surface) LiDAR
- Calcul de la position du soleil avec le package `suncalc`
- Lancer de rayons (raytracing) avec le package `rayshader` pour des ombres réalistes
- **Calcul sur une période** : moyenne, min, max ou somme des heures d'ensoleillement sur plusieurs jours/mois
- Paramètres ajustables :
  - `zscale` : facteur d'échelle vertical (ajuste la longueur des ombres)
  - `max_distance` : distance maximale de projection des ombres (défaut: 1000m)
  - `seuil_ensoleillement` : seuil pour compter les heures d'ensoleillement (défaut: 0.1)
- Production de cartes :
  - Ombrage par heure (0 = ombre complète, 1 = plein soleil)
  - Ombrage moyen sur la journée ou sur une période
  - Nombre d'heures d'ensoleillement par pixel (journalier ou moyenne sur période)

### Modules à venir
- Texture et classe de drainage des sols
- Orientation des champs
- Localisation et hauteur des arbres bordant les parcelles

## Installation

### Prérequis

Installez d'abord le package `rgeomorphon` depuis GitHub:

```r
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("brownag/rgeomorphon")
```

### Installation

```r
# Installer depuis GitHub
remotes::install_github("cedricbouffard/covariablechamps")
```

## Utilisation rapide

```r
library(covariablechamps)

# Charger votre champ
champ <- sf::st_read("chemin/vers/champ.shp")

# Extraire toutes les covariables terrain
covariables <- extraire_covariables_terrain(champ)

# Visualiser
plot(covariables$pente, main = "Pente")
plot(covariables$aspect, main = "Aspect")
plot(covariables$geomorphons, main = "Géomorphons")
```

### Calculer l'ombrage d'une parcelle

```r
library(covariablechamps)

# Charger votre champ
champ <- sf::st_read("chemin/vers/champ.shp")

# Calculer l'ombrage pour une date spécifique
resultats <- calculer_ombrage(
  polygone = champ,
  date = "2024-06-21",  # Solstice d'été
  intervalle_heures = 1,
  dossier = "output/ombrage"
)

# Ajuster la longueur des ombres avec zscale
# zscale > 1 : ombres plus longues (exagère la hauteur)
# zscale < 1 : ombres plus courtes (minimise la hauteur)
resultats_ajuste <- calculer_ombrage(
  polygone = champ,
  date = "2024-06-21",
  zscale = 1.5,           # Ombres 50% plus longues
  max_distance = 1000,    # Projection jusqu'à 1000m
  dossier = "output/ombrage"
)

# Si vous obtenez trop peu d'heures d'ensoleillement dans les zones ouvertes,
# baissez le seuil d'ensoleillement (défaut: 0.1)
resultats <- calculer_ombrage(
  polygone = champ,
  date = "2024-06-21",
  seuil_ensoleillement = 0.05,  # Plus permissif - compte plus d'heures
  max_distance = 1000
)

# Visualiser les résultats
plot(resultats$ombrage_moyen, main = "Ombrage moyen")
plot(resultats$heures_ensoleillement, main = "Heures d'ensoleillement")

# Ou utiliser la fonction de visualisation complète
calculer_ombrage_visualisation(resultats, titre = "Ombrage du 21 juin 2024")
```

### Calculer l'ombrage sur une période

```r
# Calculer sur un mois complet (moyenne des heures d'ensoleillement)
resultats_mois <- calculer_ombrage_periode(
  polygone = champ,
  date_debut = "2024-06-01",
  date_fin = "2024-06-30",
  intervalle_jours = 3  # Calcul tous les 3 jours pour accélérer
)

# Visualiser les statistiques sur la période
plot(resultats_mois$heures_ensoleillement_moyen, 
     main = "Heures moyennes d'ensoleillement/jour")
plot(resultats_mois$heures_ensoleillement_min, 
     main = "Heures minimales (pire cas)")
plot(resultats_mois$heures_ensoleillement_max, 
     main = "Heures maximales (meilleur cas)")
plot(resultats_mois$ombrage_moyen_periode, 
     main = "Ombrage moyen sur la période")

# Obtenir le total des heures sur toute la période
resultats_total <- calculer_ombrage_periode(
  polygone = champ,
  date_debut = "2024-06-01",
  date_fin = "2024-06-30",
  resume_type = "somme"  # Somme totale au lieu de moyenne
)
```

## Documentation

Consultez la [documentation complète](https://cedricbouffard.github.io/covariablechamps/) pour plus de détails sur:
- L'extraction des covariables terrain
- Les paramètres des géomorphons
- La gestion des données LiDAR

## Données

Les données LiDAR proviennent du [DataCube du gouvernement du Canada](https://datacube.services.geo.ca/) via l'API STAC de Ressources naturelles Canada.

## Licence

Ce package est sous licence MIT.

## Contribution

Les contributions sont les bienvenues ! Veuillez ouvrir une issue ou une pull request sur GitHub.

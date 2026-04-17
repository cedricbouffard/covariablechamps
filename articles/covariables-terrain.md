# Extraction des Covariables Terrain avec covariablechamps

## Introduction

Ce package permet d’extraire automatiquement des covariables terrain à
partir de données LiDAR pour des champs agricoles au Québec. Les
covariables extraites incluent:

- **Le Modèle Numérique de Terrain (MNT)**: Élévation du terrain
- **La pente**: Inclinaison du terrain en degrés
- **L’aspect**: Orientation du terrain (Nord, Sud, Est, Ouest)
- **Les géomorphons**: Classification des formes de terrain (crêtes,
  vallées, pentes, etc.)

Les données LiDAR proviennent du [DataCube du gouvernement du
Canada](https://datacube.services.geo.ca/) via l’API STAC de Ressources
naturelles Canada.

## Installation

### Prérequis

Avant d’installer ce package, assurez-vous d’avoir installé le package
`rgeomorphon` qui n’est pas sur CRAN:

``` r
# Installer rgeomorphon depuis GitHub
if (!requireNamespace("remotes", quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("brownag/rgeomorphon")
```

### Installation du package

``` r
# Installer depuis GitHub (une fois publié)
# remotes::install_github("votrenom/covariablechamps")

# Ou installer depuis la source
devtools::install()
```

## Utilisation de base

### Charger le package

``` r
library(covariablechamps)
library(sf)
library(terra)
```

### Charger un champ

Vous pouvez charger votre champ à partir d’un fichier shapefile ou
geopackage:

``` r
# À partir d'un fichier
champ <- st_read("chemin/vers/votre/champ.shp")

# Ou utiliser les données d'exemple fournies avec le package
champ_exemple <- st_read(system.file("extdata", "M2.shp", package = "covariablechamps"))
```

### Vérifier la disponibilité des données LiDAR

Avant de télécharger les données, vous pouvez vérifier quelles années
sont disponibles pour votre zone:

``` r
# Vérifier les années disponibles
disponibilite <- verifier_disponibilite_lidar(champ_exemple)
print(disponibilite)
```

### Extraction complète des covariables

Pour extraire toutes les covariables terrain en une seule commande:

``` r
# Extraire toutes les covariables
covariables <- extraire_covariables_terrain(
  polygone = champ_exemple,
  search_radius = 100,  # Rayon pour les géomorphons (en mètres)
  dossier = "output"    # Dossier de sortie (optionnel)
)

# Visualiser les résultats
plot(covariables$mnt, main = "MNT")
plot(covariables$pente, main = "Pente (degrés)")
plot(covariables$aspect, main = "Aspect (degrés)")
plot(covariables$geomorphons, main = "Géomorphons")
```

## Utilisation avancée

### Téléchargement séparé du LiDAR

Si vous souhaitez uniquement télécharger les données LiDAR sans calculer
les covariables:

``` r
# Télécharger uniquement le MNT
mnt <- telecharger_lidar(
  polygone = champ_exemple,
  dossier = "output/lidar",
  mne = FALSE,        # FALSE = MNT, TRUE = MNE
  recent = TRUE       # Prendre la donnée la plus récente
)

# Télécharger le MNE (Modèle Numérique d'Élévation)
mne <- telecharger_lidar(
  polygone = champ_exemple,
  dossier = "output/lidar",
  mne = TRUE
)
```

### Calcul individuel des covariables

Vous pouvez également calculer chaque covariable séparément:

``` r
# Supposons que vous avez déjà le MNT
mnt <- telecharger_lidar(champ_exemple)

# Calculer la pente
pente <- calculer_pente(mnt, filename = "output/pente.tif")

# Calculer l'aspect
aspect <- calculer_aspect(mnt, filename = "output/aspect.tif")

# Calculer les géomorphons
geomorphons <- calculer_geomorphons(
  mnt,
  search_radius = 100,      # Rayon de recherche en mètres
  threshold = 1,            # Seuil de tolérance
  flatness_threshold = 1,   # Seuil de platitude
  filename = "output/geomorphons.tif"
)
```

### Comprendre les géomorphons

Les géomorphons identifient 10 types de formes de terrain:

``` r
# Obtenir les labels
labels <- labels_geomorphons()
print(labels)
```

| Code | Nom           | Description                |
|------|---------------|----------------------------|
| 1    | Plat          | Surface horizontale        |
| 2    | Pic           | Point culminant            |
| 3    | Crête         | Ligne de partage des eaux  |
| 4    | Épaulement    | Transition crête-pente     |
| 5    | Éperon        | Protubérance sur une pente |
| 6    | Pente         | Surface inclinée           |
| 7    | Creux         | Dépression sur une pente   |
| 8    | Pied de pente | Base d’une pente           |
| 9    | Vallée        | Ligne de thalweg           |
| 10   | Fosse         | Point bas/local minimum    |

### Paramètres des géomorphons

Le rayon de recherche est un paramètre important:

- **Petit rayon** (\< 50m): Détecte les microformes du terrain
- **Rayon moyen** (50-200m): Détecte les formes à l’échelle du champ
  (recommandé)
- **Grand rayon** (\> 200m): Détecte les grandes formes du paysage

``` r
# Comparer différents rayons
covariables_50m <- extraire_covariables_terrain(champ_exemple, search_radius = 50)
covariables_200m <- extraire_covariables_terrain(champ_exemple, search_radius = 200)

# Visualiser la différence
par(mfrow = c(1, 2))
plot(covariables_50m$geomorphons, main = "Rayon: 50m")
plot(covariables_200m$geomorphons, main = "Rayon: 200m")
```

## Système de coordonnées

Par défaut, les données sont reprojetées en **EPSG:2949** (NAD83(CSRS) /
Québec Lambert), le système de coordonnées officiel du Québec. Vous
pouvez changer cela avec le paramètre `epsg`:

``` r
# Utiliser un autre système de coordonnées (ex: UTM zone 18N)
mnt_utm <- telecharger_lidar(champ_exemple, epsg = 32618)
```

## Gestion des erreurs

Le package gère plusieurs situations d’erreur courantes:

### Pas de données LiDAR disponibles

``` r
# Si aucune donnée n'est disponible, une warning est émise
# et NULL est retourné
resultat <- telecharger_lidar(champ_loin_quebec)
if (is.null(resultat)) {
  message("Pas de données disponibles pour cette zone")
}
```

### Polygone sans CRS

Si votre polygone n’a pas de système de coordonnées défini, WGS84
(EPSG:4326) est assigné automatiquement:

``` r
# Créer un polygone sans CRS
poly_sans_crs <- st_sf(geometry = st_sfc(st_point(c(-72, 45))))
st_crs(poly_sans_crs) <- NA

# Fonctionne quand même - WGS84 est assigné
mnt <- telecharger_lidar(poly_sans_crs)
```

## Performances et stockage

### Réutiliser les données téléchargées

Si vous spécifiez un dossier de sortie, les rasters sont sauvegardés et
peuvent être rechargés:

``` r
# Premier appel - télécharge et sauvegarde
covariables <- extraire_covariables_terrain(champ_exemple, dossier = "output")

# Plus tard - recharger sans retélécharger
mnt_charge <- rast("output/mnt_2020.tif")
pente_charge <- rast("output/pente.tif")
```

### Optimisation de la mémoire

Pour les grands champs, vous pouvez traiter par morceaux ou utiliser des
fichiers temporaires:

``` r
# Traiter sans garder tout en mémoire
telecharger_lidar(champ_exemple, dossier = "temp")
calculer_pente(rast("temp/mnt_2020.tif"), filename = "temp/pente.tif")
```

## Références

- Jasiewicz, J., & Stepinski, T. F. (2013). Geomorphons—A pattern
  recognition approach to classification and mapping of landforms.
  *Geomorphology*, 182, 147-156.
- [DataCube Canada](https://datacube.services.geo.ca/)
- [Ressources naturelles Canada -
  LiDAR](https://www.nrcan.gc.ca/science-and-data/science-and-research/earth-sciences/geomatics/canadas-elevation-data/llid-high-resolution-dem)

## Aide et support

Pour signaler un bug ou demander une fonctionnalité, veuillez ouvrir une
issue sur GitHub: <https://github.com/votrenom/covariablechamps/issues>

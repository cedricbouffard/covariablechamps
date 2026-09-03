# Télécharger les données LiDAR pour une zone donnée

Cette fonction interroge l'API STAC de RNCan pour télécharger les
données LiDAR (MNE ou MNT) correspondant à une géométrie fournie, puis
les recadre à cette zone d'intérêt. Par défaut, ne retient que la
couverture la plus récente et complète.

## Usage

``` r
telecharger_lidar(
  polygone,
  dossier = NULL,
  mne = FALSE,
  recent = TRUE,
  epsg = 4326,
  annee = NULL
)
```

## Arguments

- polygone:

  Un objet \`sf\` représentant la zone d'intérêt ou un chemin vers un
  fichier vectoriel (\`.shp\`, \`.gpkg\`, etc.)

- dossier:

  Dossier de sortie pour enregistrer les fichiers raster (optionnel)

- mne:

  Logique. Si TRUE, télécharge le MNE (modèle de surface). Sinon, le MNT
  (modèle de terrain).

- recent:

  Logique. Si TRUE (par défaut), conserve uniquement la version la plus
  récente. Ignoré si \`annee\` est fourni.

- epsg:

  Code EPSG pour la projection de sortie (défaut: 4326 - WGS84)

- annee:

  Optionnel. Année spécifique à télécharger (ex: 2018). Si fourni,
  ignore le paramètre \`recent\`.

## Value

Un objet \`SpatRaster\` contenant le MNT ou MNE recadré

## Examples

``` r
if (FALSE) { # \dontrun{
# Télécharger le MNT pour un champ
champ <- sf::st_read("champ.shp")
mnt <- telecharger_lidar(champ)

# Télécharger une année spécifique
mnt_2018 <- telecharger_lidar(champ, annee = 2018)

# Télécharger le MNE
mne <- telecharger_lidar(champ, mne = TRUE)
} # }
```

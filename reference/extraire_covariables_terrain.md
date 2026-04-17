# Extraire les covariables terrain complètes

Calcule et agrège toutes les covariables terrain (pente, aspect,
géomorphons) pour un champ donné à partir du MNT LiDAR.

## Usage

``` r
extraire_covariables_terrain(
  polygone,
  search_radius = 100,
  dossier = NULL,
  epsg = 4326,
  ...
)
```

## Arguments

- polygone:

  Un objet \`sf\` représentant la zone d'intérêt ou un chemin de fichier

- dossier:

  Dossier de sortie pour sauvegarder les rasters (optionnel)

- epsg:

  Code EPSG pour la projection de sortie (défaut: 4326)

- ...:

  Arguments supplémentaires passés à \`telecharger_lidar()\`

## Value

Une liste contenant les rasters MNT, pente, aspect et géomorphons (les
géomorphons sont calculés aux 3 échelles: petit, moyen, grand)

## Examples

``` r
if (FALSE) { # \dontrun{
# Extraire toutes les covariables terrain pour un champ
covariables <- extraire_covariables_terrain("champ.shp")

# Accéder aux résultats
plot(covariables$mnt)
plot(covariables$pente)
plot(covariables$aspect)
plot(covariables$geomorphons$petit)   # Échelle 50m
plot(covariables$geomorphons$moyen)   # Échelle 100m
plot(covariables$geomorphons$grand)   # Échelle 200m
} # }
```

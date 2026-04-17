# Extraire l'ensemble des covariables pour un champ

Cette méta-fonction orchestre l'extraction complète des covariables pour
un polygone donné: terrain, pédologie, arbres, ombrage et vent. Elle
retourne également un raster multi-bandes consolidé avec toutes les
covariables alignées sur le MNT.

## Usage

``` r
extraire_covariables_complet(
  polygone,
  buffer = 100,
  covariables = list(terrain = TRUE, pedologie = TRUE, lidar_ponctuel = TRUE, arbres =
    TRUE, ombrage = TRUE, vent_dominant = TRUE, distances_vent = TRUE, distance_arbres =
    TRUE),
  dossier = NULL,
  date_debut_ombrage = "2024-05-01",
  date_fin_ombrage = "2024-08-31",
  intervalle_jours_ombrage = 14,
  progress_bar = TRUE,
  ...
)
```

## Arguments

- polygone:

  Un objet \`sf\` représentant le champ/parcelle

- buffer:

  Distance du buffer en mètres (défaut: 100)

- covariables:

  Liste des covariables à calculer. Par défaut, toutes sont activées: -
  "terrain": MNT, pente, aspect, géomorphons - "pedologie": Extraction
  et désagrégation des sols - "lidar_ponctuel": Nuage de points LiDAR -
  "arbres": Détection et classification des arbres/haies - "ombrage":
  Calcul de l'ombrage (1er mai - 31 août) - "vent_dominant": Rose des
  vents et direction dominante - "distances_vent": Distances amont/aval
  (fetch de vent) - "distance_arbres": Distance euclidienne aux arbres

- dossier:

  Dossier de sortie pour sauvegarder les résultats (optionnel)

- date_debut_ombrage:

  Date de début pour le calcul d'ombrage (défaut: "2024-05-01")

- date_fin_ombrage:

  Date de fin pour le calcul d'ombrage (défaut: "2024-08-31")

- intervalle_jours_ombrage:

  Intervalle en jours pour l'ombrage (défaut: 14)

- progress_bar:

  Afficher une barre de progression (défaut: TRUE)

- ...:

  Arguments supplémentaires passés aux fonctions internes

## Value

Une liste structurée contenant tous les résultats:

- polygone:

  Le polygone d'origine

- polygone_buffer:

  Le polygone avec buffer

- terrain:

  Liste avec MNT, pente, aspect, géomorphons

- pedologie:

  Liste avec sols extraits et désagrégation

- lidar_ponctuel:

  Résultat du LiDAR ponctuel

- arbres:

  Arbres détectés et classifiés

- ombrage:

  Résultats du calcul d'ombrage

- vent:

  Rose des vents et direction dominante

- distances_vent:

  Distances amont/aval avec fetch de vent

- distance_arbres:

  Distance euclidienne aux arbres

- raster_consolide:

  Raster multi-bandes avec toutes les covariables

- noms_bandes:

  Noms des bandes du raster consolidé

## Examples

``` r
if (FALSE) { # \dontrun{
# Extraction complète
champ <- sf::st_read("mon_champ.shp")
resultats <- extraire_covariables_complet(champ, buffer = 100)

# Accéder au raster consolidé
raster <- resultats$raster_consolide
names(raster)  # Voir les noms des bandes

# Extraction partielle (sans ombrage)
resultats <- extraire_covariables_complet(
  champ,
  buffer = 100,
  covariables = list(terrain = TRUE, pedologie = TRUE, ombrage = FALSE)
)
} # }
```

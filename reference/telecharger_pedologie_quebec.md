# Télécharger et traiter les données pédologiques pour un champ

Cette fonction télécharge les polygones de couverture pédologique du
Québec, les filtre pour l'emprise du champ, et joint les données de
texture et de proportions de séries (PPS) incluses dans le package.

## Usage

``` r
telecharger_pedologie_quebec(
  champ,
  url_pedologie =
    "https://storage.googleapis.com/geoqc/Pedologie/couverture_pedologique.fgb",
  path_texture = system.file("data", "texture.rds", package = "covariablechamps"),
  path_pps = system.file("data", "couverture_pps.rds", package = "covariablechamps")
)
```

## Arguments

- champ:

  Un objet \`sf\` représentant l'emprise du champ.

- url_pedologie:

  URL du fichier FlatGeobuf de pédologie (défaut: geoqc).

- path_texture:

  Chemin vers le fichier RDS de texture (défaut: data interne).

- path_pps:

  Chemin vers le fichier RDS de PPS (défaut: data interne).

## Value

Une liste contenant:

- \`polygones\`: Un objet \`sf\` des polygones pédologiques intersectant
  le champ.

- \`table_series\`: Un \`data.frame\` formaté pour
  \`proba_et_classement_serie_quota_ilr()\`.

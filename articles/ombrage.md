# Calcul de l'ombrage des parcelles

## Introduction

L’ombrage projete par les arbres et les haies sur les parcelles
agricoles peut avoir un impact significatif sur les cultures. Ce guide
presente les fonctions du package `covariablechamps` pour calculer et
visualiser l’ombrage a partir des donnees LiDAR et de la position du
soleil.

## Principes de base

Le calcul de l’ombrage repose sur:

1.  **Le Modele Numerique d’Elevation (MNE)**: Inclut les arbres et
    structures
2.  **La position du soleil**: Azimut et elevation selon la date et
    l’heure
3.  **Le raycasting**: Projection des ombres selon l’angle du soleil

``` r
library(covariablechamps)
library(terra)
library(sf)
```

## Preparation des donnees

``` r
# Definir le polygone du champ
champ <- st_read("mon_champ.gpkg")

# Telecharger les donnees LiDAR (MNT et MNE)
lidar <- telecharger_lidar(champ)

# Le MNE (Modele Numerique d'Elevation) inclut les arbres
mne <- lidar$mne
mnt <- lidar$mnt
```

## Calcul simple de l’ombrage

Pour calculer l’ombrage a un instant donne:

``` r
# Calculer l'ombrage pour le 21 juin 2024 a midi
ombrage <- calculer_ombrage(
  mne = mne,
  date = "2024-06-21",
  heure = 12,
  latitude = 46.8,
  longitude = -71.2
)

# Visualiser
plot(ombrage)
```

### Parametres importants

- `date`: Date au format “YYYY-MM-DD”
- `heure`: Heure locale (0-24)
- `latitude`, `longitude`: Coordonnees du site
- `zscale`: Facteur d’echelle vertical (1 = metres)

## Calcul de l’ombrage sur une periode

Pour analyser l’ombrage au cours d’une journee:

``` r
# Calculer l'ombrage pour une journee complete
result <- calculer_ombrage_periode(
  mne = mne,
  date = "2024-06-21",      # Solstice d'ete
  pas_horaire = 2,           # Calcul toutes les 2 heures
  zscale = 1,                # Echelle verticale
  max_distance = 100         # Distance max d'ombre (m)
)
```

### Contenu du resultat

``` r
# Le resultat contient plusieurs couches
names(result)

# Ombrage moyen sur la journee (0 = toujours ombre, 1 = jamais ombre)
plot(result$ombrage_moyen, main = "Ombrage moyen")

# Heures d'ensoleillement (nombre d'heures en plein soleil)
plot(result$heures_ensoleillement, main = "Heures d'ensoleillement")

# Stack de toutes les heures
nlyr(result$ombrage_stack)  # Nombre de couches horaires
```

## Visualisation detaillee

Pour une visualisation plus elaboree:

``` r
# Visualisation avec parametres personnalises
viz <- calculer_ombrage_visualisation(
  mne = mne,
  date = "2024-06-21",
  heures = c(8, 12, 16),  # Matin, midi, apres-midi
  palette = "viridis"
)
```

## Analyse saisonniere

L’ombrage varie considerablement selon la saison.

``` r
# Comparer differentes dates
dates <- c(
  "2024-03-21",  # Equinoxe de printemps
  "2024-06-21",  # Solstice d'ete
  "2024-09-21",  # Equinoxe d'automne
  "2024-12-21"  # Solstice d'hiver
)

# Calculer l'ombrage pour chaque date
resultats <- lapply(dates, function(d) {
  calculer_ombrage_periode(
    mne = mne,
    date = d,
    pas_horaire = 2,
    max_distance = 100
  )
})

# Comparer les heures d'ensoleillement
par(mfrow = c(2, 2))
for (i in seq_along(dates)) {
  plot(resultats[[i]]$heures_ensoleillement, 
       main = dates[i])
}
```

## Integration avec l’analyse des haies

Combinez l’ombrage avec la detection des haies pour une analyse
complete.

``` r
# 1. Detecter les haies
haies <- extraire_classifier_haies_lidar(
  mne = mne,
  mnt = mnt,
  polygone_champ = champ
)

# 2. Calculer l'ombrage
ombrage <- calculer_ombrage_periode(
  mne = mne,
  date = "2024-06-21",
  pas_horaire = 1,
  max_distance = 50
)

# 3. Visualiser ensemble
plot(ombrage$heures_ensoleillement)
plot(st_geometry(haies), add = TRUE, col = "darkgreen")
plot(st_geometry(champ), add = TRUE, border = "black", lwd = 2)
```

## Calcul de l’impact sur les cultures

### Zone ombragee vs ensoleillee

``` r
# Seuil: moins de 6 heures = zone ombragee
seuil_heures <- 6
zones_ombragees <- ombrage$heures_ensoleillement < seuil_heures

# Calculer la superficie
superficie_ombragee <- sum(values(zones_ombragees), na.rm = TRUE) * 
                        prod(res(zones_ombragees)) / 10000  # en hectares

cat("Superficie ombragee (<", seuil_heures, "h):", 
    round(superficie_ombragee, 2), "ha\n")

# Visualiser les zones
plot(zones_ombragees, col = c("yellow", "gray"), 
     main = "Zones ombragees (< 6h d'ensoleillement)")
```

### Statistiques par zone

``` r
# Statistiques de l'ombrage
stats <- global(ombrage$heures_ensoleillement, c("mean", "min", "max", "sd"))
print(stats)

# Distribution des heures d'ensoleillement
hist(values(ombrage$heures_ensoleillement), 
     main = "Distribution des heures d'ensoleillement",
     xlab = "Heures", ylab = "Frequence",
     col = "skyblue")
```

## Parametres avances

### Ajuster le zscale

Le parametre `zscale` ajuste l’echelle verticale:

``` r
# Si le MNE est en metres et les XY en metres: zscale = 1
# Si le MNE est en pieds: zscale = 0.3048
# Pour exagerer les ombres: zscale > 1

ombrage_exagere <- calculer_ombrage_periode(
  mne = mne,
  date = "2024-06-21",
  zscale = 2,  # Exageration x2
  max_distance = 100
)
```

### Distance maximale d’ombre

La `max_distance` limite la propagation des ombres:

``` r
# Pour des arbres de 15m de haut:
# - A midi en ete: ombre ~5m
# - Matin/soir: ombre peut atteindre >30m
# Recommande: 2-3x la hauteur maximale des arbres

ombrage_court <- calculer_ombrage_periode(mne = mne, date = "2024-06-21",
                                          max_distance = 30)
ombrage_long <- calculer_ombrage_periode(mne = mne, date = "2024-06-21",
                                         max_distance = 100)
```

## Exemple complet

``` r
library(covariablechamps)
library(terra)
library(sf)

# 1. Charger le champ
champ <- st_read("champ.gpkg")

# 2. Telecharger LiDAR
lidar <- telecharger_lidar(champ)

# 3. Calculer l'ombrage au solstice d'ete
ombrage_ete <- calculer_ombrage_periode(
  mne = lidar$mne,
  date = "2024-06-21",
  pas_horaire = 2,
  zscale = 1,
  max_distance = 100
)

# 4. Calculer l'ombrage en automne
ombrage_automne <- calculer_ombrage_periode(
  mne = lidar$mne,
  date = "2024-09-21",
  pas_horaire = 2,
  zscale = 1,
  max_distance = 100
)

# 5. Comparer
par(mfrow = c(1, 2))
plot(ombrage_ete$heures_ensoleillement, 
     main = "Ete - 21 juin",
     col = terrain.colors(20))
plot(ombrage_automne$heures_ensoleillement, 
     main = "Automne - 21 sept",
     col = terrain.colors(20))

# 6. Calculer la difference
difference <- ombrage_ete$heures_ensoleillement - 
              ombrage_automne$heures_ensoleillement
plot(difference, main = "Difference (ete - automne)")
```

## Considerations pratiques

### Performance

- Le calcul d’ombrage est intensif en ressources
- Reduisez `max_distance` pour accelerer le calcul
- Augmentez `pas_horaire` pour moins de calculs

### Precision

- Utilisez des donnees LiDAR recentes
- Le MNE doit inclure la vegetation
- Verifiez que le CRS est en metres

### Limitations

- Ne tient pas compte de la diffusion atmospherique
- Suppose un soleil ponctuel (pas de penombre)
- Les arbres a feuilles caduques: adapter selon la saison

## References

- Corripio, J.G. (2003). Vectorial algebra algorithms for calculating
  terrain parameters from DEMs and solar radiation modelling in
  mountainous terrain. International Journal of Geographical Information
  Science.
- Fu, P., Rich, P.M. (2002). A geometric solar radiation model with
  applications in agriculture and forestry. Computers and Electronics in
  Agriculture.

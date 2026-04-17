# Calculer l'ombrage sur une periode de dates

Cette fonction calcule l'ombrage et les heures d'ensoleillement moyennes
sur une periode donnee (ex: un mois, une saison). Elle agrege les
resultats journaliers pour donner des statistiques sur la periode.

## Usage

``` r
calculer_ombrage_periode(
  polygone,
  date_debut,
  date_fin,
  intervalle_jours = 1,
  intervalle_heures = 1,
  dossier = NULL,
  mne = NULL,
  tz = "America/Toronto",
  zscale = 1,
  max_distance = 1000,
  lambert = TRUE,
  seuil_ensoleillement = 0.1,
  resume_type = "moyenne"
)
```

## Arguments

- polygone:

  Un objet \`sf\` representant la zone d'interet ou un chemin vers un
  fichier vectoriel

- date_debut:

  Date de debut (format Date ou chaine "YYYY-MM-DD")

- date_fin:

  Date de fin (format Date ou chaine "YYYY-MM-DD")

- intervalle_jours:

  Intervalle en jours entre chaque calcul (defaut: 1 = tous les jours)

- intervalle_heures:

  Intervalle en heures entre chaque calcul journalier (defaut: 1)

- dossier:

  Dossier de sortie pour sauvegarder les resultats (optionnel)

- mne:

  Objet SpatRaster optionnel contenant le MNE deja charge

- tz:

  Fuseau horaire (defaut: "America/Toronto")

- zscale:

  Facteur d'echelle pour la hauteur (defaut: 1)

- max_distance:

  Distance maximale de projection des ombres en metres (defaut: 1000)

- lambert:

  Si TRUE, applique l'ombrage de Lambert (defaut: TRUE)

- seuil_ensoleillement:

  Seuil pour considerer un pixel comme ensoleille (defaut: 0.1)

- resume_type:

  Type de resume pour les heures d'ensoleillement : "moyenne" (defaut),
  "min", "max", "somme"

## Value

Une liste contenant :

- heures_ensoleillement_moyen:

  SpatRaster avec les heures d'ensoleillement moyennes par jour

- heures_ensoleillement_min:

  SpatRaster avec les heures minimales sur la periode

- heures_ensoleillement_max:

  SpatRaster avec les heures maximales sur la periode

- ombrage_moyen_periode:

  SpatRaster avec l'ombrage moyen sur toute la periode

- nb_jours_calcules:

  Nombre de jours effectivement calcules

- dates_calculees:

  Vecteur des dates calculees

- info_soleil:

  Data.frame avec les informations sur les positions du soleil

- mne:

  Le MNE utilise pour les calculs

## Details

Cette fonction calcule les statistiques d'ensoleillement sur une periode
en : 1. Calculant l'ombrage pour chaque jour echantillonne dans la
periode 2. Agregeant les heures d'ensoleillement journalieres

Le parametre \`resume_type\` determine comment agreger les heures : -
"moyenne" : heures moyennes par jour (defaut) - "min" : heures minimales
(pire cas) - "max" : heures maximales (meilleur cas) - "somme" : total
des heures sur toute la periode

## Examples

``` r
if (FALSE) { # \dontrun{
# Calculer sur un mois complet
champ <- sf::st_read("champ.shp")
resultats <- calculer_ombrage_periode(
  polygone = champ,
  date_debut = "2024-06-01",
  date_fin = "2024-06-30"
)

# Calculer sur une saison avec un point tous les 3 jours
resultats_saison <- calculer_ombrage_periode(
  polygone = champ,
  date_debut = "2024-06-01",
  date_fin = "2024-08-31",
  intervalle_jours = 3
)

# Visualiser les resultats
terra::plot(resultats$heures_ensoleillement_moyen, 
            main = "Heures moyennes d'ensoleillement/jour")
terra::plot(resultats$heures_ensoleillement_min, 
            main = "Heures minimales d'ensoleillement (pire cas)")
} # }
```

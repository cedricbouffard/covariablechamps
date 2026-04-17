#' Exemple d'utilisation de la fonction calculer_ombrage avec rayshader
#'
#' Ce script montre comment calculer les ombres projetées d'une parcelle
#' à partir du LiDAR (MNE) et de la position du soleil en utilisant le 
#' package rayshader pour un lancer de rayons réaliste.

library(covariablechamps)
library(sf)
library(terra)

# =============================================================================
# EXEMPLE 1 : Calculer l'ombrage pour aujourd'hui
# =============================================================================

# Charger un fichier shapefile de champ
champ <- sf::st_read("inst/extdata/M2.shp")

# Calculer l'ombrage pour aujourd'hui
resultats <- calculer_ombrage(champ)

# Visualiser les résultats
par(mfrow = c(2, 2))
plot(resultats$mne, main = "MNE (Modèle Numérique de Surface)")
plot(resultats$ombrage_moyen, main = "Ombrage moyen")
plot(resultats$heures_ensoleillement, main = "Heures d'ensoleillement")

# Afficher les informations sur le soleil
print(resultats$info_soleil)

# =============================================================================
# EXEMPLE 2 : Ajuster l'échelle verticale (zscale)
# =============================================================================

# Si les ombres semblent trop courtes, augmenter zscale
resultats_zscale2 <- calculer_ombrage(
  polygone = champ,
  date = "2024-06-21",
  zscale = 2,  # Exagère la hauteur pour des ombres plus longues
  max_distance = 1000  # Augmente la distance de projection des ombres
)

# Si les ombres semblent trop longues, diminuer zscale
resultats_zscale05 <- calculer_ombrage(
  polygone = champ,
  date = "2024-06-21",
  zscale = 0.5,  # Réduit la hauteur pour des ombres plus courtes
  max_distance = 300
)

# Comparer les résultats
par(mfrow = c(1, 3))
plot(resultats$ombrage_moyen, main = "zscale = 1 (défaut)")
plot(resultats_zscale2$ombrage_moyen, main = "zscale = 2 (ombres longues)")
plot(resultats_zscale05$ombrage_moyen, main = "zscale = 0.5 (ombres courtes)")

# =============================================================================
# EXEMPLE 3 : Calculer l'ombrage pour différentes saisons
# =============================================================================

# Solstice d'été (21 juin) - Soleil haut, ombres courtes
resultats_ete <- calculer_ombrage(
  polygone = champ,
  date = "2024-06-21",
  intervalle_heures = 1,
  dossier = "output/ombrage_ete",
  zscale = 1.5,
  max_distance = 500
)

# Solstice d'hiver (21 décembre) - Soleil bas, ombres longues
resultats_hiver <- calculer_ombrage(
  polygone = champ,
  date = "2024-12-21",
  intervalle_heures = 1,
  dossier = "output/ombrage_hiver",
  zscale = 1,
  max_distance = 1000  # Ombres plus longues en hiver
)

# Comparer les deux saisons
par(mfrow = c(2, 2))
plot(resultats_ete$ombrage_moyen, main = "Ombrage moyen - Été")
plot(resultats_ete$heures_ensoleillement, main = "Heures d'ensoleillement - Été")
plot(resultats_hiver$ombrage_moyen, main = "Ombrage moyen - Hiver")
plot(resultats_hiver$heures_ensoleillement, main = "Heures d'ensoleillement - Hiver")

# =============================================================================
# EXEMPLE 3b : Ajuster le seuil d'ensoleillement
# =============================================================================

# Si vous obtenez moins d'heures d'ensoleillement que prévu dans les zones ouvertes,
# vous pouvez baisser le seuil pour être plus permissif

# Seuil par défaut (0.1) - plus permissif, compte plus d'heures
resultats_permissif <- calculer_ombrage(
  polygone = champ,
  date = "2024-06-21",
  intervalle_heures = 1,
  seuil_ensoleillement = 0.1,  # Défaut : compte les pixels avec > 10% de lumière
  max_distance = 1000
)

# Seuil strict (0.3) - plus restrictif, compte moins d'heures  
resultats_strict <- calculer_ombrage(
  polygone = champ,
  date = "2024-06-21",
  intervalle_heures = 1,
  seuil_ensoleillement = 0.3,  # Compte seulement les pixels avec > 30% de lumière
  max_distance = 1000
)

# Comparer
par(mfrow = c(1, 2))
plot(resultats_permissif$heures_ensoleillement, 
     main = paste("Seuil 0.1 - Max:", round(max(values(resultats_permissif$heures_ensoleillement), na.rm = TRUE), 1), "h"))
plot(resultats_strict$heures_ensoleillement, 
     main = paste("Seuil 0.3 - Max:", round(max(values(resultats_strict$heures_ensoleillement), na.rm = TRUE), 1), "h"))

# =============================================================================
# EXEMPLE 4 : Comparer hillshade vs ombres projetées
# =============================================================================

# Utiliser un MNE déjà chargé
mne_existant <- resultats_ete$mne

# Calculer avec différents paramètres de distance
resultats_500m <- calculer_ombrage(
  polygone = champ,
  date = "2024-06-21",
  mne = mne_existant,
  max_distance = 500,  # Ombres projetées jusqu'à 500m
  zscale = 1
)

resultats_1000m <- calculer_ombrage(
  polygone = champ,
  date = "2024-06-21",
  mne = mne_existant,
  max_distance = 1000,  # Ombres projetées jusqu'à 1000m
  zscale = 1
)

# Comparer
par(mfrow = c(1, 2))
plot(resultats_500m$ombrage_moyen, main = "Distance max: 500m")
plot(resultats_1000m$ombrage_moyen, main = "Distance max: 1000m")

# =============================================================================
# EXEMPLE 5 : Visualisation avancée
# =============================================================================

# Utiliser la fonction de visualisation
resultats <- calculer_ombrage(champ, date = "2024-06-21", zscale = 1.2)
calculer_ombrage_visualisation(resultats, titre = "Ombrage du solstice d'été")

# Afficher une couche horaire spécifique (ex: midi)
plot(resultats$ombrage_par_heure[[6]], 
     main = paste("Ombrage à", names(resultats$ombrage_par_heure)[6]))

# =============================================================================
# EXEMPLE 6 : Analyse des résultats
# =============================================================================

# Statistiques sur l'ombrage moyen
moyenne_ombrage <- mean(terra::values(resultats$ombrage_moyen), na.rm = TRUE)
message("Ombrage moyen sur la parcelle: ", round(moyenne_ombrage * 100, 1), "%")

# Statistiques sur les heures d'ensoleillement
moyenne_heures <- mean(terra::values(resultats$heures_ensoleillement), na.rm = TRUE)
max_heures <- max(terra::values(resultats$heures_ensoleillement), na.rm = TRUE)
min_heures <- min(terra::values(resultats$heures_ensoleillement), na.rm = TRUE)

message("Moyenne d'heures d'ensoleillement: ", round(moyenne_heures, 1), " heures")
message("Maximum d'heures d'ensoleillement: ", round(max_heures, 1), " heures")
message("Minimum d'heures d'ensoleillement: ", round(min_heures, 1), " heures")

# Identifier les zones les plus ombragées
zone_plus_ombragee <- which(terra::values(resultats$ombrage_moyen) == 
                             min(terra::values(resultats$ombrage_moyen), na.rm = TRUE))
message("Nombre de pixels les plus ombragés: ", length(zone_plus_ombragee))

# Afficher les paramètres utilisés
message("\nParamètres utilisés:")
message("- zscale: ", resultats$zscale)
message("- max_distance: ", resultats$max_distance, "m")

# =============================================================================
# NOTES IMPORTANTES SUR LES PARAMÈTRES
# =============================================================================

# 1. zscale (Facteur d'échelle verticale):
#    - zscale = 1 : échelle réelle (pas de modification)
#    - zscale > 1 : exagère la hauteur → ombres plus LONGUES
#    - zscale < 1 : minimise la hauteur → ombres plus COURTES
#    - Valeurs typiques : 0.5 à 3

# 2. max_distance (Distance maximale de projection):
#    - Distance en mètres jusqu'où les ombres sont calculées
#    - Valeur plus élevée = calcul plus long mais ombres plus complètes
#    - Valeurs typiques : 300m à 2000m
#    - Pour les arbres hauts (>30m), utilisez au moins 1000m

# 3. lambert (Ombrage de Lambert):
#    - TRUE (défaut) : applique l'ombrage de Lambert pour plus de réalisme
#    - FALSE : ombres seulement basées sur le raytracing

# 4. Le MNE (Modèle Numérique de Surface) inclut:
#    - Le terrain (élévation du sol)
#    - Les arbres, bâtiments, et autres structures
#    - C'est pourquoi on utilise le MNE et non le MNT pour les ombres

# 5. rayshader::ray_shade() effectue un lancer de rayons qui:
#    - Calcule quels pixels sont visibles depuis le soleil
#    - Prend en compte les élévations des pixels voisins
#    - Produit des ombres projetées réalistes

# =============================================================================
# EXEMPLE 7 : Calcul sur une période
# =============================================================================

# Calculer sur un mois complet avec moyenne par jour
resultats_mois <- calculer_ombrage_periode(
  polygone = champ,
  date_debut = "2024-06-01",
  date_fin = "2024-06-30",
  intervalle_jours = 3,  # Calcul tous les 3 jours pour accélérer
  max_distance = 1000
)

# Visualiser les statistiques sur la période
par(mfrow = c(2, 2))
plot(resultats_mois$heures_ensoleillement_moyen, 
     main = "Heures moyennes/jour")
plot(resultats_mois$heures_ensoleillement_min, 
     main = "Heures min (pire cas)")
plot(resultats_mois$heures_ensoleillement_max, 
     main = "Heures max (meilleur cas)")
plot(resultats_mois$ombrage_moyen_periode, 
     main = "Ombrage moyen période")

# Afficher les statistiques
cat("Nombre de jours calculés:", resultats_mois$nb_jours_calcules, "\n")
cat("Type de résumé:", resultats_mois$resume_type, "\n")
cat("Dates:", format(resultats_mois$periode$debut, "%d/%m/%Y"), 
    "au", format(resultats_mois$periode$fin, "%d/%m/%Y"), "\n")

# =============================================================================
# EXEMPLE 8 : Différents types de résumé sur une période
# =============================================================================

# Moyenne (par défaut) - heures moyennes par jour
resultats_moyenne <- calculer_ombrage_periode(
  polygone = champ,
  date_debut = "2024-06-01",
  date_fin = "2024-06-07",
  resume_type = "moyenne",
  intervalle_jours = 1
)

# Minimum - heures minimales sur la période (pire cas)
resultats_min <- calculer_ombrage_periode(
  polygone = champ,
  date_debut = "2024-06-01",
  date_fin = "2024-06-07",
  resume_type = "min",
  intervalle_jours = 1
)

# Maximum - heures maximales sur la période (meilleur cas)
resultats_max <- calculer_ombrage_periode(
  polygone = champ,
  date_debut = "2024-06-01",
  date_fin = "2024-06-07",
  resume_type = "max",
  intervalle_jours = 1
)

# Somme - total des heures sur toute la période
resultats_total <- calculer_ombrage_periode(
  polygone = champ,
  date_debut = "2024-06-01",
  date_fin = "2024-06-07",
  resume_type = "somme",
  intervalle_jours = 1
)

# Comparer les différents types de résumé
par(mfrow = c(2, 2))
plot(resultats_moyenne$heures_ensoleillement_resume, main = "Moyenne (heures/jour)")
plot(resultats_min$heures_ensoleillement_resume, main = "Minimum (pire cas)")
plot(resultats_max$heures_ensoleillement_resume, main = "Maximum (meilleur cas)")
plot(resultats_total$heures_ensoleillement_resume, main = "Somme (total période)")

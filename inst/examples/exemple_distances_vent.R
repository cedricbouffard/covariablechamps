# Exemple complet: Carte des distances amont/aval selon le vent
#
# Ce script montre comment utiliser les nouvelles fonctions pour:
# 1. Extraire les arbres d'un nuage LiDAR
# 2. Obtenir la direction du vent dominant
# 3. Calculer les distances amont et aval aux arbres
# 4. Créer une carte avec les flèches de vent

# Charger les packages nécessaires
library(covariablechamps)
library(ggplot2)
library(sf)
library(terra)

# ============================================================================
# ÉTAPE 1: Extraire les arbres depuis le LiDAR
# ============================================================================

# Chemin vers votre fichier LAS
fichier_las <- "chemin/vers/votre/fichier.las"

# Lire le nuage de points
message("Chargement du nuage de points...")
las <- lidR::readLAS(fichier_las)

# Extraire et classifier les arbres
message("Extraction des arbres...")
resultats_arbres <- extraire_classifier_haies_lidar(
  nuage_points = las,
  res_dtm = 1,           # Résolution DTM en mètres
  res_chm = 0.25,        # Résolution CHM en mètres
  hmin = 2,              # Hauteur minimale des arbres
  eps_dbscan = 6,        # Distance pour le clustering
  minPts_dbscan = 3,     # Points minimum pour un cluster
  seuil_aspect = 6,      # Seuil d'aspect pour les haies
  seuil_largeur_max = 12 # Largeur maximale d'une haie
)

# Récupérer les arbres en format sf
arbres_sf <- resultats_arbres$trees_sf

message(paste(nrow(arbres_sf), "arbres extraits"))

# ============================================================================
# ÉTAPE 2: Obtenir la direction du vent dominant
# ============================================================================

# Définir le contour du champ (à adapter selon vos données)
# Option 1: À partir d'un fichier shapefile
champ_sf <- sf::st_read("chemin/vers/champ.shp")

# Option 2: Définir manuellement un rectangle
# coords <- matrix(c(
#   1000, 2000,  # Coin inférieur gauche
#   1100, 2000,  # Coin inférieur droit
#   1100, 2100,  # Coin supérieur droit
#   1000, 2100,  # Coin supérieur gauche
#   1000, 2000   # Retour au début
# ), ncol = 2, byrow = TRUE)
# champ_sf <- sf::st_polygon(list(coords))
# champ_sf <- sf::st_sfc(champ_sf, crs = 2154)  # CRS Lambert 93

# Obtenir les données de vent depuis NASA POWER
message("Récupération des données de vent...")
rose_vents <- obtenir_rose_vents(
  polygone = champ_sf,
  date_debut = "20230101",
  date_fin = "20231231",
  hauteur = "10m"
)

# Déterminer la direction du vent dominant
direction_dominante <- rose_vents$directions[which.max(rose_vents$wd_pct)]
message(paste("Direction du vent dominant:", direction_dominante, "degrés"))

# Visualiser la rose des vents
print(tracer_rose_vents(rose_vents))

# ============================================================================
# ÉTAPE 3: Calculer les distances amont et aval
# ============================================================================

message("Calcul des distances amont/aval...")

distances <- calculer_distances_vent(
  arbres_sf = arbres_sf,
  angle_vent = direction_dominante,
  champ_bbox = champ_sf,
  resolution = 2,           # Résolution du raster en mètres
  max_distance = 100,       # Distance maximale à calculer
  ouverture_angulaire = 45  # Angle d'ouverture des secteurs (±45°)
)

# Examiner les résultats
summary(distances$distance_amont[])
summary(distances$distance_aval[])

# ============================================================================
# ÉTAPE 4: Créer les cartes de visualisation
# ============================================================================

# 4.1 Carte simple avec les distances amont
message("Création des cartes...")

carte_amont <- tracer_carte_vent(
  distances = distances,
  champ = champ_sf,
  arbres = arbres_sf,
  type = "amont",
  titre = "Distance aux arbres en amont (contre le vent)",
  afficher_fleches = TRUE,
  afficher_arbres = TRUE,
  n_fleches = 5
)
print(carte_amont)

# 4.2 Carte simple avec les distances aval
carte_aval <- tracer_carte_vent(
  distances = distances,
  champ = champ_sf,
  arbres = arbres_sf,
  type = "aval",
  titre = "Distance aux arbres en aval (sous le vent)",
  afficher_fleches = TRUE,
  afficher_arbres = TRUE
)
print(carte_aval)

# 4.3 Carte combinée amont/aval (valeurs positives et négatives)
carte_combinee <- tracer_carte_vent(
  distances = distances,
  champ = champ_sf,
  arbres = arbres_sf,
  type = "les_deux",
  titre = "Distances amont (-) et aval (+) aux arbres",
  afficher_fleches = TRUE
)
print(carte_combinee)

# 4.4 Vue côte à côte (nécessite le package patchwork)
if (requireNamespace("patchwork", quietly = TRUE)) {
  carte_double <- tracer_carte_vent_double(
    distances = distances,
    champ = champ_sf,
    arbres = arbres_sf,
    titre = "Distances aux arbres selon la direction du vent"
  )
  print(carte_double)
}

# ============================================================================
# ÉTAPE 5: Sauvegarder les résultats
# ============================================================================

# Sauvegarder les rasters
message("Sauvegarde des rasters...")

terra::writeRaster(distances$distance_amont,
                   "distance_amont.tif",
                   overwrite = TRUE)

terra::writeRaster(distances$distance_aval,
                   "distance_aval.tif",
                   overwrite = TRUE)

terra::writeRaster(distances$distance_totale,
                   "distance_totale.tif",
                   overwrite = TRUE)

# Sauvegarder les cartes en PNG
ggsave("carte_amont.png", carte_amont, width = 10, height = 8, dpi = 300)
ggsave("carte_aval.png", carte_aval, width = 10, height = 8, dpi = 300)
ggsave("carte_combinee.png", carte_combinee, width = 12, height = 8, dpi = 300)

message("Exemple terminé!")

# ============================================================================
# NOTES SUR LA MÉTHODOLOGIE
# ============================================================================
#
# 1. CALCUL DES DISTANCES:
#    - Un raster de résolution définie est créé autour du champ
#    - La distance euclidienne à l'arbre le plus proche est calculée pour
#      chaque cellule avec terra::distance()
#    - Pour chaque cellule, l'angle vers l'arbre le plus proche est calculé
#
# 2. CLASSIFICATION AMONT/AVAL:
#    - La différence entre l'angle de l'arbre et l'angle du vent est calculée
#    - Si la différence est proche de 0° (±ouverture_angulaire/2): AMONT
#      La cellule est contre le vent (l'arbre protège la cellule)
#    - Si la différence est proche de 180° (±ouverture_angulaire/2): AVAL
#      La cellule est sous le vent (protégée par l'arbre en amont)
#    - Sinon: zone perpendiculaire au vent (non classée)
#
# 3. ANGLE DU VENT:
#    - Convention météorologique: 0° = Nord, 90° = Est, etc., sens horaire
#    - La fonction convertit automatiquement en coordonnées mathématiques
#
# 4. VISUALISATION:
#    - Les flèches de vent indiquent la direction vers laquelle souffle le vent
#    - Les couleurs bleues représentent les distances amont
#    - Les couleurs rouges représentent les distances aval
#    - Les arbres sont affichés comme des triangles verts
#
# ============================================================================

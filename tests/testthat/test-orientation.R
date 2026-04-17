# Tests pour les fonctions d'orientation et de distance aux bordures
# =============================================================================

test_that("calculer_orientation_champ fonctionne avec un rectangle simple", {
  skip_if_not_installed("sf")
  
  # Créer un rectangle horizontal (plus large que haut)
  coords <- matrix(c(
    0, 0,
    100, 0,
    100, 50,
    0, 50,
    0, 0
  ), ncol = 2, byrow = TRUE)
  
  poly <- sf::st_polygon(list(coords))
  champ <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 32618))
  
  result <- calculer_orientation_champ(champ, methode = "mbr")
  
  # Vérifications

  expect_type(result, "list")
  expect_true("angle" %in% names(result))
  expect_true("longueur" %in% names(result))
  expect_true("largeur" %in% names(result))
  expect_true("rapport_aspect" %in% names(result))
  
  # Le rectangle est 100x50, donc longueur ~100, largeur ~50
  expect_equal(result$longueur, 100, tolerance = 1)
  expect_equal(result$largeur, 50, tolerance = 1)
  expect_equal(result$rapport_aspect, 2, tolerance = 0.1)
  
  # L'angle devrait être proche de 0 ou 180 (horizontal)
  expect_true(result$angle < 5 || result$angle > 175)
})


test_that("calculer_orientation_champ fonctionne avec un rectangle à 45 degrés", {
  skip_if_not_installed("sf")
  
  # Créer un rectangle à 45 degrés
  # Centre à (50, 50), longueur 100, largeur 30
  angle_rad <- 45 * pi / 180
  L <- 100
  W <- 30
  cx <- 50
  cy <- 50
  
  # Coins du rectangle
  corners <- matrix(c(
    -L/2, -W/2,
    L/2, -W/2,
    L/2, W/2,
    -L/2, W/2,
    -L/2, -W/2
  ), ncol = 2, byrow = TRUE)
  
  # Rotation
  R <- matrix(c(cos(angle_rad), -sin(angle_rad),
                sin(angle_rad), cos(angle_rad)), nrow = 2)
  corners_rot <- corners %*% t(R)
  corners_rot[, 1] <- corners_rot[, 1] + cx
  corners_rot[, 2] <- corners_rot[, 2] + cy
  
  poly <- sf::st_polygon(list(corners_rot))
  champ <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 32618))
  
  result <- calculer_orientation_champ(champ, methode = "mbr")
  
  # L'angle devrait être proche de 45
  expect_equal(result$angle, 45, tolerance = 5)
  expect_equal(result$longueur, L, tolerance = 2)
  expect_equal(result$largeur, W, tolerance = 2)
})


test_that("calculer_orientation_champ retourne une géométrie MBR valide", {
  skip_if_not_installed("sf")
  
  # Créer un polygone quelconque
  coords <- matrix(c(
    0, 0,
    80, 10,
    100, 60,
    20, 50,
    0, 0
  ), ncol = 2, byrow = TRUE)
  
  poly <- sf::st_polygon(list(coords))
  champ <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 32618))
  
  result <- calculer_orientation_champ(champ, methode = "mbr")
  
  # Vérifier que la géométrie est valide
  expect_true(!is.null(result$geometry))
  expect_s3_class(result$geometry, "sf")
  expect_true(sf::st_is_valid(result$geometry))
})


test_that("calculer_distance_bordures_orientee retourne les bons éléments", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  
  # Créer un champ simple
  coords <- matrix(c(
    0, 0,
    200, 0,
    200, 100,
    0, 100,
    0, 0
  ), ncol = 2, byrow = TRUE)
  
  poly <- sf::st_polygon(list(coords))
  champ <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 32618))
  
  # Tester avec une résolution grossière pour la rapidité
  result <- calculer_distance_bordures_orientee(
    champ_poly = champ,
    resolution = 20,
    buffer = 10
  )
  
  # Vérifications de structure
  expect_type(result, "list")
  expect_true("distance_long" %in% names(result))
  expect_true("distance_large" %in% names(result))
  expect_true("distance_min" %in% names(result))
  expect_true("orientation" %in% names(result))
  
  # Vérifier que ce sont des SpatRaster
  expect_s4_class(result$distance_long, "SpatRaster")
  expect_s4_class(result$distance_large, "SpatRaster")
  expect_s4_class(result$distance_min, "SpatRaster")
})


test_that("calculer_distance_bordures_orientee: valeurs cohérentes", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  
  # Créer un champ carré simple
  coords <- matrix(c(
    0, 0,
    100, 0,
    100, 100,
    0, 100,
    0, 0
  ), ncol = 2, byrow = TRUE)
  
  poly <- sf::st_polygon(list(coords))
  champ <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 32618))
  
  result <- calculer_distance_bordures_orientee(
    champ_poly = champ,
    resolution = 10,
    buffer = 5
  )
  
  # Les distances doivent être positives ou NA
  vals_long <- terra::values(result$distance_long)
  vals_long <- vals_long[!is.na(vals_long)]
  expect_true(all(vals_long >= 0))
  
  vals_large <- terra::values(result$distance_large)
  vals_large <- vals_large[!is.na(vals_large)]
  expect_true(all(vals_large >= 0))
  
  # La distance min doit être <= à chaque composante
  vals_min <- terra::values(result$distance_min)
  vals_min <- vals_min[!is.na(vals_min)]
  expect_true(all(vals_min >= 0))
  
  # Pour un carré 100x100, la distance max au centre est ~50m
  expect_true(max(vals_min) <= 55)
})


test_that("calculer_distance_bordures_orientee fonctionne avec des points", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  
  # Créer un champ
  coords <- matrix(c(
    0, 0,
    200, 0,
    200, 100,
    0, 100,
    0, 0
  ), ncol = 2, byrow = TRUE)
  
  poly <- sf::st_polygon(list(coords))
  champ <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 32618))
  
  # Créer quelques points
  points_coords <- matrix(c(
    50, 50,   # Centre-ish
    10, 50,   # Près du bord gauche
    190, 50,  # Près du bord droit
    100, 10   # Près du bord bas
  ), ncol = 2, byrow = TRUE)
  
  points <- sf::st_as_sf(
    data.frame(id = 1:4, x = points_coords[, 1], y = points_coords[, 2]),
    coords = c("x", "y"),
    crs = 32618
  )
  
  result <- calculer_distance_bordures_orientee(
    points_sf = points,
    champ_poly = champ
  )
  
  # Vérifier la structure
  expect_true("points" %in% names(result))
  expect_s3_class(result$points, "sf")
  expect_true("distance_long" %in% names(result$points))
  expect_true("distance_large" %in% names(result$points))
  expect_true("distance_min" %in% names(result$points))
  
  # Vérifier les valeurs
  # Le point (10, 50) est à ~10m du bord gauche
  expect_equal(result$points$distance_min[2], 10, tolerance = 2)
})


test_that("classifier_distances_bordures fonctionne correctement", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  
  # Créer un champ et calculer les distances
  coords <- matrix(c(
    0, 0,
    200, 0,
    200, 100,
    0, 100,
    0, 0
  ), ncol = 2, byrow = TRUE)
  
  poly <- sf::st_polygon(list(coords))
  champ <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 32618))
  
  dist_result <- calculer_distance_bordures_orientee(
    champ_poly = champ,
    resolution = 20,
    buffer = 10
  )
  
  # Classifier
  classes <- classifier_distances_bordures(dist_result)
  
  # Vérifications de structure
  expect_type(classes, "list")
  expect_true("classe_long" %in% names(classes))
  expect_true("classe_large" %in% names(classes))
  expect_true("classe_combinee" %in% names(classes))
  expect_true("table_classes" %in% names(classes))
  
  # Vérifier que les classes sont des entiers positifs
  vals <- terra::values(classes$classe_long)
  vals <- vals[!is.na(vals)]
  expect_true(all(vals >= 1))
  expect_true(all(vals == floor(vals)))
})


test_that("classifier_distances_bordures valide les seuils et labels", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  
  # Créer un champ simple
  coords <- matrix(c(0, 0, 100, 0, 100, 100, 0, 100, 0, 0), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(coords))
  champ <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 32618))
  
  dist_result <- calculer_distance_bordures_orientee(
    champ_poly = champ,
    resolution = 20,
    buffer = 5
  )
  
  # Test avec labels incorrects (trop peu)
  expect_error(
    classifier_distances_bordures(
      dist_result,
      seuils_long = c(10, 20, 30),
      labels_long = c("a", "b")  # Devrait être 4 labels pour 3 seuils
    ),
    "labels_long doit avoir"
  )
  
  # Test avec seuils personnalisés valides
  classes <- classifier_distances_bordures(
    dist_result,
    seuils_long = c(10, 20),
    seuils_large = c(5, 15),
    labels_long = c("proche", "moyen", "loin"),
    labels_large = c("proche", "moyen", "loin")
  )
  
  expect_equal(length(classes$labels_long), 3)
  expect_equal(length(classes$labels_large), 3)
  expect_equal(nrow(classes$table_classes), 9)  # 3 x 3 combinaisons
})


test_that("visualiser_distances_bordures génère des ggplots valides", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("ggplot2")
  
  # Créer un champ simple
  coords <- matrix(c(0, 0, 100, 0, 100, 50, 0, 50, 0, 0), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(coords))
  champ <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 32618))
  
  dist_result <- calculer_distance_bordures_orientee(
    champ_poly = champ,
    resolution = 20,
    buffer = 5
  )
  
  # Tester chaque type
  p_long <- visualiser_distances_bordures(dist_result, type = "long")
  expect_s3_class(p_long, "ggplot")
  
  p_large <- visualiser_distances_bordures(dist_result, type = "large")
  expect_s3_class(p_large, "ggplot")
  
  p_min <- visualiser_distances_bordures(dist_result, type = "min")
  expect_s3_class(p_min, "ggplot")
  
  # Tester comparaison
  plots <- visualiser_distances_bordures(dist_result, type = "comparaison")
  expect_type(plots, "list")
  expect_true(all(c("long", "large", "min") %in% names(plots)))
})


test_that("visualiser_classes_bordures génère des ggplots valides", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  skip_if_not_installed("ggplot2")
  
  # Créer un champ et classifier
  coords <- matrix(c(0, 0, 100, 0, 100, 50, 0, 50, 0, 0), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(coords))
  champ <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 32618))
  
  dist_result <- calculer_distance_bordures_orientee(
    champ_poly = champ,
    resolution = 20,
    buffer = 5
  )
  
  classes <- classifier_distances_bordures(dist_result)
  
  # Tester chaque type
  p_long <- visualiser_classes_bordures(classes, type = "long")
  expect_s3_class(p_long, "ggplot")
  
  p_large <- visualiser_classes_bordures(classes, type = "large")
  expect_s3_class(p_large, "ggplot")
  
  p_comb <- visualiser_classes_bordures(classes, type = "combinee")
  expect_s3_class(p_comb, "ggplot")
})


test_that("calculer_distance_bordures_orientee gère les erreurs correctement", {
  skip_if_not_installed("sf")
  
  # Sans champ_poly
  expect_error(
    calculer_distance_bordures_orientee(champ_poly = NULL),
    "champ_poly est requis"
  )
})

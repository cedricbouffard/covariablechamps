# Tests pour les fonctions d'extraction et classification des arbres/haies
# =============================================================================

test_that("extraire_classifier_haies_lidar requiert les packages necessaires", {
 skip_if_not_installed("lidR")
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  skip_if_not_installed("dbscan")
  
  # Test que la fonction existe
  expect_true(exists("extraire_classifier_haies_lidar"))
})


test_that("extraire_ligne_centrale_haies gere les entrees NULL", {
  skip_if_not_installed("sf")
  
  # Doit retourner NULL ou warning si entree NULL
  expect_warning(
    result <- extraire_ligne_centrale_haies(NULL),
    "Aucune haie"
  )
})


test_that("calculer_zones_vent_spline gere les entrees NULL", {
  skip_if_not_installed("sf")
  
  # Doit retourner NULL avec warning si entree NULL
  expect_warning(
    result <- calculer_zones_vent_spline(NULL),
    "Aucune haie"
  )
  expect_null(result)
})


test_that("rasteriser_zones_gradient gere les entrees NULL", {
  skip_if_not_installed("terra")
  
  expect_warning(
    result <- rasteriser_zones_gradient(NULL),
    "Aucune zone"
  )
  expect_null(result)
})


test_that("rasteriser_zones_gradient_v2 gere les entrees NULL", {
  skip_if_not_installed("terra")
  
  expect_warning(
    result <- rasteriser_zones_gradient_v2(NULL),
    "Aucune zone"
  )
  expect_null(result)
})


test_that("extraire_ligne_centrale_haies retourne le bon format", {
  skip_if_not_installed("sf")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("purrr")
  
  # Creer un faux rectangle de haie
  coords <- matrix(c(
    0, 0,
    100, 0,
    100, 10,
    0, 10,
    0, 0
  ), ncol = 2, byrow = TRUE)
  
  poly <- sf::st_polygon(list(coords))
  
  haies <- sf::st_sf(
    cluster = 1,
    n_arbres = 10,
    hauteur_p95 = 15,
    largeur = 10,
    longueur = 100,
    aspect = 10,
    linearite = 20,
    angle_deg = 0,
    x_center = 50,
    y_center = 5,
    classe = "haie_brise_vent",
    geometry = sf::st_sfc(poly, crs = 32618)
  )
  
  # Tester les trois types de sortie
  result_tous <- extraire_ligne_centrale_haies(haies, output_type = "tous")
  expect_type(result_tous, "list")
  expect_true("lignes" %in% names(result_tous))
  expect_true("extremites" %in% names(result_tous))
  
  result_lignes <- extraire_ligne_centrale_haies(haies, output_type = "lignes")
  expect_s3_class(result_lignes, "sf")
  
  result_extremites <- extraire_ligne_centrale_haies(haies, output_type = "extremites")
  expect_s3_class(result_extremites, "sf")
  # Chaque haie a 2 extremites
  expect_equal(nrow(result_extremites), 2)
})


test_that("calculer_zones_vent_spline cree les zones correctement", {
  skip_if_not_installed("sf")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("purrr")
  
  # Creer un faux rectangle de haie
  coords <- matrix(c(
    0, 0,
    100, 0,
    100, 10,
    0, 10,
    0, 0
  ), ncol = 2, byrow = TRUE)
  
  poly <- sf::st_polygon(list(coords))
  
  haies <- sf::st_sf(
    cluster = 1,
    n_arbres = 10,
    hauteur_p95 = 15,
    largeur = 10,
    longueur = 100,
    aspect = 10,
    linearite = 20,
    angle_deg = 0,
    x_center = 50,
    y_center = 5,
    classe = "haie_brise_vent",
    geometry = sf::st_sfc(poly, crs = 32618)
  )
  
  # Calculer les zones avec 3 facteurs H
  zones <- calculer_zones_vent_spline(
    haies,
    direction_vent = 225,
    facteur_hauteur = 1:3,
    n_points = 20
  )
  
  expect_s3_class(zones, "sf")
  # 1 haie x 3 facteurs = 3 zones
  expect_equal(nrow(zones), 3)
  expect_true("facteur_h" %in% names(zones))
  expect_true("direction_vent" %in% names(zones))
  expect_true("distance_H" %in% names(zones))
})

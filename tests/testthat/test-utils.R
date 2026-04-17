# Tests pour les fonctions utilitaires

test_that("labels_geomorphons retourne un vecteur nommé correct", {
  labels <- labels_geomorphons()
  
  expect_type(labels, "character")
  expect_length(labels, 10)
  expect_named(labels)
  
  # Vérifier que tous les noms 1-10 sont présents
  expect_equal(sort(as.numeric(names(labels))), 1:10)
  
  # Vérifier quelques valeurs spécifiques
  expect_true(labels["1"] == "Plat")
  expect_true(labels["2"] == "Pic")
  expect_true(labels["9"] == "Vallée")
})

test_that("verifier_disponibilite_lidar gère les entrées", {
  skip_if_not_installed("sf")
  skip_if_offline()
  
  # Test avec un petit polygone valide au Québec
  coords <- matrix(c(
    -71.2, 46.8,
    -71.1, 46.8,
    -71.1, 46.9,
    -71.2, 46.9,
    -71.2, 46.8
  ), ncol = 2, byrow = TRUE)
  poly <- sf::st_sfc(sf::st_polygon(list(coords)), crs = 4326)
  poly <- sf::st_sf(geometry = poly)
  
  # Peut retourner un data.frame avec les années disponibles ou NULL si pas de données
  result <- verifier_disponibilite_lidar(poly)
  expect_true(is.null(result) || is.data.frame(result))
  
  # Si un data.frame est retourné, vérifier sa structure

  if (is.data.frame(result)) {
    expect_true("annee" %in% names(result))
    expect_true("nb_tuiles" %in% names(result))
  }
})

test_that("verifier_disponibilite_lidar gère les polygones vides", {
  skip_if_not_installed("sf")
  
  # Test avec polygone vide - devrait retourner une erreur informative
  poly <- sf::st_sfc(sf::st_polygon())
  poly <- sf::st_sf(geometry = poly)
  sf::st_crs(poly) <- 4326
  
  # Un polygone vide devrait causer une erreur
  expect_error(verifier_disponibilite_lidar(poly))
})

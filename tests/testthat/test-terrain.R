# Tests pour les fonctions terrain

test_that("calculer_pente valide les entrées", {
  skip_if_not_installed("terra")
  
  # Test avec entrée invalide
  expect_error(calculer_pente("pas_un_raster"), "doit être un objet SpatRaster")
  expect_error(calculer_pente(123), "doit être un objet SpatRaster")
  expect_error(calculer_pente(NULL), "doit être un objet SpatRaster")
})

test_that("calculer_pente retourne un SpatRaster", {
  skip_if_not_installed("terra")
  
  # Créer un MNT de test simple
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
  terra::values(r) <- matrix(1:100, nrow = 10, byrow = TRUE)
  
  pente <- calculer_pente(r)
  
  expect_s4_class(pente, "SpatRaster")
  expect_equal(names(pente), "pente")
})

test_that("calculer_aspect valide les entrées", {
  skip_if_not_installed("terra")
  
  # Test avec entrée invalide
  expect_error(calculer_aspect("pas_un_raster"), "doit être un objet SpatRaster")
  expect_error(calculer_aspect(123), "doit être un objet SpatRaster")
})

test_that("calculer_aspect retourne un SpatRaster", {
  skip_if_not_installed("terra")
  
  # Créer un MNT de test
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
  terra::values(r) <- runif(100, 0, 100)
  
  aspect <- calculer_aspect(r)
  
  expect_s4_class(aspect, "SpatRaster")
  expect_equal(names(aspect), "aspect")
})

test_that("calculer_geomorphons valide les entrées", {
  skip_if_not_installed("terra")
  
  # Test avec entrée invalide
  expect_error(calculer_geomorphons("pas_un_raster"), "doit être un objet SpatRaster")
})

test_that("calculer_geomorphons vérifie la disponibilité de rgeomorphon", {
  skip_if_not_installed("terra")
  
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 100, ymin = 0, ymax = 100)
  terra::values(r) <- runif(100, 0, 100)
  
  # Si rgeomorphon n'est pas installé, doit retourner une erreur informative
  if (!requireNamespace("rgeomorphon", quietly = TRUE)) {
    expect_error(calculer_geomorphons(r), "rgeomorphon")
  }
})

test_that("labels_geomorphons retourne les bons labels", {
  labels <- labels_geomorphons()
  
  expect_type(labels, "character")
  expect_length(labels, 10)
  expect_equal(names(labels), as.character(1:10))
  expect_equal(labels["1"], c("1" = "Plat"))
  expect_equal(labels["9"], c("9" = "Vallée"))
})

test_that("extraire_covariables_terrain valide les entrées", {
  skip_if_not_installed("sf")
  
  # Test avec entrée invalide
  expect_error(extraire_covariables_terrain(123))
  expect_error(extraire_covariables_terrain("fichier_inexistant.shp"))
})

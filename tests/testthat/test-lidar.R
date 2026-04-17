# Tests pour les fonctions LiDAR

# Charger les données de test
test_data_path <- system.file("extdata", "M2.shp", package = "covariablechamps")

test_that("telecharger_lidar valide les entrées", {
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  
  # Test avec entrée invalide
  expect_error(telecharger_lidar(123), "doit être un objet sf")
  
  # Test avec polygone vide
  sf_obj <- sf::st_sfc(sf::st_polygon(), crs = 4326)
  sf_obj <- sf::st_sf(geometry = sf_obj)
  expect_error(telecharger_lidar(sf_obj))
})

test_that("verifier_disponibilite_lidar fonctionne correctement", {
  skip_if_not_installed("sf")
  skip_if_not_installed("rstac")
  skip_on_cran()
  skip_if_offline()
  
  # Utiliser les données de test
  if (file.exists(test_data_path)) {
    result <- verifier_disponibilite_lidar(test_data_path)
    
    # Le résultat doit être NULL ou un data.frame
    expect_true(is.null(result) || is.data.frame(result))
    
    if (!is.null(result)) {
      expect_true(all(c("annee", "nb_tuiles") %in% names(result)))
      expect_type(result$annee, "double")
      expect_type(result$nb_tuiles, "double")
    }
  }
})

test_that("telecharger_lidar gère le CRS manquant", {
  skip_if_not_installed("sf")
  
  # Créer un polygone sans CRS
  poly <- sf::st_sfc(sf::st_point(c(0, 0)))
  poly <- sf::st_sf(geometry = poly)
  sf::st_crs(poly) <- NA
  
  # Ne devrait pas planter - assigne WGS84 par défaut
  expect_error(telecharger_lidar(poly), NA)
})

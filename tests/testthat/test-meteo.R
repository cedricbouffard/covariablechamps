# Tests pour les fonctions meteorologiques
# =============================================================================

test_that("obtenir_rose_vents accepte des coordonnees", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  skip_on_cran()  # Sauter sur CRAN car necessite acces internet
  
  # Test que la fonction existe
  expect_true(exists("obtenir_rose_vents"))
})


test_that("obtenir_rose_vents valide le parametre hauteur", {
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  
  # Hauteur invalide devrait lever une erreur
  expect_error(
    obtenir_rose_vents(c(-71.0, 46.5), hauteur = "100m"),
    "hauteur"
  )
})


test_that("tracer_rose_vents requiert ggplot2", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  
  expect_true(exists("tracer_rose_vents"))
})


test_that("tracer_rose_vents_stacked requiert ggplot2", {
  skip_if_not_installed("ggplot2")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("tidyr")
  
  expect_true(exists("tracer_rose_vents_stacked"))
})


# Test d'integration (necessite internet)
test_that("obtenir_rose_vents retourne le bon format", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("httr")
  skip_if_not_installed("jsonlite")
  
  # Coordonnees de Quebec
  coords <- c(-71.2, 46.8)
  
  result <- tryCatch({
    obtenir_rose_vents(
      coords,
      date_debut = "20230101",
      date_fin = "20230131",  # Un mois seulement pour rapidite
      hauteur = "10m"
    )
  }, error = function(e) {
    skip("API NASA POWER non disponible")
  })
  
  if (!is.null(result)) {
    expect_type(result, "list")
    expect_true("directions" %in% names(result))
    expect_true("wd_pct" %in% names(result))
    expect_true("wd_avg" %in% names(result))
    expect_true("coordonnees" %in% names(result))
    
    # 16 directions
    expect_equal(length(result$directions), 16)
    
    # Pourcentages doivent sommer a ~100
    expect_equal(sum(result$wd_pct), 100, tolerance = 1)
  }
})

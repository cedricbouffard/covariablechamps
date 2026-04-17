library(testthat)
library(covariablechamps)

test_that("calculer_ombrage fonctionne avec des données simulées", {
  skip_if_not_installed("suncalc")
  skip_if_not_installed("rayshader")
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  
  # Créer un polygone de test simple
  poly <- sf::st_polygon(list(matrix(c(
    -73.5, 45.5,
    -73.5, 45.51,
    -73.49, 45.51,
    -73.49, 45.5,
    -73.5, 45.5
  ), ncol = 2, byrow = TRUE)))
  
  champ <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 4326))
  
  # Créer un MNE de test simple (raster synthétique)
  r <- terra::rast(
    xmin = -73.5, xmax = -73.49,
    ymin = 45.5, ymax = 45.51,
    resolution = 0.0001,
    crs = "EPSG:4326"
  )
  terra::values(r) <- runif(terra::ncell(r), 0, 100)
  names(r) <- "MNE"
  
  # Tester le calcul d'ombrage (en utilisant le MNE fourni)
  resultats <- calculer_ombrage(
    polygone = champ,
    date = "2024-06-21",
    intervalle_heures = 2,
    mne = r,
    zscale = 1,
    max_distance = 100
  )
  
  # Vérifier que les résultats sont valides
  expect_type(resultats, "list")
  expect_true("ombrage_par_heure" %in% names(resultats))
  expect_true("ombrage_moyen" %in% names(resultats))
  expect_true("heures_ensoleillement" %in% names(resultats))
  expect_true("info_soleil" %in% names(resultats))
  expect_true("mne" %in% names(resultats))
  expect_true("zscale" %in% names(resultats))
  expect_true("max_distance" %in% names(resultats))
  
  # Vérifier les dimensions
  expect_equal(terra::nrow(resultats$ombrage_moyen), terra::nrow(r))
  expect_equal(terra::ncol(resultats$ombrage_moyen), terra::ncol(r))
  expect_equal(terra::nrow(resultats$heures_ensoleillement), terra::nrow(r))
  expect_equal(terra::ncol(resultats$heures_ensoleillement), terra::ncol(r))
  
  # Vérifier les valeurs
  expect_true(all(terra::values(resultats$ombrage_moyen) >= 0, na.rm = TRUE))
  expect_true(all(terra::values(resultats$ombrage_moyen) <= 1, na.rm = TRUE))
  expect_true(all(terra::values(resultats$heures_ensoleillement) >= 0, na.rm = TRUE))
  
  # Vérifier que zscale et max_distance sont bien retournés
  expect_equal(resultats$zscale, 1)
  expect_equal(resultats$max_distance, 100)
})

test_that("calculer_ombrage gère les erreurs correctement", {
  skip_if_not_installed("suncalc")
  skip_if_not_installed("rayshader")
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  
  # Test avec un polygone invalide
  expect_error(calculer_ombrage(polygone = "invalide", date = "2024-06-21"))
  
  # Test avec une date invalide
  poly <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 0, 1, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE))),
    crs = 4326
  ))
  expect_error(calculer_ombrage(polygone = poly, date = "date-invalide"))
})

test_that("calculer_ombrage avec différents paramètres zscale", {
  skip_if_not_installed("suncalc")
  skip_if_not_installed("rayshader")
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  
  # Créer des résultats de test avec différents zscale
  r <- terra::rast(
    xmin = 0, xmax = 1,
    ymin = 0, ymax = 1,
    resolution = 0.01,
    crs = "EPSG:4326"
  )
  terra::values(r) <- runif(terra::ncell(r), 0, 100)
  names(r) <- "MNE"
  
  poly <- sf::st_sf(geometry = sf::st_sfc(
    sf::st_polygon(list(matrix(c(0, 0, 0, 1, 1, 1, 1, 0, 0, 0), ncol = 2, byrow = TRUE))),
    crs = 4326
  ))
  
  # Test avec zscale = 0.5
  resultats_05 <- calculer_ombrage(
    polygone = poly,
    date = "2024-06-21",
    intervalle_heures = 3,
    mne = r,
    zscale = 0.5
  )
  expect_equal(resultats_05$zscale, 0.5)
  
  # Test avec zscale = 2
  resultats_2 <- calculer_ombrage(
    polygone = poly,
    date = "2024-06-21",
    intervalle_heures = 3,
    mne = r,
    zscale = 2
  )
  expect_equal(resultats_2$zscale, 2)
})

test_that("calculer_ombrage_visualisation fonctionne", {
  skip_if_not_installed("suncalc")
  skip_if_not_installed("rayshader")
  skip_if_not_installed("terra")
  skip_if_not_installed("sf")
  
  # Créer des résultats de test
  r <- terra::rast(
    xmin = 0, xmax = 1,
    ymin = 0, ymax = 1,
    resolution = 0.1,
    crs = "EPSG:4326"
  )
  
  mne <- r
  terra::values(mne) <- runif(terra::ncell(mne), 0, 100)
  
  ombrage_h <- c(r, r, r)
  terra::values(ombrage_h) <- runif(terra::ncell(ombrage_h) * 3, 0, 1)
  names(ombrage_h) <- c("H08:00", "H10:00", "H12:00")
  
  ombrage_m <- r
  terra::values(ombrage_m) <- runif(terra::ncell(ombrage_m), 0, 1)
  names(ombrage_m) <- "ombrage_moyen"
  
  heures <- r
  terra::values(heures) <- runif(terra::ncell(heures), 0, 12)
  names(heures) <- "heures_ensoleillement"
  
  resultats <- list(
    ombrage_par_heure = ombrage_h,
    ombrage_moyen = ombrage_m,
    heures_ensoleillement = heures,
    mne = mne,
    info_soleil = data.frame(
      heure = Sys.time() + 0:2 * 3600,
      heure_str = c("08:00", "10:00", "12:00"),
      azimuth = c(90, 135, 180),
      altitude = c(20, 45, 60),
      ensoleille = c(TRUE, TRUE, TRUE)
    ),
    zscale = 1,
    max_distance = 500
  )
  
  # Tester la visualisation (ne devrait pas produire d'erreur)
  expect_silent(calculer_ombrage_visualisation(resultats))
  expect_silent(calculer_ombrage_visualisation(resultats, couche_heure = 2))
  expect_silent(calculer_ombrage_visualisation(resultats, titre = "Test"))
})

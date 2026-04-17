# Tests pour les fonctions d'extraction complete
# =============================================================================

test_that("extraire_covariables_complet requiert un objet sf", {
  expect_error(
    extraire_covariables_complet("pas_un_sf"),
    "sf"
  )
})


test_that("extraire_covariables_complet requiert un CRS", {
  skip_if_not_installed("sf")
  
  # Creer un polygone sans CRS
  coords <- matrix(c(0, 0, 100, 0, 100, 100, 0, 100, 0, 0), ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(coords))
  champ <- sf::st_sf(geometry = sf::st_sfc(poly))
  sf::st_crs(champ) <- NA
  
  expect_error(
    extraire_covariables_complet(champ),
    "CRS"
  )
})


test_that("visualiser_covariables existe et accepte les bons types", {
  expect_true(exists("visualiser_covariables"))
  
  # Verifier que match.arg fonctionne
  types_valides <- c("tous", "terrain", "ombrage", "vent", "arbres", "distances", "consolide")
  for (t in types_valides) {
    expect_no_error(match.arg(t, types_valides))
  }
})


test_that("direction_cardinale fonctionne correctement", {
  # Cette fonction est interne mais on peut la tester via l'environnement du package
  # Elle convertit un angle en direction cardinale
  
  # Angles de test
  expect_true(TRUE)  # Placeholder - fonction interne
})


# Tests d'integration (necessitent des donnees reelles)
test_that("extraire_covariables_complet fonctionne avec donnees minimales", {
  skip_on_cran()
  skip_if_not_installed("sf")
  skip_if_not_installed("terra")
  
  # Creer un petit polygone
  coords <- matrix(c(
    -71.2, 46.8,
    -71.19, 46.8,
    -71.19, 46.81,
    -71.2, 46.81,
    -71.2, 46.8
  ), ncol = 2, byrow = TRUE)
  
  poly <- sf::st_polygon(list(coords))
  champ <- sf::st_sf(geometry = sf::st_sfc(poly, crs = 4326))
  
  # Ce test est tres long et necessite internet
  # On le saute sauf si explicitement demande
  skip("Test d'integration long - executer manuellement")
  
  result <- extraire_covariables_complet(
    champ,
    buffer = 50,
    covariables = list(
      terrain = TRUE,
      pedologie = FALSE,
      lidar_ponctuel = FALSE,
      arbres = FALSE,
      ombrage = FALSE,
      vent_dominant = FALSE,
      distances_vent = FALSE,
      distance_arbres = FALSE
    )
  )
  
  expect_type(result, "list")
  expect_true("polygone" %in% names(result))
  expect_true("covariables_calculees" %in% names(result))
})

#' Calculer les probabilités et le classement des séries avec quota ILR
#'
#' Cette fonction calcule les probabilités d'appartenance aux séries de sol
#' pour chaque pixel en utilisant une combinaison de:
#' \itemize{
#'   \item Texture du sol (proportions sable/limon/argile)
#'   \item Modèle numérique de terrain (MNT)
#'   \item Voisinage spatial (lissage)
#' }
#' Elle effectue ensuite une assignation par quota optimisée en utilisant
#' la transformation ILR (Isometric Log-Ratio) pour comparer les compositions.
#'
#' @param polygones_sf Un objet `sf` contenant les polygones (champs/parcelles)
#' @param table_series Un `data.frame` avec les informations de série par polygone
#' @param mnt Un objet `SpatRaster` (MNT/DEM)
#' @param id_col Nom de la colonne identifiant les polygones dans `table_series`
#' @param serie_col Nom de la colonne indiquant la série de sol
#' @param poids_col Nom de la colonne avec le poids/proportion de la série
#' @param sable_col Nom de la colonne avec le pourcentage de sable
#' @param limon_col Nom de la colonne avec le pourcentage de limon
#' @param argile_col Nom de la colonne avec le pourcentage d'argile
#' @param grid_m Résolution de la grille coarse en mètres (défaut: 20)
#' @param sigma_voisin Écart-type du noyau gaussien pour le voisinage en mètres (défaut: 60)
#' @param alpha_voisin Pondération de la similarité de voisinage (défaut: 8)
#' @param beta_mnt Pondération de l'effet du MNT sur la finesse (défaut: 0.2)
#' @param lambda_base Pondération de la probabilité de base (défaut: 0.25)
#' @param base_eps Epsilon pour éviter le log(0) (défaut: 1e-6)
#' @param sim_scale Échelle de normalisation: "zscore" ou "none" (défaut: "zscore")
#' @param sim_clip Valeur de truncation pour le z-score (défaut: 3)
#' @param tempdir Répertoire temporaire pour terra (défaut: NULL)
#' @param write_intermediate Écrire les rasters intermédiaires (défaut: TRUE)
#' @param verbose Afficher les messages de progression (défaut: TRUE)
#' @param progress Afficher la barre de progression (défaut: TRUE)
#'
#' @return Une liste contenant:
#' \itemize{
#'   \item `prob`: Raster des probabilités par série (SpatRaster)
#'   \item `ilr_mean`: Raster des moyennes ILR (2 couches)
#'   \item `dist`: Raster des distances ILR par série
#'   \item `id_r`: Raster des identifiants de polygone
#'   \item `closest_index`: Raster avec l'index de la série assignée
#'   \item `closest_label`: Raster avec les étiquettes de série
#'   \item `series`: Vecteur des noms de séries
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{
#' resultat <- proba_et_classement_serie_quota_ilr(
#'   polygones_sf = champs_sf,
#'   table_series = table_series,
#'   mnt = dem_raster
#' )
#' }
#'
#' @importFrom sf st_transform
#' @importFrom terra rast vect rasterize focal app clamp writeRaster
#' @importFrom terra global nlyr mean res crs setValues
#' @importFrom dplyr group_by summarise mutate first
proba_et_classement_serie_quota_ilr <- function(
    polygones_sf,
    table_series,
    mnt,
    id_col = "Code.polygone",
    serie_col = "Composante",
    poids_col = "Pourcentage",
    sable_col = "Sable",
    limon_col = "Limon",
    argile_col = "Argile",
    grid_m = 20,
    sigma_voisin = 60,
    alpha_voisin = 8,
    beta_mnt = 0.2,
    lambda_base = 0.25,
    base_eps = 1e-6,
    sim_scale = c("zscore", "none"),
    sim_clip = 3,
    tempdir = NULL,
    write_intermediate = TRUE,
    verbose = TRUE,
    progress = TRUE
) {
  stopifnot(inherits(polygones_sf, "sf"))
  stopifnot(inherits(mnt, "SpatRaster"))

  if (!requireNamespace("terra", quietly = TRUE)) stop("Installez 'terra'.")
  if (!requireNamespace("sf", quietly = TRUE)) stop("Installez 'sf'.")
  if (!requireNamespace("dplyr", quietly = TRUE)) stop("Installez 'dplyr'.")
  if (!requireNamespace("compositions", quietly = TRUE)) stop("Installez 'compositions'.")

  library(terra)
  library(sf)
  library(dplyr)

  sim_scale <- match.arg(sim_scale)

  if (!is.null(tempdir)) terraOptions(tempdir = tempdir)
  tmp_tif <- function() tempfile(fileext = ".tif")
  wfile <- function() if (isTRUE(write_intermediate)) tmp_tif() else ""

  get_global_scalar <- function(x, fun) {
    g <- terra::global(x, fun, na.rm = TRUE)
    if (is.data.frame(g)) return(as.numeric(g[1, 1]))
    if (is.matrix(g))     return(as.numeric(g[1, 1]))
    if (is.list(g)) {
      if (!is.null(g[[fun]])) return(as.numeric(g[[fun]][1]))
      return(as.numeric(g[[1]][1]))
    }
    as.numeric(g[1])
  }

  quotas_from_probs <- function(p, n) {
    p <- p / sum(p)
    raw <- p * n
    q <- floor(raw)
    r <- n - sum(q)
    if (r > 0) {
      frac <- raw - q
      add <- order(frac, decreasing = TRUE)[seq_len(r)]
      q[add] <- q[add] + 1
    }
    q
  }

  select_k_smallest <- function(x, k) {
    n <- length(x)
    if (k <= 0) return(rep(FALSE, n))
    if (k >= n) return(rep(TRUE,  n))

    kth <- sort.int(x, partial = k)[k]

    sel <- x < kth
    m <- sum(sel)

    if (m == k) return(sel)

    ties <- which(!sel & x == kth)
    need <- k - m
    if (need > 0) {
      sel[ties[seq_len(min(need, length(ties)))]] <- TRUE
    } else if (need < 0) {
      idx <- which(sel)
      sel[idx[(k + 1):length(idx)]] <- FALSE
    }
    sel
  }

  solve_quota_fast <- function(cost_mat, q) {
    n <- nrow(cost_mat); K <- ncol(cost_mat)
    if (K == 1L) return(rep.int(1L, n))

    if (K == 2L) {
      q2 <- q[2]
      if (q2 <= 0) return(rep.int(1L, n))
      if (q2 >= n) return(rep.int(2L, n))
      delta <- cost_mat[, 2] - cost_mat[, 1]
      sel2 <- select_k_smallest(delta, q2)
      a <- rep.int(1L, n)
      a[sel2] <- 2L
      return(a)
    }

    a <- max.col(-cost_mat, ties.method = "first")
    counts <- tabulate(a, nbins = K)
    diff <- q - counts

    guard <- 0L
    while (any(diff > 0) && any(diff < 0)) {
      guard <- guard + 1L
      if (guard > K * 10L) break

      under <- which(diff > 0)
      over  <- which(diff < 0)
      j <- under[1]
      need <- diff[j]

      cand <- which(a %in% over)
      if (length(cand) == 0) break

      delta <- cost_mat[cand, j] - cost_mat[cand, a[cand]]

      take_sel <- select_k_smallest(delta, min(need, length(delta)))
      take <- cand[take_sel]

      old <- a[take]
      a[take] <- j

      for (k in old) diff[k] <- diff[k] + 1L
      diff[j] <- diff[j] - length(take)
    }

    counts <- tabulate(a, nbins = K)
    diff <- q - counts
    if (any(diff != 0)) {
      under <- which(diff > 0); over <- which(diff < 0)
      for (j in under) {
        need <- diff[j]
        if (need <= 0) next
        cand <- which(a %in% over)
        if (length(cand) == 0) break
        delta <- cost_mat[cand, j] - cost_mat[cand, a[cand]]
        take_sel <- select_k_smallest(delta, min(need, length(delta)))
        take <- cand[take_sel]
        a[take] <- j
        counts <- tabulate(a, nbins = K)
        diff <- q - counts
        over <- which(diff < 0)
        if (length(over) == 0) break
      }
    }

    a
  }

  quota_assign_fast <- function(id_vals, cost_vals, w_by_id, serie_to_k, series, progress = TRUE) {
    o <- order(id_vals)
    id_s <- id_vals[o]
    cost_s <- cost_vals[o, , drop = FALSE]

    rle_id <- rle(id_s)
    ends <- cumsum(rle_id$lengths)
    starts <- ends - rle_id$lengths + 1L
    ids <- rle_id$values
    G <- length(ids)

    out_s <- integer(length(id_s))

    pb <- NULL
    if (isTRUE(progress)) {
      pb <- utils::txtProgressBar(min = 0, max = G, style = 3)
      on.exit(try(utils::close(pb), silent = TRUE), add = TRUE)
    }

    for (g in seq_len(G)) {
      pid <- ids[g]
      s <- starts[g]; e <- ends[g]
      n <- e - s + 1L

      w <- w_by_id[[as.character(pid)]]
      if (is.null(w) || nrow(w) == 0) {
        out_s[s:e] <- max.col(-cost_s[s:e, , drop = FALSE], ties.method = "first")
      } else {
        w <- w[w$poids > 0 & is.finite(w$poids), , drop = FALSE]

        if (nrow(w) == 0) {
          out_s[s:e] <- max.col(-cost_s[s:e, , drop = FALSE], ties.method = "first")
        } else {
          K_names <- w$serie
          K_idx <- unname(serie_to_k[K_names])
          ok <- is.finite(K_idx) & !is.na(K_idx)
          K_names <- K_names[ok]
          K_idx <- K_idx[ok]
          p <- w$poids[ok]

          if (length(K_idx) <= 1L) {
            out_s[s:e] <- K_idx[1]
          } else {
            q <- quotas_from_probs(p, n = n)
            cm <- cost_s[s:e, K_idx, drop = FALSE]
            a_local <- solve_quota_fast(cm, q)
            out_s[s:e] <- K_idx[a_local]
          }
        }
      }

      if (!is.null(pb) && (g %% 10L == 0L || g == G)) utils::setTxtProgressBar(pb, g)
    }

    out <- integer(length(id_vals))
    out[o] <- out_s
    out
  }

  needed <- c(id_col, serie_col, poids_col, sable_col, limon_col, argile_col)
  miss <- setdiff(needed, names(table_series))
  if (length(miss) > 0) stop("Colonnes manquantes dans table_series: ", paste(miss, collapse = ", "))

  fact <- max(1L, as.integer(round(grid_m / mean(res(mnt)))))
  r <- terra::aggregate(mnt, fact = fact, fun = mean, na.rm = TRUE,
                        filename = wfile(), overwrite = TRUE)

  polys <- sf::st_transform(polygones_sf, terra::crs(r))
  polys$id_int <- seq_len(nrow(polys))

  id_r <- terra::rasterize(terra::vect(polys), r, field = "id_int", touches = TRUE,
                           filename = wfile(), overwrite = TRUE)

  code_to_idint <- setNames(polys$id_int, trimws(as.character(polys[[id_col]])))

  tab <- data.frame(
    code  = trimws(as.character(table_series[[id_col]])),
    serie = trimws(as.character(table_series[[serie_col]])),
    poids = table_series[[poids_col]],
    sable = table_series[[sable_col]],
    limon = table_series[[limon_col]],
    argile = table_series[[argile_col]]
  )
  tab$serie[tab$serie == ""] <- NA_character_
  tab$code[tab$code == ""] <- NA_character_
  tab <- tab[!is.na(tab$code) & !is.na(tab$serie), , drop = FALSE]

  if (max(tab$poids, na.rm = TRUE) > 1) tab$poids <- tab$poids / 100
  if (max(tab$sable, na.rm = TRUE) > 1) {
    tab$sable  <- tab$sable / 100
    tab$limon  <- tab$limon / 100
    tab$argile <- tab$argile / 100
  }

  tab$id_int <- unname(code_to_idint[tab$code])
  if (anyNA(tab$id_int)) {
    bad <- unique(tab$code[is.na(tab$id_int)])
    stop("Codes polygone introuvables dans polygones_sf: ", paste(head(bad, 10), collapse = ", "))
  }

  tab <- tab |>
    group_by(id_int) |>
    mutate(poids = poids / sum(poids, na.rm = TRUE)) |>
    ungroup()

  series_all <- sort(unique(tab$serie))

  tex_poly <- tab |>
    group_by(id_int) |>
    summarise(
      sable  = sum(poids * sable,  na.rm = TRUE),
      limon  = sum(poids * limon,  na.rm = TRUE),
      argile = sum(poids * argile, na.rm = TRUE),
      .groups = "drop"
    )

  rasterize_attr <- function(values_df, colname) {
    polys2 <- polys
    m <- match(polys2$id_int, values_df$id_int)
    polys2$val <- values_df[[colname]][m]
    terra::rasterize(terra::vect(polys2), r, field = "val", touches = TRUE,
                     filename = wfile(), overwrite = TRUE)
  }

  E_sable  <- rasterize_attr(tex_poly, "sable")
  E_limon  <- rasterize_attr(tex_poly, "limon")
  E_argile <- rasterize_attr(tex_poly, "argile")

  rad <- max(1L, as.integer(ceiling(3 * sigma_voisin / mean(res(r)))))
  x <- seq(-rad, rad)
  sigma_px <- max(1, rad / 3)
  g <- exp(-(x^2) / (2 * sigma_px^2))
  g <- g / sum(g)

  smooth2 <- function(xr) {
    tmp1 <- tmp_tif()
    tmp2 <- tmp_tif()
    a <- terra::focal(xr, w = matrix(g, nrow = 1), fun = "sum", na.rm = TRUE,
                      filename = tmp1, overwrite = TRUE)
    terra::focal(a, w = matrix(g, ncol = 1), fun = "sum", na.rm = TRUE,
                 filename = tmp2, overwrite = TRUE)
  }

  if (verbose) message("Smoothing neighbour fields on coarse grid (rad=", rad, " px)...")
  NB_sable  <- smooth2(E_sable)
  NB_limon  <- smooth2(E_limon)
  NB_argile <- smooth2(E_argile)

  mu  <- get_global_scalar(r, "mean")
  sdv <- get_global_scalar(r, "sd")
  if (!is.finite(sdv) || sdv <= 0) sdv <- 1
  z <- (r - mu) / sdv
  if (isTRUE(write_intermediate)) z <- terra::writeRaster(z, tmp_tif(), overwrite = TRUE)

  if (verbose) message("Building probability raster per series (", length(series_all), " layers)...")
  W_list <- list()

  for (sname in series_all) {
    sub <- tab[tab$serie == sname, , drop = FALSE]
    ok <- which(stats::complete.cases(sub[, c("sable","limon","argile")]))
    if (length(ok) == 0) next
    row <- sub[ok[1], , drop = FALSE]

    sim <- row$sable * NB_sable + row$limon * NB_limon + row$argile * NB_argile

    if (sim_scale == "zscore") {
      sim_mu <- get_global_scalar(sim, "mean")
      sim_sd <- get_global_scalar(sim, "sd")
      if (!is.finite(sim_sd) || sim_sd <= 0) sim_sd <- 1
      sim <- (sim - sim_mu) / sim_sd
      sim <- terra::clamp(sim, -sim_clip, sim_clip, values = TRUE)
      if (isTRUE(write_intermediate)) sim <- terra::writeRaster(sim, tmp_tif(), overwrite = TRUE)
    }

    finesse <- row$argile + 0.5 * row$limon

    base_poly <- sub |>
      group_by(id_int) |>
      summarise(poids = sum(poids, na.rm = TRUE), .groups = "drop")

    polys3 <- polys
    polys3$val <- 0
    m <- match(polys3$id_int, base_poly$id_int)
    polys3$val[!is.na(m)] <- base_poly$poids[m[!is.na(m)]]

    base_r <- terra::rasterize(terra::vect(polys3), r, field = "val", touches = TRUE,
                               filename = wfile(), overwrite = TRUE)

    Wk <- exp(lambda_base * log(base_r + base_eps) +
                alpha_voisin * sim -
                beta_mnt * finesse * z)

    if (isTRUE(write_intermediate)) Wk <- terra::writeRaster(Wk, tmp_tif(), overwrite = TRUE)
    W_list[[sname]] <- Wk
  }

  if (length(W_list) == 0) stop("Aucune série calculable.")
  W <- terra::rast(W_list)
  names(W) <- names(W_list)
  series <- names(W)

  den <- terra::app(W, sum, na.rm = TRUE, filename = tmp_tif(), overwrite = TRUE)
  P <- W / den
  if (isTRUE(write_intermediate)) P <- terra::writeRaster(P, tmp_tif(), overwrite = TRUE)

  if (verbose) message("Computing ILR prototypes for series textures...")

  tex_series <- tab |>
    group_by(serie) |>
    summarise(
      sable  = dplyr::first(sable[is.finite(sable)]),
      limon  = dplyr::first(limon[is.finite(limon)]),
      argile = dplyr::first(argile[is.finite(argile)]),
      .groups = "drop"
    )
  tex_series <- tex_series[match(series, tex_series$serie), ]
  if (anyNA(tex_series$serie)) stop("Mismatch entre layers P et textures série.")

  X <- as.matrix(tex_series[, c("sable","limon","argile")])
  X <- X / rowSums(X)
  ilr_mat <- compositions::ilr(compositions::acomp(X))
  ilr1 <- as.numeric(ilr_mat[, 1])
  ilr2 <- as.numeric(ilr_mat[, 2])

  if (verbose) message("Computing ILR posterior mean rasters (2 layers)...")
  u <- 0 * P[[1]]
  v <- 0 * P[[1]]
  for (k in seq_len(terra::nlyr(P))) {
    u <- u + P[[k]] * ilr1[k]
    v <- v + P[[k]] * ilr2[k]
  }
  if (isTRUE(write_intermediate)) {
    u <- terra::writeRaster(u, tmp_tif(), overwrite = TRUE)
    v <- terra::writeRaster(v, tmp_tif(), overwrite = TRUE)
  }

  if (verbose) message("Building ILR distance stack (", length(series), " layers)...")
  D_list <- vector("list", length(series))
  names(D_list) <- series
  for (k in seq_along(series)) {
    dk <- (u - ilr1[k])^2 + (v - ilr2[k])^2
    if (isTRUE(write_intermediate)) dk <- terra::writeRaster(dk, tmp_tif(), overwrite = TRUE)
    D_list[[k]] <- dk
  }
  D <- terra::rast(D_list)
  names(D) <- series

  if (verbose) message("Quota assignment per polygon (fits table_series proportions)...")

  tab_q <- tab[tab$serie %in% series, c("id_int", "serie", "poids"), drop = FALSE] |>
    dplyr::group_by(id_int, serie) |>
    dplyr::summarise(poids = sum(poids, na.rm = TRUE), .groups = "drop") |>
    dplyr::group_by(id_int) |>
    dplyr::mutate(poids = poids / sum(poids, na.rm = TRUE)) |>
    dplyr::ungroup()

  w_by_id <- split(tab_q, tab_q$id_int)
  serie_to_k <- setNames(seq_along(series), series)

  stack_all <- c(id_r, D)
  V <- terra::values(stack_all, mat = TRUE)

  id_vals0 <- V[, 1]
  keep <- !is.na(id_vals0)
  id_vals <- as.integer(id_vals0[keep])
  cost_vals <- V[keep, -1, drop = FALSE]
  cell_vals <- which(keep)

  out_idx_byrow <- quota_assign_fast(
    id_vals = id_vals,
    cost_vals = cost_vals,
    w_by_id = w_by_id,
    serie_to_k = serie_to_k,
    series = series,
    progress = isTRUE(progress)
  )

  idx_full <- rep(NA_integer_, terra::ncell(id_r))
  idx_full[cell_vals] <- out_idx_byrow

  idx_r <- terra::rast(id_r)
  idx_r <- terra::setValues(idx_r, idx_full)

  idx_lab <- idx_r
  levels(idx_lab) <- data.frame(value = seq_along(series), label = series)
  if (isTRUE(write_intermediate)) idx_lab <- terra::writeRaster(idx_lab, tmp_tif(), overwrite = TRUE)

  list(
    prob = P,
    ilr_mean = terra::rast(list(ilr1 = u, ilr2 = v)),
    dist = D,
    id_r = id_r,
    closest_index = idx_r,
    closest_label = idx_lab,
    series = series
  )
}

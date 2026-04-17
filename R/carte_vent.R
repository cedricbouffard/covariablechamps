#' Visualiser les distances amont/aval avec les flèches de vent
#'
#' @param distances Résultat de calculer_distances_vent()
#' @param type Type: "amont", "aval", "total" ou "les_deux"
#' @param titre Titre du graphique
#' @param n_fleches Nombre de flèches
#' @param taille_fleches Taille des flèches
#'
#' @return Un objet ggplot2
#' @export
#'
#' @examples
#' \dontrun{
#' carte <- tracer_carte_vent(distances, type = "amont")
#' print(carte)
#' }
#'
tracer_carte_vent <- function(distances,
                              type = c("les_deux", "amont", "aval", "total"),
                              titre = NULL,
                              n_fleches = 5,
                              taille_fleches = 0.2) {

  if (!requireNamespace("ggplot2", quietly = TRUE))
    stop("Le package 'ggplot2' est requis.")
  if (!requireNamespace("terra", quietly = TRUE))
    stop("Le package 'terra' est requis.")

  type <- match.arg(type)

  # Palette selon le type
  if (type == "amont") {
    palette <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6",
                 "#4292c6", "#2171b5", "#08519c", "#08306b")
    legend_title <- "Distance amont (m)"
    if (is.null(titre)) titre <- "Distance aux arbres en amont (contre le vent)"
  } else if (type == "aval") {
    palette <- c("#fff5f0", "#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a",
                 "#ef3b2c", "#cb181d", "#a50f15", "#67000d")
    legend_title <- "Distance aval (m)"
    if (is.null(titre)) titre <- "Distance aux arbres en aval (sous le vent)"
  } else if (type == "total") {
    palette <- viridis::viridis(100)
    legend_title <- "Distance (m)"
    if (is.null(titre)) titre <- "Distance aux arbres les plus proches"
  } else {
    palette <- c("#2166ac", "#4393c3", "#92c5de", "#d1e5f0", "#f7f7f7",
                 "#fddbc7", "#f4a582", "#d6604b", "#b2182b")
    legend_title <- "Distance (m)"
    if (is.null(titre)) titre <- "Distances amont (-) et aval (+)"
  }

  # Préparer les données
  raster_to_df <- function(r, col_name) {
    df <- as.data.frame(r, xy = TRUE)
    names(df)[3] <- col_name
    df
  }

  if (type == "amont") {
    df_plot <- raster_to_df(distances$distance_amont, "distance")
    df_plot <- df_plot[!is.na(df_plot$distance), ]
    max_val <- max(terra::values(distances$distance_totale), na.rm = TRUE)
  } else if (type == "aval") {
    df_plot <- raster_to_df(distances$distance_aval, "distance")
    df_plot <- df_plot[!is.na(df_plot$distance), ]
    max_val <- max(terra::values(distances$distance_totale), na.rm = TRUE)
  } else if (type == "total") {
    df_plot <- raster_to_df(distances$distance_totale, "distance")
    df_plot$distance[is.na(df_plot$distance)] <- 0
    max_val <- max(terra::values(distances$distance_totale), na.rm = TRUE)
  } else {
    df_amont <- raster_to_df(distances$distance_amont, "distance")
    df_aval <- raster_to_df(distances$distance_aval, "distance")
    df_amont <- df_amont[!is.na(df_amont$distance), ]
    df_aval <- df_aval[!is.na(df_aval$distance), ]
    df_amont$distance <- -df_amont$distance
    df_plot <- rbind(df_amont, df_aval)
    max_val <- max(terra::values(distances$distance_totale), na.rm = TRUE)
  }

  # Étendue
  ext <- terra::ext(distances$distance_totale)
  x_range <- ext[2] - ext[1]
  y_range <- ext[4] - ext[3]

  # Créer le graphique
  p <- ggplot2::ggplot()

  # Raster
  p <- p + ggplot2::geom_raster(
    data = df_plot,
    ggplot2::aes(x = x, y = y, fill = distance)
  )

  # Palette
  if (type == "les_deux") {
    p <- p + ggplot2::scale_fill_gradientn(
      colors = palette, name = legend_title,
      limits = c(-max_val, max_val)
    )
  } else {
    p <- p + ggplot2::scale_fill_gradientn(
      colors = palette, name = legend_title,
      limits = c(0, max_val)
    )
  }

  # Flèches de vent
  angle_rad <- (90 - distances$angle_vent) * pi / 180
  u <- cos(angle_rad) * taille_fleches * x_range
  v <- sin(angle_rad) * taille_fleches * y_range

  x_pos <- seq(ext[1] + 0.15 * x_range, ext[2] - 0.15 * x_range, length.out = n_fleches)
  y_pos <- rep(ext[4] - 0.08 * y_range, n_fleches)

  arrows_df <- data.frame(
    x = x_pos - u/2, y = y_pos - v/2,
    xend = x_pos + u/2, yend = y_pos + v/2
  )

  p <- p + ggplot2::geom_segment(
    data = arrows_df,
    ggplot2::aes(x = x, y = y, xend = xend, yend = yend),
    arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm")),
    color = "darkblue", linewidth = 1.5
  ) + ggplot2::annotate(
    "text", x = ext[1] + 0.02 * x_range, y = ext[4] - 0.02 * y_range,
    label = paste0("Vent: ", round(distances$angle_vent, 0), "°"),
    hjust = 0, fontface = "bold", size = 4
  )

  # Configuration
  p + ggplot2::coord_fixed(ratio = 1) +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = titre, subtitle = paste0("Ouverture: ±", distances$ouverture_angulaire, "°"),
                  x = "X (m)", y = "Y (m)") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(face = "bold", size = 14),
      legend.position = "right",
      axis.text = ggplot2::element_blank()
    )
}

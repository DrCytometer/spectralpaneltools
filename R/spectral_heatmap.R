# spectral_heatmap.r


#' @title Spectral Heatmap
#'
#' @description This function plots a spectral matrix as a heatmap and saves it
#'     as a JPEG file.
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c
#' @importFrom ggplot2 coord_fixed element_text labs ggsave theme theme_classic
#' @importFrom tidyr pivot_longer
#' @importFrom dplyr mutate %>%
#'
#' @param spectra Matrix or dataframe containing spectral data (fluorophores x detectors).
#' @param file.name Character. Output file name. Default is "Spectral Heatmap.jpg".
#'     Path may be supplied.
#' @param legend.label Character string that will appear on the heatmap legend.
#'
#' @return Saves the heatmap plot as a JPEG file in the specified directory.
#'
#' @export

spectral.heatmap <- function( spectra, file.name = "Spectral Heatmap.jpg",
                              legend.label = "Intensity" ) {

  heatmap.df <- data.frame( spectra, check.names = FALSE )

  plot.width <- ( ncol( heatmap.df ) - 1 ) / 64 * 12
  plot.height <- 5 + round( nrow( heatmap.df ) / 8, 0 )

  row.levels <- rownames( heatmap.df )
  col.levels <- colnames( heatmap.df )
  heatmap.df$Fluorophore <- row.levels

  heatmap.long <- heatmap.df %>%
    tidyr::pivot_longer( cols = -Fluorophore, names_to = "Detector", values_to = "value" ) %>%
    dplyr::mutate( Fluorophore = factor( Fluorophore, levels = rev( row.levels ) ),
            Detector = factor( Detector, levels = col.levels ) )

  heatmap.plot <- ggplot( heatmap.long, aes( Detector, Fluorophore, fill = value ) ) +
    geom_tile() +
    theme_classic() +
    coord_fixed( ratio = 1 ) +
    theme( axis.text.x = element_text( angle = 45, hjust = 1 ) ) +
    labs( x = NULL, y = NULL, fill = legend.label )+
    scale_fill_viridis_c()

  ggsave( filename = file.name,
    plot = heatmap.plot,
    width = plot.width, height = plot.height )
}

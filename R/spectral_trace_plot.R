# spectral_trace_plot.r


#' @title Spectral Trace Plot
#'
#' @description This function plots the fluorophore spectra and saves the plots
#'     as JPEG files.
#'
#' @importFrom ggplot2 ggplot aes geom_path geom_point labs theme_minimal
#' @importFrom ggplot2 element_text ggsave theme
#' @importFrom tidyr pivot_longer
#'
#' @param spectra Matrix containing spectral data. Fluorophores in rows,
#'     detectors in columns. The matrix should be normalized to peak emission.
#'     The fluorophore names should be the rownames of the matrix, and the only
#'     information in the matrix should be the normalized emission for each
#'     fluorescent detector channel.
#' @param plot.title Character. Title for the plot.
#' @param file.name Character. Name for the output file. Path may be supplied.
#' @param line.width Numeric. Width of the traces on the plot. Default is 1.
#' @param point.size Numeric. Size of the points on the traces at each detector.
#'     Default is 1.
#'
#' @return Saves the plot as JPEG (or other) file.
#' @export


spectral.trace.plot <- function( spectra,
                         plot.title = "Fluorophore Spectra",
                         file.name = "Spectra Plot.jpg",
                         line.width = 1,
                         point.size = 1 ){

  fluor.spectra.plotting <- data.frame( spectra, check.names = FALSE )
  fluor.spectra.plotting$Fluorophore <- rownames( fluor.spectra.plotting )

  plot.width <- ( ncol( fluor.spectra.plotting ) - 1 ) / 64 * 12
  plot.height <- 5 + round( nrow( fluor.spectra.plotting ) / 8, 0 )

  fluor.spectra.long <- tidyr::pivot_longer( fluor.spectra.plotting, -Fluorophore,
                                      names_to = "Detector",
                                      values_to = "Intensity" )

  fluor.spectra.long$Detector <-  factor( fluor.spectra.long$Detector,
                                         levels = unique( fluor.spectra.long$Detector ),
                                         ordered = TRUE )

  spectra.plot <- ggplot( fluor.spectra.long,
                         aes( x = Detector, y = Intensity,
                             group = Fluorophore, color = Fluorophore ) ) +
    geom_path( linewidth = line.width ) +
    geom_point( size = point.size ) +
    labs( title = plot.title,
         x = "Detector",
         y = "Normalized Intensity" ) +
    theme_minimal() +
    theme( axis.text.x = element_text( angle = 45, hjust = 1 )  ) +
    theme( legend.position = "bottom" )

  ggsave( file.name,
          spectra.plot,
          width = plot.width, height = plot.height,
          limitsize = FALSE )
}

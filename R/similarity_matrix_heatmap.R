# similarity_matrix_heatmap.r

#' @title Similarity Matrix Heatmap
#'
#' @description This function plots the similarity matrix as a heatmap and saves
#' it as a JPEG file. It also calculates and displays the complexity index.
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_viridis_c theme_minimal
#' @importFrom ggplot2 coord_fixed element_text labs ggsave
#' @importFrom dplyr mutate %>%
#' @importFrom tidyr pivot_longer
#'
#' @param spectra Data frame containing spectral data. Fluorophores in rows,
#' detectors in columns.
#' @param file.name Character string. Default is `Similarity Matrix.jpg`.
#' Provide a file path to save somewhere other than the working directory.
#' @param figure.width Numeric. Default is `8`
#' @param figure.height Numeric. Default is `6`
#'
#' @return Saves the heatmap plot as a JPEG file in the working directory or in
#' the path provided as an argument to `file.name`.
#'
#' @export

similarity.matrix.heatmap <- function( spectra,
                                       file.name = "Similarity Matrix.jpg",
                                       figure.width = 8, figure.height = 6 ){

  # similarity matrix
  similarity.matrix <- calculate.similarity.matrix( t( spectra ) )

  complexity.index <- calculate.complexity.index( spectra )

  similarity.df <- as.data.frame( similarity.matrix )

  # rearrange
  similarity.df <- similarity.df %>%
    mutate( Fluor1 = factor( rownames( similarity.df ),
                             levels = rownames( similarity.df ) ) ) %>%
    tidyr::pivot_longer( cols = -Fluor1, names_to = "Fluor2", values_to = "value" ) %>%
    mutate( Fluor2 = factor( Fluor2, levels = rev( colnames( similarity.matrix ) ) ) )

  # plot
  similarity.heatmap <- ggplot( similarity.df, aes( Fluor1, Fluor2, fill = value ) ) +
    geom_tile() +
    scale_fill_viridis_c() +
    theme_minimal() +
    coord_fixed( ratio = 1 ) +
    theme( axis.text.x = element_text( angle = 90, hjust = 1 ) ) +
    labs( x = paste( "Complexity Index", complexity.index ),
          y = NULL, fill = "Cosine Similarity" )

  ggsave( filename = file.name,
          plot = similarity.heatmap,
          width = figure.width, height = figure.height )

}

# calculate_similarity_matrix.r

#' @title Calculate Similarity Matrix
#' @description
#' Calculates the similarity matrix (set of cosine similarity values)
#'     for a set of fluorophores (a spectral panel).
#'
#' @param spectra Spectral signatures of fluorophores, normalized between 0 and 1,
#' with fluorophores in rows and detectors (channels) in columns. More than one
#' spectra are needed to calculate anything here. Matrix or dataframe.
#' @return The similarity matrix for the spectral flow cytometry panel.
#' @export

calculate.similarity.matrix <- function( spectra ) {
  spectra.t <- as.matrix( t( spectra ) )
  euclidean.norm <- sqrt( colSums( spectra.t^2 ) )
  dot.product <- t( spectra.t ) %*% spectra.t
  similarity.matrix <- dot.product / outer( euclidean.norm, euclidean.norm )
  return( similarity.matrix )
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spectral_trace_plot.R
\name{spectral.trace.plot}
\alias{spectral.trace.plot}
\title{Spectral Trace Plot}
\usage{
spectral.trace.plot(
  spectra,
  plot.title = "Fluorophore Spectra",
  file.name = "Spectra Plot.jpg",
  line.width = 1,
  point.size = 1
)
}
\arguments{
\item{spectra}{Matrix containing spectral data. Fluorophores in rows,
detectors in columns. The matrix should be normalized to peak emission.
The fluorophore names should be the rownames of the matrix, and the only
information in the matrix should be the normalized emission for each
fluorescent detector channel.}

\item{plot.title}{Character. Title for the plot.
Default is \verb{Fluorophore Spectra}.}

\item{file.name}{Character. Name for the output file. Path may be supplied.
Default is \verb{Spectra Plot.jpg}.}

\item{line.width}{Numeric. Width of the traces on the plot. Default is \code{1}.}

\item{point.size}{Numeric. Size of the points on the traces at each detector.
Default is \code{1}.}
}
\value{
Saves the plot as JPEG (or other) file.
}
\description{
This function plots the fluorophore spectra and saves the plots
as JPEG files.
}

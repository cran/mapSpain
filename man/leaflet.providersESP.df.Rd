% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\encoding{UTF-8}
\name{leaflet.providersESP.df}
\alias{leaflet.providersESP.df}
\title{(Superseded) Database of public WMS and WMTS of Spain}
\format{
A \code{data.frame} object with a list of the required parameters for calling
the service:
\describe{
\item{provider}{Provider name}.
\item{field}{Description of \code{value}}.
\item{value}{INE code of each province}.
}
}
\source{
\url{https://dieghernan.github.io/leaflet-providersESP/} leaflet plugin,
\strong{v1.3.3}.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}}

This \code{data.frame} is not longer in use by \CRANpkg{mapSpain}. See
\link{esp_tiles_providers} instead.

A \code{data.frame} containing information of different public WMS and WMTS
providers of Spain
}
\examples{
data("leaflet.providersESP.df")
}
\keyword{internal}

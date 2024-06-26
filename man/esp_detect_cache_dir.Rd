% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esp_cache.R
\name{esp_detect_cache_dir}
\alias{esp_detect_cache_dir}
\title{Detect cache dir for \CRANpkg{mapSpain}}
\usage{
esp_detect_cache_dir(x = NULL)
}
\arguments{
\item{x}{Ignored.}
}
\value{
A character with the path to your \code{cache_dir}.
}
\description{
Helper function to detect the current cache folder. See
\code{\link[=esp_set_cache_dir]{esp_set_cache_dir()}}.
}
\examples{
esp_detect_cache_dir()

}
\seealso{
Other cache utilities: 
\code{\link{esp_clear_cache}()},
\code{\link{esp_set_cache_dir}()}
}
\concept{cache utilities}

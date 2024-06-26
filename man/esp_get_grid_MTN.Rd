% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esp_get_grid_MTN.R
\name{esp_get_grid_MTN}
\alias{esp_get_grid_MTN}
\title{Get \code{\link[sf:sf]{sf}} \code{POLYGON} of the national geographic grids from IGN}
\source{
IGN data via a custom CDN (see
\url{https://github.com/rOpenSpain/mapSpain/tree/sianedata/MTN}).
}
\usage{
esp_get_grid_MTN(
  grid = "MTN25_ETRS89_Peninsula_Baleares_Canarias",
  update_cache = FALSE,
  cache_dir = NULL,
  verbose = FALSE
)
}
\arguments{
\item{grid}{Name of the grid to be loaded. See \strong{Details}.}

\item{update_cache}{A logical whether to update cache. Default is \code{FALSE}.
When set to \code{TRUE} it would force a fresh download of the source file.}

\item{cache_dir}{A path to a cache directory. See \strong{About caching}.}

\item{verbose}{Logical, displays information. Useful for debugging,
default is \code{FALSE}.}
}
\value{
A \code{\link[sf:sf]{sf}} \code{POLYGON}.
}
\description{
Loads a \code{\link[sf:sf]{sf}} \code{POLYGON} with the geographic grids of Spain.
}
\details{
Metadata available on
\url{https://github.com/rOpenSpain/mapSpain/tree/sianedata/MTN}.

Possible values of \code{grid} are:\tabular{l}{
   \strong{grid_name} \cr
   MTN25_ED50_Peninsula_Baleares \cr
   MTN25_ETRS89_ceuta_melilla_alboran \cr
   MTN25_ETRS89_Peninsula_Baleares_Canarias \cr
   MTN25_RegCan95_Canarias \cr
   MTN50_ED50_Peninsula_Baleares \cr
   MTN50_ETRS89_Peninsula_Baleares_Canarias \cr
   MTN50_RegCan95_Canarias \cr
}

\subsection{MTN Grids}{

A description of the MTN (Mapa Topografico Nacional) grids available:

\strong{MTN25_ED50_Peninsula_Baleares}

MTN25 grid corresponding to the Peninsula and Balearic Islands, in ED50 and
geographical coordinates (longitude, latitude) This is the real MTN25 grid,
that is, the one that divides the current printed series of the map, taking
into account special sheets and irregularities.

\strong{MTN50_ED50_Peninsula_Baleares}

MTN50 grid corresponding to the Peninsula and Balearic Islands, in ED50 and
geographical coordinates (longitude, latitude) This is the real MTN50 grid,
that is, the one that divides the current printed series of the map, taking
into account special sheets and irregularities.

\strong{MTN25_ETRS89_ceuta_melilla_alboran}

MTN25 grid corresponding to Ceuta, Melilla, Alboran and Spanish territories
in North Africa, adjusted to the new official geodetic reference system
ETRS89, in geographical coordinates (longitude, latitude).

\strong{MTN25_ETRS89_Peninsula_Baleares_Canarias}

MTN25 real grid corresponding to the Peninsula, the Balearic Islands and the
Canary Islands, adjusted to the new ETRS89 official reference geodetic
system, in geographical coordinates (longitude, latitude).

\strong{MTN50_ETRS89_Peninsula_Baleares_Canarias}

MTN50 real grid corresponding to the Peninsula, the Balearic Islands and the
Canary Islands, adjusted to the new ETRS89 official reference geodetic
system, in geographical coordinates (longitude, latitude).

\strong{MTN25_RegCan95_Canarias}

MTN25 grid corresponding to the Canary Islands, in REGCAN95 (WGS84
compatible) and geographic coordinates (longitude, latitude). It is the real
MTN25 grid, that is, the one that divides the current printed series of the
map, taking into account the special distribution of the Canary Islands
sheets.

\strong{MTN50_RegCan95_Canarias}

MTN50 grid corresponding to the Canary Islands, in REGCAN95 (WGS84
compatible) and geographic coordinates (longitude, latitude). This is the
real grid of the MTN50, that is, the one that divides the current printed
series of the map, taking into account the special distribution of the
Canary Islands sheets.
}
}
\section{About caching}{
You can set your \code{cache_dir} with \code{\link[=esp_set_cache_dir]{esp_set_cache_dir()}}.

Sometimes cached files may be corrupt. On that case, try re-downloading
the data setting \code{update_cache = TRUE}.

If you experience any problem on download, try to download the
corresponding .geojson file by any other method and save it on your
\code{cache_dir}. Use the option \code{verbose = TRUE} for debugging the API query.
}

\examples{
\dontshow{if (esp_check_access()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
grid <- esp_get_grid_MTN(grid = "MTN50_ETRS89_Peninsula_Baleares_Canarias")

library(ggplot2)

ggplot(grid) +
  geom_sf() +
  theme_light() +
  labs(title = "MTN50 Grid for Spain")
}
\dontshow{\}) # examplesIf}
}
\seealso{
Other grids: 
\code{\link{esp_get_grid_BDN}()},
\code{\link{esp_get_grid_EEA}()},
\code{\link{esp_get_grid_ESDAC}()}
}
\concept{grids}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esp_get_rivers.R
\name{esp_get_rivers}
\alias{esp_get_rivers}
\title{Get \code{\link[sf:sf]{sf}} \code{POLYGON} or \code{LINESTRING} of rivers, channels and other
wetlands of Spain}
\source{
IGN data via a custom CDN (see
\url{https://github.com/rOpenSpain/mapSpain/tree/sianedata}).
}
\usage{
esp_get_rivers(
  epsg = "4258",
  cache = TRUE,
  update_cache = FALSE,
  cache_dir = NULL,
  verbose = FALSE,
  resolution = "3",
  spatialtype = "line",
  name = NULL
)
}
\arguments{
\item{epsg}{projection of the map: 4-digit \href{https://epsg.io/}{EPSG code}.
One of:
\itemize{
\item \code{"4258"}: ETRS89.
\item \code{"4326"}: WGS84.
\item \code{"3035"}: ETRS89 / ETRS-LAEA.
\item \code{"3857"}: Pseudo-Mercator.
}}

\item{cache}{A logical whether to do caching. Default is \code{TRUE}. See
\strong{About caching}.}

\item{update_cache}{A logical whether to update cache. Default is \code{FALSE}.
When set to \code{TRUE} it would force a fresh download of the source file.}

\item{cache_dir}{A path to a cache directory. See \strong{About caching}.}

\item{verbose}{Logical, displays information. Useful for debugging,
default is \code{FALSE}.}

\item{resolution}{Resolution of the \code{POLYGON}. Values available are
\code{"3"}, \code{"6.5"} or \code{"10"}.}

\item{spatialtype}{Spatial type of the output. Use \code{"area"} for \code{POLYGON} or
\code{"line"} for \code{LINESTRING}.}

\item{name}{Optional. A character or  \code{\link[base:grep]{regex}} expression
with the name of the element(s) to be extracted.}
}
\value{
A \code{\link[sf:sf]{sf}} \code{POLYGON} or \code{LINESTRING} object.
}
\description{
Loads a \code{\link[sf:sf]{sf}} \code{POLYGON} or \code{LINESTRING} object representing
rivers, channels, reservoirs and other wetlands of Spain.
}
\details{
Metadata available on
\url{https://github.com/rOpenSpain/mapSpain/tree/sianedata/}.
}
\examples{
\dontshow{if (esp_check_access()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
\donttest{
# Use of regex

regex1 <- esp_get_rivers(name = "Tajo|Segura")
unique(regex1$rotulo)


regex2 <- esp_get_rivers(name = "Tajo$| Segura")
unique(regex2$rotulo)

# See the diference

# Rivers in Spain
shapeEsp <- esp_get_country(moveCAN = FALSE)

MainRivers <-
  esp_get_rivers(name = "Tajo$|Ebro$|Ebre$|Duero|Guadiana$|Guadalquivir")

sf::st_bbox(MainRivers)
library(ggplot2)

ggplot(shapeEsp) +
  geom_sf() +
  geom_sf(data = MainRivers, color = "skyblue", linewidth = 2) +
  coord_sf(
    xlim = c(-7.5, 1),
    ylim = c(36.8, 43)
  ) +
  theme_void()


# Wetlands in South-West Andalucia
and <- esp_get_prov(c("Huelva", "Sevilla", "Cadiz"))
Wetlands <- esp_get_rivers(spatialtype = "area")

ggplot(and) +
  geom_sf() +
  geom_sf(
    data = Wetlands, fill = "skyblue",
    color = "skyblue", alpha = 0.5
  ) +
  coord_sf(
    xlim = c(-7.5, -4.5),
    ylim = c(36, 38.5)
  ) +
  theme_void()
}
\dontshow{\}) # examplesIf}
}
\seealso{
Other natural: 
\code{\link{esp_get_hydrobasin}()},
\code{\link{esp_get_hypsobath}()}
}
\concept{natural}

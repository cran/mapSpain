% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esp_get_can_box.R
\name{esp_get_can_box}
\alias{esp_get_can_box}
\alias{esp_get_can_provinces}
\title{Get complementary lines when plotting Canary Islands.}
\source{
\code{esp_get_can_provinces} extracted from CartoBase ANE,
\code{se89_mult_admin_provcan_l.shp} file.
}
\usage{
esp_get_can_box(style = "right", moveCAN = TRUE, epsg = "4258")

esp_get_can_provinces(moveCAN = TRUE, epsg = "4258")
}
\arguments{
\item{style}{Style of line around Canary Islands. Four options available:
"left", "right", "box" or "poly".}

\item{moveCAN}{A logical \code{TRUE/FALSE} or a vector of coordinates
\code{c(lat, lon)}. It places the Canary Islands close to Spain's mainland.
Initial position can be adjusted using the vector of coordinates.}

\item{epsg}{projection of the map: 4-digit \href{https://epsg.io/}{EPSG code}.
One of:
\itemize{
\item "4258": ETRS89
\item "4326": WGS84
\item "3035": ETRS89 / ETRS-LAEA
\item "3857": Pseudo-Mercator
}}
}
\value{
A \code{LINESTRING} or \code{POLYGON} object if \code{style = "poly"}.

\code{esp_get_can_provinces} returns a \code{LINESTRING} object.
}
\description{
When plotting Spain, it is usual to represent the Canary Islands as an inset
(see \code{moveCAN} on \code{\link[=esp_get_nuts]{esp_get_nuts()}}. These functions provides complementary
borders when Canary Islands are displaced.

\code{esp_get_can_box} is used to draw lines around the displaced Canary Islands.

\code{esp_get_can_provinces} is used to draw a separator line between the two
provinces of the Canary Islands.
}
\examples{
library(sf)

Provs <- esp_get_prov()
Box <- esp_get_can_box()
Line <- esp_get_can_provinces()


plot(st_geometry(Provs), col = hcl.colors(4, palette = "Grays"))
plot(Box, add = TRUE)
plot(Line, add = TRUE)


# Displacing Canary

Provs_D <- esp_get_prov(moveCAN = c(15, 0))
Box_D <- esp_get_can_box(style = "left", moveCAN = c(15, 0))
Line_D <- esp_get_can_provinces(moveCAN = c(15, 0))



plot(st_geometry(Provs_D), col = hcl.colors(4, palette = "Grays"))
plot(Box_D, add = TRUE)
plot(Line_D, add = TRUE)

# Example with poly option

library(giscoR)

Countries <-
  gisco_get_countries(
    res = "20",
    epsg = "4326",
    region = c("Europe", "Africa")
  )
CANbox <-
  esp_get_can_box(
    style = "poly",
    epsg = "4326",
    moveCAN = c(12.5, 0)
  )
CCAA <- esp_get_ccaa(
  res = "20",
  epsg = "4326",
  moveCAN = c(12.5, 0)
)


plot_sf(CCAA, axes = TRUE)
plot(st_geometry(Countries), col = "grey80", add = TRUE)
plot(st_geometry(CANbox),
  border = "black",
  col = "white",
  add = TRUE
)
plot(st_geometry(CCAA), add = TRUE, col = "beige")
box()
}
\seealso{
\code{\link[=esp_get_nuts]{esp_get_nuts()}}, \code{\link[=esp_get_ccaa]{esp_get_ccaa()}}.
}
\author{
dieghernan, \url{https://github.com/dieghernan/}
}
\concept{political}

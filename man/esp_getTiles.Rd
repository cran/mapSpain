% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esp_getTiles.R
\name{esp_getTiles}
\alias{esp_getTiles}
\title{Get static tiles from public administrations of Spain}
\source{
\url{https://dieghernan.github.io/leaflet-providersESP/} leaflet plugin,
\strong{v1.3.3}.
}
\usage{
esp_getTiles(
  x,
  type = "IDErioja",
  zoom = NULL,
  zoommin = 0,
  crop = TRUE,
  res = 512,
  bbox_expand = 0.05,
  transparent = TRUE,
  mask = FALSE,
  update_cache = FALSE,
  cache_dir = NULL,
  verbose = FALSE,
  options = NULL
)
}
\arguments{
\item{x}{An \code{\link[sf:sf]{sf}} or \code{\link[sf:sfc]{sfc}} object.}

\item{type}{This parameter could be either:
\itemize{
\item The name of one of the  pre-defined providers
(see \code{\link[=esp_tiles_providers]{esp_tiles_providers()}}).
\item A list with two named elements \code{id} and \code{q} with your own parameters.
See \code{\link[=esp_make_provider]{esp_make_provider()}} and examples.
}}

\item{zoom}{Zoom level. If \code{NULL}, it is determined automatically. If set,
it overrides \code{zoommin}. Only valid for WMTS tiles. On a single point it
applies a buffer to the point and on \code{zoom = NULL} the function set a zoom
level of 18. See \strong{Details}.}

\item{zoommin}{Delta on default \code{zoom}. The default value is designed to
download fewer tiles than you probably want. Use \code{1} or \code{2} to
increase the resolution.}

\item{crop}{\code{TRUE} if results should be cropped to the specified \code{x} extent,
\code{FALSE} otherwise. If \code{x} is an \code{\link[sf:sf]{sf}} object with one \code{POINT},
\code{crop} is set to \code{FALSE}.}

\item{res}{Resolution (in pixels) of the final tile. Only valid for WMS.}

\item{bbox_expand}{A numeric value that indicates the expansion percentage
of the bounding box of \code{x}.}

\item{transparent}{Logical. Provides transparent background, if supported.
Depends on the selected provider on \code{type}.}

\item{mask}{\code{TRUE} if the result should be masked to \code{x}.}

\item{update_cache}{A logical whether to update cache. Default is \code{FALSE}.
When set to \code{TRUE} it would force a fresh download of the source file.}

\item{cache_dir}{A path to a cache directory. See \strong{About caching}.}

\item{verbose}{Logical, displays information. Useful for debugging,
default is \code{FALSE}.}

\item{options}{A named list containing additional options to pass to the
query.}
}
\value{
A \code{SpatRaster} is returned, with 3 (RGB) or 4 (RGBA) layers, depending on
the provider. See \code{\link[terra:rast]{terra::rast()}}.
.
}
\description{
Get static map tiles based on a spatial object. Maps can be fetched from
various open map servers.

This function is a implementation of the javascript plugin
\href{https://dieghernan.github.io/leaflet-providersESP/}{leaflet-providersESP}
\strong{v1.3.3}.
}
\details{
Zoom levels are described on the
\href{https://wiki.openstreetmap.org/wiki/Zoom_levels}{OpenStreetMap wiki}:\tabular{rl}{
   zoom \tab area to represent \cr
   0 \tab whole world \cr
   3 \tab large country \cr
   5 \tab state \cr
   8 \tab county \cr
   10 \tab metropolitan area \cr
   11 \tab city \cr
   13 \tab village or suburb \cr
   16 \tab streets \cr
   18 \tab some buildings, trees \cr
}


For a complete list of providers see \link{esp_tiles_providers}.

Most WMS/WMTS providers provide tiles on "EPSG:3857". In case that the tile
looks deformed, try projecting first \code{x}:

\code{x <- sf::st_transform(x, 3857)}
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
\dontrun{
# This script downloads tiles to your local machine
# Run only if you are online

segovia <- esp_get_prov_siane("segovia", epsg = 3857)
tile <- esp_getTiles(segovia, "IGNBase.Todo")

library(ggplot2)
library(tidyterra)

ggplot(segovia) +
  geom_spatraster_rgb(data = tile, maxcell = Inf) +
  geom_sf(fill = NA)

# Another provider

tile2 <- esp_getTiles(segovia, type = "MDT")

ggplot(segovia) +
  geom_spatraster_rgb(data = tile2, maxcell = Inf) +
  geom_sf(fill = NA)

# A custom WMS provided

custom_wms <- esp_make_provider(
  id = "an_id_for_caching",
  q = "https://idecyl.jcyl.es/geoserver/ge/wms?",
  service = "WMS",
  version = "1.3.0",
  format = "image/png",
  layers = "geolog_cyl_litologia"
)

custom_wms_tile <- esp_getTiles(segovia, custom_wms)

autoplot(custom_wms_tile, maxcell = Inf) +
  geom_sf(data = segovia, fill = NA, color = "red")

# A custom WMTS provider

custom_wmts <- esp_make_provider(
  id = "cyl_wmts",
  q = "https://www.ign.es/wmts/pnoa-ma?",
  service = "WMTS",
  layer = "OI.OrthoimageCoverage"
)

custom_wmts_tile <- esp_getTiles(segovia, custom_wmts)

autoplot(custom_wmts_tile, maxcell = Inf) +
  geom_sf(data = segovia, fill = NA, color = "white", linewidth = 2)

# Example from https://leaflet-extras.github.io/leaflet-providers/preview/
cartodb_voyager <- list(
  id = "CartoDB_Voyager",
  q = "https://a.basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png"
)
cartodb <- esp_getTiles(segovia, cartodb_voyager, zoommin = 1)

autoplot(cartodb, maxcell = Inf) +
  geom_sf(data = segovia, fill = NA, color = "black", linewidth = 1)
}
}
\seealso{
\code{\link[terra:rast]{terra::rast()}}.

Other imagery utilities: 
\code{\link{addProviderEspTiles}()},
\code{\link{esp_make_provider}()},
\code{\link{esp_tiles_providers}}
}
\concept{imagery utilities}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\encoding{UTF-8}
\name{esp_nuts.sf}
\alias{esp_nuts.sf}
\title{All NUTS \code{POLYGON} object of Spain}
\format{
A \code{POLYGON} data frame (resolution: 1:1million, EPSG:4258) object with 86
rows and fields:
\itemize{
\item COAST_TYPE: COAST_TYPE
\item FID: FID
\item NUTS_NAME: NUTS name on local alphabet
\item MOUNT_TYPE: MOUNT_TYPE
\item NAME_LATN: Name on Latin characters
\item CNTR_CODE: Eurostat Country code
\item URBN_TYPE: URBN_TYPE
\item NUTS_ID: NUTS identifier
\item LEVL_CODE: NUTS level code (0,1,2,3)
\item geometry: geometry field
}
}
\source{
\url{https://gisco-services.ec.europa.eu/distribution/v2/nuts/}, file
\code{NUTS_RG_20M_2016_4326.geojson}.
}
\description{
A \code{sf} object including all NUTS levels of Spain as provided by
GISCO (2016 version).
}
\examples{
library(sf)

nuts <- esp_nuts.sf
nuts3 <- subset(nuts, LEVL_CODE == 3)

unique(nuts3$MOUNT_TYPE)

plot(
  nuts3[, "URBN_TYPE"],
  pal = hcl.colors(3, palette = "Viridis"),
  main = "Urban type -  NUTS3 levels of Spain",
  key.pos = NULL
)
}
\seealso{
\code{\link[=esp_get_nuts]{esp_get_nuts()}}
}
\concept{datasets}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/esp_dict.R
\name{esp_dict_region_code}
\alias{esp_dict_region_code}
\alias{esp_dict_translate}
\title{Convert and translate subdivision names}
\usage{
esp_dict_region_code(sourcevar, origin = "text", destination = "text")

esp_dict_translate(sourcevar, lang = "en", all = FALSE)
}
\arguments{
\item{sourcevar}{Vector which contains the subdivision names to be converted.}

\item{origin, destination}{One of \code{"text"}, \code{"nuts"}, \code{"iso2"}, \code{"codauto"}
and \code{"cpro"}.}

\item{lang}{Language of translation. Available languages are:
\itemize{
\item \code{"es"}: Spanish
\item \code{"en"}: English
\item \code{"ca"}: Catalan
\item \code{"ga"}: Galician
\item \code{"eu"}: Basque
}}

\item{all}{Logical. Should the function return all names or not?
On \code{FALSE} it returns a character vector. See \strong{Value}.}
}
\value{
\code{\link[=esp_dict_region_code]{esp_dict_region_code()}} returns a vector of characters.

\code{\link[=esp_dict_translate]{esp_dict_translate()}} returns a \code{character} vector or a named \code{list} with
each of the possible names of each \code{sourcevar} on the required language
\code{lang}.
}
\description{
Converts long subdivision names into different coding schemes and languages.
}
\details{
If no match is found for any value, the function displays a warning and
returns \code{NA} for those values.

Note that mixing names of different administrative levels (e.g. "Catalonia"
and "Barcelona") may return empty values, depending on the \code{destination}
values.
}
\examples{
vals <- c("Errioxa", "Coruna", "Gerona", "Madrid")

esp_dict_region_code(vals)
esp_dict_region_code(vals, destination = "nuts")
esp_dict_region_code(vals, destination = "cpro")
esp_dict_region_code(vals, destination = "iso2")

# From ISO2 to another codes

iso2vals <- c("ES-M", "ES-S", "ES-SG")
esp_dict_region_code(iso2vals, origin = "iso2")
esp_dict_region_code(iso2vals,
  origin = "iso2",
  destination = "nuts"
)
esp_dict_region_code(iso2vals,
  origin = "iso2",
  destination = "cpro"
)

# Mixing levels
valsmix <- c("Centro", "Andalucia", "Seville", "Menorca")
esp_dict_region_code(valsmix, destination = "nuts")
\dontrun{

# Warning

esp_dict_region_code(valsmix, destination = "codauto")
esp_dict_region_code(valsmix, destination = "iso2")
}


vals <- c("La Rioja", "Sevilla", "Madrid", "Jaen", "Orense", "Baleares")

esp_dict_translate(vals)
esp_dict_translate(vals, lang = "es")
esp_dict_translate(vals, lang = "ca")
esp_dict_translate(vals, lang = "eu")
esp_dict_translate(vals, lang = "ga")

esp_dict_translate(vals, lang = "ga", all = TRUE)
}
\seealso{
Other dictionary: 
\code{\link{esp_codelist}}

Other dictionary: 
\code{\link{esp_codelist}}
}
\concept{dictionary}

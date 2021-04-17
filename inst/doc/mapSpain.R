## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE,
  fig.width = 6,
  fig.height = 4,
  out.width = "100%",
  out.height = "60%"
)

## ----basic--------------------------------------------------------------------

library(mapSpain)
library(tmap)

country <- esp_get_country()
lines <- esp_get_can_box()

tm_shape(country) +
  tm_polygons() +
  tm_shape(lines) +
  tm_lines() +
  tm_graticules(lines = FALSE) +
  tm_style("classic") +
  tm_layout(main.title = "Map of Spain")

# Plot provinces

Andalucia <- esp_get_prov("Andalucia")

tm_shape(Andalucia) +
  tm_polygons(col = "darkgreen", border.col = "white") +
  tm_graticules(lines = FALSE)

# Plot municipalities

Euskadi_CCAA <- esp_get_ccaa("Euskadi")
Euskadi <- esp_get_munic(region = "Euskadi")

# Use dictionary

Euskadi$name_eu <- esp_dict_translate(Euskadi$ine.prov.name, lang = "eu")

tm_shape(Euskadi_CCAA) +
  tm_fill("grey50") +
  tm_shape(Euskadi) +
  tm_polygons("name_eu",
    palette = c("red2", "darkgreen", "ivory2"),
    title = ""
  ) +
  tm_layout(
    main.title = paste0(
      "Euskal Autonomia Erkidegoko",
      "\n",
      "Probintziak"
    ),
    main.title.size = 0.8,
    main.title.fontface = "bold"
  )



## ----choro--------------------------------------------------------------------

census <- mapSpain::pobmun19

# Extract CCAA from base dataset

codelist <- mapSpain::esp_codelist

census <-
  unique(merge(census, codelist[, c("cpro", "codauto")], all.x = TRUE))

# Summarize by CCAA
census_ccaa <-
  aggregate(cbind(pob19, men, women) ~ codauto, data = census, sum)

census_ccaa$porc_women <- census_ccaa$women / census_ccaa$pob19
census_ccaa$porc_women_lab <-
  paste0(round(100 * census_ccaa$porc_women, 2), "%")

# Merge into spatial data

CCAA_sf <- esp_get_ccaa()
CCAA_sf <- merge(CCAA_sf, census_ccaa)
Can <- esp_get_can_box()


# Plot with tmap
tm_shape(CCAA_sf) +
  tm_polygons(
    "porc_women",
    border.col = "grey70",
    title = "Porc. women",
    palette = "Blues",
    alpha = 0.7,
    legend.format = list(
      fun = function(x) {
        sprintf("%1.1f%%", 100 * x)
      }
    )
  ) +
  tm_shape(CCAA_sf, point.per = "feature") +
  tm_text("porc_women_lab", auto.placement = TRUE) +
  tm_shape(Can) +
  tm_lines(col = "grey70") +
  tm_layout(legend.position = c("LEFT", "center"))

## ----thematic, fig.asp=0.6----------------------------------------------------

# Population density of Spain

library(sf)

pop <- mapSpain::pobmun19
munic <- esp_get_munic()

# Get area (km2) - Use LAEA projection
municarea <- as.double(st_area(st_transform(munic, 3035)) / 1000000)
munic$area <- municarea

munic.pop <- merge(munic, pop, all.x = TRUE)
munic.pop$dens <- munic.pop$pob19 / munic.pop$area

br <-
  c(
    0,
    10,
    25,
    100,
    200,
    500,
    1000,
    5000,
    10000,
    Inf
  )


tm_shape(munic.pop) +
  tm_fill("dens",
    breaks = br,
    alpha = 0.8,
    title = "Pop. per km2",
    palette = "inferno",
    showNA = FALSE,
    colorNA = "#000004"
  ) +
  tm_layout(
    main.title = "Population Density in Spain (2019)",
    main.title.size = 0.8,
    frame = FALSE,
    bg.color = "#000004",
    legend.outside = TRUE,
    legend.text.color = "white",
    legend.title.color = "white",
    main.title.color = "white",
    main.title.fontface = "bold",
    legend.text.fontface = "bold",
    legend.title.fontface = "bold"
  )




## ----giscoR-------------------------------------------------------------------

library(giscoR)

# Set the same resolution for a perfect fit

res <- "20"

all_countries <- gisco_get_countries(resolution = res)
eu_countries <- gisco_get_countries(resolution = res, region = "EU")
ccaa <- esp_get_ccaa(moveCAN = FALSE, resolution = res)

# Project to same CRS

all_countries <- st_transform(all_countries, 3035)
eu_countries <- st_transform(eu_countries, 3035)
ccaa <- st_transform(ccaa, 3035)

# Plot

tm_shape(all_countries, bbox = c(23, 14, 67, 54) * 10e4) +
  tm_graticules(col = "#DFDFDF", alpha = 0.7) +
  tm_fill("#DFDFDF") +
  tm_shape(eu_countries) +
  tm_polygons("#FDFBEA", border.col = "#656565") +
  tm_shape(ccaa) +
  tm_polygons("#C12838", border.col = "white")


## ----session_info, echo=FALSE-------------------------------------------------
sessionInfo()


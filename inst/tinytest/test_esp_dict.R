library(tinytest)

vals <- c("Errioxa", "Coruna", "Gerona", "Madrid")

expect_error(esp_dict_region_code(vals, "aa"))
expect_error(esp_dict_region_code(vals, destination = "aa"))

expect_silent(esp_dict_region_code(vals))
expect_silent(esp_dict_region_code(vals, destination = "nuts"))
expect_silent(esp_dict_region_code(vals, destination = "cpro"))
expect_silent(esp_dict_region_code(vals, destination = "iso2"))

# From ISO2 to another codes

iso2vals <- c("ES-M", "ES-S", "ES-SG")

expect_silent(esp_dict_region_code(iso2vals, origin = "iso2"))


# Test all ISO2 prov

f <- unique(esp_codelist$iso2.prov.code)

expect_silent(esp_dict_region_code(f, "iso2", "cpro"))

# Test all ISO2 auto

f <- unique(esp_codelist$iso2.ccaa.code)

expect_silent(esp_dict_region_code(f, "iso2", "codauto"))




# Mixing levels
valsmix <- c("Centro", "Andalucia", "Seville", "Menorca")
expect_silent(esp_dict_region_code(valsmix, destination = "nuts"))

## Not run:

# Warning

expect_warning(esp_dict_region_code(valsmix, destination = "codauto"))
expect_warning(esp_dict_region_code(valsmix, destination = "iso2"))

## End(Not run)

vals <-
  c("La Rioja", "Sevilla", "Madrid", "Jaen", "Orense", "Baleares")
expect_error(esp_dict_translate(vals, "xx"))
expect_silent(esp_dict_translate(vals))
expect_true(class(esp_dict_translate(vals, all = TRUE)) == "list")
expect_warning(esp_dict_translate(c(vals, "pepe")))

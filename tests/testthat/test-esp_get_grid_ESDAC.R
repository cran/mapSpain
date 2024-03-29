test_that("Errors", {
  expect_error(esp_get_grid_ESDAC("50"))
})

test_that("ESDAC grid online", {
  skip_on_cran()
  skip_if_siane_offline()

  expect_message(
    esp_get_grid_ESDAC(
      cache_dir = tempdir(),
      verbose = TRUE
    )
  )

  expect_message(
    esp_get_grid_ESDAC(
      cache_dir = tempdir(),
      update_cache = TRUE,
      verbose = TRUE
    )
  )
  expect_silent(esp_get_grid_ESDAC())
})

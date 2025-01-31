context("Testing mapping function")

library(occCite)

data("myOccCiteObject")

test_that("inputs to map are as expected", {
  x <- myOccCiteObject@occResults[[1]]
  sp.name <- names(myOccCiteObject@occResults)[[1]]
  tabResults <- occCite:::tabulate.occResults(x = x, sp.name = sp.name)

  expect_true(class(myOccCiteObject) == "occCiteData")
  expect_true(names(myOccCiteObject@occResults) > 0)

  expect_true(all(c("longitude", "latitude")
  %in% names(occCite:::tabulate.occResults(x, sp.name))))
})

test_that("default occCiteMap settings work", {
  expect_error(occCiteMap())

  test <- occCiteMap(myOccCiteObject)
  expect_true(all(c("leaflet", "htmlwidget") %in% class(test)))
})

test_that("occCiteMap works with species specified", {
  expect_error(occCiteMap(myOccCiteObject, "Protea cynaroides",
                          species_colors = "red", "blue"))

  test <- occCiteMap(myOccCiteObject, "Protea cynaroides")
  expect_true(all(c("leaflet", "htmlwidget") %in% class(test)))
})

test_that("occCiteMap works with non-awesome markers color specified", {
  test <- occCiteMap(myOccCiteObject,
    "Protea cynaroides",
    species_colors = "brown",
    awesomeMarkers = F
  )
  expect_true(all(c("leaflet", "htmlwidget") %in% class(test)))
})

test_that("occCiteMap works with awesome markers color specified", {
  test <- occCiteMap(myOccCiteObject,
    "Protea cynaroides",
    species_colors = "lightred",
    awesomeMarkers = T
  )
  expect_true(all(c("leaflet", "htmlwidget") %in% class(test)))
})

test_that("occCiteMap works with map_limit specified", {
  test <- occCiteMap(myOccCiteObject,
    "Protea cynaroides",
    species_colors = "lightred",
    awesomeMarkers = T,
    map_limit = 10
  )
  expect_true(all(c("leaflet", "htmlwidget") %in% class(test)))
})

test_that("occCiteMap works with cluster set to true", {
  test <- occCiteMap(myOccCiteObject,
    "Protea cynaroides",
    species_colors = "lightred",
    awesomeMarkers = T,
    cluster = T
  )
  expect_true(all(c("leaflet", "htmlwidget") %in% class(test)))
})


context("Testing tabulate.occResults")
library(occCite)
data("myOccCiteObject")
test_that("inputs to tabulate.occResults are as expected", {
  testResults <- myOccCiteObject@occResults
  resNames <- names(testResults)

  expect_true(class(myOccCiteObject) == "occCiteData")
  expect_true(names(myOccCiteObject@occResults) > 0)
  expect_true(is.character(resNames))
  expect_true(all(!is.na(stringr::str_extract(
    string = resNames,
    pattern = "(\\w+\\s\\w+)"
  ))))
})

test_that("outputs to tablulate.occResults are as expected", {

  expect_error(tabulate.occResults())

  sp.name <- names(myOccCiteObject@occResults)[[1]]
  x <- myOccCiteObject@occResults[[1]]
  testResults <- tabulate.occResults(x = x, sp.name = sp.name)

  expect_true(class(testResults) == "data.frame")

  expect_true("name" %in% names(testResults))
  expect_true(!is.na(testResults$name[1]))
  expect_equal(class(testResults$name), "character")
  expect_true("longitude" %in% names(testResults))
  expect_equal(class(testResults$longitude), "numeric")
  expect_true("latitude" %in% names(testResults))
  expect_equal(class(testResults$latitude), "numeric")
  expect_true("day" %in% names(testResults))
  expect_equal(class(testResults$day), "integer")
  expect_true("month" %in% names(testResults))
  expect_equal(class(testResults$month), "integer")
  expect_true("year" %in% names(testResults))
  expect_equal(class(testResults$year), "integer")
  expect_true("datasetName" %in% names(testResults))
  expect_equal(class(testResults$datasetName), "character")
  expect_true("datasetKey" %in% names(testResults))
  expect_equal(class(testResults$datasetKey), "character")
  expect_true("dataService" %in% names(testResults))
  expect_equal(class(testResults$dataService), "character")
  expect_true("GBIF" %in% testResults$dataService)
  expect_true("BIEN" %in% testResults$dataService)
})

context("Testing summary plot function")

library(occCite)
data("myOccCiteObject")

test_that("default sumFig settings work", {
  test <- plot(myOccCiteObject)
  expect_true("yearHistogram" %in% names(test))
  expect_equal(class(test[[1]]), "ggplot_built")
  expect_true("source" %in% names(test))
  expect_equal(class(test[[2]]), "ggplot_built")
  expect_true("aggregator" %in% names(test))
  expect_equal(class(test[[3]]), "ggplot_built")
})

test_that("sumFig works when plotting by species", {
  test <- plot(myOccCiteObject, bySpecies = T)
  expect_true(names(test) == "Protea cynaroides")
  expect_true("yearHistogram" %in% names(test[[1]]))
  expect_equal(class(test[[1]][[1]]), "ggplot_built")
  expect_true("source" %in% names(test[[1]]))
  expect_equal(class(test[[1]][[2]]), "ggplot_built")
  expect_true("aggregator" %in% names(test[[1]]))
  expect_equal(class(test[[1]][[3]]), "ggplot_built")
})

test_that("sumFig works when plotting only year histogram by species", {
  data("myOccCiteObject")
  test <- plot(myOccCiteObject, bySpecies = T, plotTypes = "yearHistogram")
  expect_true(names(test) == "Protea cynaroides")
  expect_true("yearHistogram" %in% names(test[[1]]))
  expect_equal(class(test[[1]][[1]]), "ggplot_built")
})

test_that("sumFig works when plotting only source by species", {
  data("myOccCiteObject")
  test <- plot(myOccCiteObject, bySpecies = T, plotTypes = "source")
  expect_true(names(test) == "Protea cynaroides")
  expect_true("source" %in% names(test[[1]]))
  expect_equal(class(test[[1]][[1]]), "ggplot_built")
})

test_that("sumFig works when plotting only aggregator by species", {
  data("myOccCiteObject")
  test <- plot(x = myOccCiteObject, bySpecies = T, plotTypes = "aggregator")
  expect_true(names(test) == "Protea cynaroides")
  expect_true("aggregator" %in% names(test[[1]]))
  expect_equal(class(test[[1]][[1]]), "ggplot_built")
})

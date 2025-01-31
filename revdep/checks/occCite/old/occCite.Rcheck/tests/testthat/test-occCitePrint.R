library(occCite)
library(ape)

test_that("is occCitePrint working?", {
  data(myOccCiteObject)
  mySimpleOccCiteObject <- myOccCiteObject
  myOccCitations <- occCitation(mySimpleOccCiteObject)
  test_that("regular print", {expect_output(print(myOccCitations))})

  test_that("print by species", {expect_output(print(myOccCitations, bySpecies = TRUE))})
})

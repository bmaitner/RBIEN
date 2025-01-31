context("Verifies performance of taxonRectification")

library(occCite)

# test_that("functions on which it depends function as necessary", {
#   skip_if(!curl::has_internet(), "internet connection unsuccessful")
#   skip_if(httr::http_error("https://resolver.globalnames.org/data_sources.json"))
#   skip_if(!requireNamespace("taxize", quietly = TRUE))
#   sources <- taxize::gnr_datasources()
#
#   expect_true("data.frame" %in% class(sources))
#   expect_true("title" %in% colnames(sources))
#   expect_true("id" %in% colnames(sources))
#   expect_true("National Center for Biotechnology Information" %in% sources$title)
#
#   datasources <- "National Center for Biotechnology Information"
#   sourceIDs <- sources$id[sources$title %in% datasources]
#   temp <- taxize::gnr_resolve(sci = "Buteo buteo", data_source_ids = sourceIDs)
#
#   expect_true("data.frame" %in% class(temp))
#   expect_true("user_supplied_name" %in% colnames(temp))
#   expect_true("matched_name" %in% colnames(temp))
#   expect_true("data_source_title" %in% colnames(temp))
#   expect_true(nrow(temp) == 1)
#   expect_true(temp$user_supplied_name == temp$matched_name)
#   expect_true(temp$data_source_title == datasources)
# })

test_that("taxonRectification performs as expected", {
  skip_if(!curl::has_internet(), "internet connection unsuccessful")
  skip_if(httr::http_error("https://resolver.globalnames.org/data_sources.json"))

  # testResult <- taxonRectification(
  #   taxName = "Buteo buteo hartedi",
  #   datasources = "National Center for Biotechnology Information"
  #
  # )
  #
  # expect_true(class(testResult) == "data.frame")
  # expect_true("Input Name" %in% colnames(testResult))
  # expect_true("Searched Taxonomic Databases w/ Matches"
  # %in% colnames(testResult))
  # expect_true(nrow(testResult) == 1)
  # expect_true(testResult$`Input Name`[1] == "Buteo buteo hartedi")
  # expect_true(testResult$`Best Match`[1] == "Buteo buteo harterti")
  # expect_true(testResult$`Searched Taxonomic Databases w/ Matches` == "National Center for Biotechnology Information")
  #
  # expect_warning(taxonRectification(taxName = "Buteo buteo hartedi", datasources = "cheese"))
  # expect_warning(taxonRectification(taxName = "Buteo buteo hartedi", datasources = NULL))
  # expect_warning(taxonRectification(taxName = "cheese", datasources = "National Center for Biotechnology Information"))
  # expect_warning(taxonRectification(taxName = "Buteo buteo hartedi",
  #                                   datasources = "National Center for Biotechnology Information", skipTaxize = "purple"))

  testResult <- taxonRectification(
    taxName = "Buteo buteo hartedi",
    datasources = "National Center for Biotechnology Information",
    skipTaxize = TRUE
  )

  expect_true(class(testResult) == "data.frame")
  expect_true("Input Name" %in% colnames(testResult))
  expect_true("Searched Taxonomic Databases w/ Matches"
              %in% colnames(testResult))
  expect_true(nrow(testResult) == 1)
  expect_true(testResult$`Input Name`[1] == "Buteo buteo hartedi")
  expect_true(testResult$`Best Match`[1] == "Buteo buteo hartedi")
  expect_true(testResult$`Searched Taxonomic Databases w/ Matches` == "Not rectified.")
})

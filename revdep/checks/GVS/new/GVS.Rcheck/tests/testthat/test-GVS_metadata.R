context("gvs metadata")


test_that("example works", {
  # because of the way this call is structured, I can't capture it via vcr, so I skip it on cran/offline
  skip_if_offline(host = "r-project.org")
  skip_on_cran()

  metadata <- GVS_metadata()

  # test below assume a data dictionary and will be skipped if one isn't returned

  skip_if_not(class(metadata) == "list")

  expect_equal(object = class(metadata[[1]]), expected = "data.frame")
  expect_equal(object = class(metadata[[2]]), expected = "data.frame")
  expect_equal(object = class(metadata[[3]]), expected = "data.frame")

  expect_equal(object = length(metadata), expected = 3)
})

context("gvs main")


test_that("example works", {
  # skip_if_offline(host = "r-project.org")

  vcr::use_cassette("gvs_example", {

    results <- GVS(occurrence_dataframe = gvs_testfile,
                   url=url,
                   skip_internet_check = TRUE)

  })


  # test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(results) == "data.frame")
  expect_equal(object = nrow(results), expected = nrow(gvs_testfile))
})


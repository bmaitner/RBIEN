context("nsr version")


test_that("example works", {
  # skip_if_offline(host = "r-project.org")

  vcr::use_cassette("nsr_version", {
    NSR_version_metadata <- NSR_version(
      url = url,
      skip_internet_check = TRUE
    )
  })


  # test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(NSR_version_metadata) == "data.frame")
  expect_equal(object = nrow(NSR_version_metadata), expected = 1)
})

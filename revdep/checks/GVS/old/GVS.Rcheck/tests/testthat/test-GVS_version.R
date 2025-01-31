context("gvs version")


test_that("example works", {
  # skip_if_offline(host = "r-project.org")

  vcr::use_cassette("gvs_version", {
    GVS_version_metadata <- GVS_version(
      url = url,
      skip_internet_check = TRUE
    )
  })


  # test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(GVS_version_metadata) == "data.frame")
  expect_equal(object = nrow(GVS_version_metadata), expected = 1)
})

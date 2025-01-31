context("gvs sources")


test_that("example works", {
  # skip_if_offline(host = "r-project.org")


  vcr::use_cassette("gvs_sources", {
    sources <- GVS_sources(
      url = url,
      skip_internet_check = TRUE
    )
  })


  # test below assume a dataframe and will be skipped if one isn't returned
  skip_if_not(class(sources) == "data.frame")
  expect_gt(object = nrow(sources), expected = 0)
})

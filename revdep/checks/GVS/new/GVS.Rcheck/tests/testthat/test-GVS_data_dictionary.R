context("gvs data dictionary")


test_that("example works", {
  # skip_if_offline(host = "r-project.org")


  vcr::use_cassette("gvs_dd", {

    GVS_fields <- GVS_data_dictionary(url = url,
                                      skip_internet_check = TRUE)
  })


  # test below assume a data.frame and will be skipped if one isn't returned
  skip_if_not(class(GVS_fields) == "data.frame")
  expect_gt(object = nrow(GVS_fields), expected = 2)
})



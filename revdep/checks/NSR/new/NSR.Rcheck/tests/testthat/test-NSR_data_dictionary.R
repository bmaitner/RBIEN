context("nsr data dictionary")


test_that("example works", {
  # skip_if_offline(host = "r-project.org")
  
  
  vcr::use_cassette("nsr_dd", {
    
    NSR_fields <- NSR_data_dictionary(url = url,
                                      skip_internet_check = TRUE)
  })
  
  
  # test below assume a data.frame and will be skipped if one isn't returned
  skip_if_not(class(NSR_fields) == "data.frame")
  expect_gt(object = nrow(NSR_fields), expected = 2)
})


test_that("example2 works", {
  # skip_if_offline(host = "r-project.org")
  
  
  vcr::use_cassette("nsr_dd_status", {
    
    NSR_status <- NSR_data_dictionary(native_status = TRUE,
                                      url = url,
                                      skip_internet_check = TRUE)
  })
  
  
  # test below assume a data.frame and will be skipped if one isn't returned
  skip_if_not(class(NSR_status) == "data.frame")
  expect_gt(object = nrow(NSR_status), expected = 2)
})

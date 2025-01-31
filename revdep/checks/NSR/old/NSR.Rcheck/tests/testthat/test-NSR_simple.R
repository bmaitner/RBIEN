context("nsr simple")


test_that("example works", {
  # skip_if_offline(host = "r-project.org")
  
  vcr::use_cassette("nsr_simple_example", {
    
    results <- NSR_simple(species = "Acer rubrum",
                          country = "Canada",
                          state_province = "Ontario",
                          url=url,
                          skip_internet_check = TRUE)
    

  })
  
  
  # test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(results) == "data.frame")
  expect_equal(object = nrow(results), expected = 1)
})


test_that("second example works", {
  # skip_if_offline(host = "r-project.org")
  
  vcr::use_cassette("nsr_simple_example2", {

    results2 <- NSR_simple(species = c("Acer rubrum", "Aspen tremuloides") ,
                          country = c("Canada","Canada"),
                          state_province = c("Ontario","Ontario"),
                          url=url,
                          skip_internet_check = TRUE)
    
    
  })
  
  # test below assume a data dictionary and will be skipped if one isn't returned
  skip_if_not(class(results2) == "data.frame")
  expect_equal(object = nrow(results2), expected = 2)
})




context("gvs collaborators")


test_that("example works", {
  # skip_if_offline(host = "r-project.org")


  vcr::use_cassette("gvs_collaborators", {
    collaborator_info <- GVS_collaborators(
      url = url,
      skip_internet_check = TRUE
    )
  })


  # test below assume a data.frame and will be skipped if one isn't returned
  skip_if_not(class(collaborator_info) == "data.frame")
  expect_gt(object = nrow(collaborator_info), expected = 3)

})

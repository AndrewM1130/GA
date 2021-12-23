test_that("generate_data", {

  expect_equal(ncol(generate_data()),51)
  expect_equal(nrow(generate_data()),1000)
})

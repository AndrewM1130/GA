test_that("genetic algorithm output", {
  data <- generate_data()
  independent_vars<-data[,2:51]
  response_vec <- data[,1]
  out <- select(5,pop=10,gene_length = 50,response_vec = response_vec,independent_vars = independent_vars)
  expect_true(is.list(out))
  expect_true(is.numeric(out[[1]]))
  expect_true(is.numeric(out[[2]]))
  expect_true(is.numeric(out[[3]]))
  expect_true(is.data.frame(out[[4]]))
  expect_true(is.data.frame(out[[5]]))
})

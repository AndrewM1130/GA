test_that("AIC Works", {
  data <- matrix(rnorm(11*20),ncol = 11)
  gene <-create_initial_generation(40,10,.3)
  metric <- 'AIC'
  custom_function <- NULL
  score_vec <- score_fitness2(gene,data,metric,custom_function = custom_function)
  expect_true(is.list(score_vec))
  expect_equal(length(score_vec),2)
  score_vec <- score_vec[[1]]
  expect_true(!any(is.na(score_vec)))
  expect_true(!any(is.infinite(score_vec)))
  expect_equal(length(score_vec),40)
  expect_true(all(score_vec>=0))

})

test_that("BIC Works", {
  data <- matrix(rnorm(11*20),ncol = 11)
  gene <-create_initial_generation(40,10,.3)
  metric <- 'BIC'
  custom_function <- NULL
  score_vec <- score_fitness2(gene,data,metric,custom_function = custom_function)
  expect_true(is.list(score_vec))
  expect_equal(length(score_vec),2)
  score_vec <- score_vec[[1]]
  expect_true(!any(is.na(score_vec)))
  expect_true(!any(is.infinite(score_vec)))
  expect_equal(length(score_vec),40)
  expect_true(all(score_vec>=0))

})

test_that("AICC Works", {
  data <- matrix(rnorm(11*20),ncol = 11)
  gene <-create_initial_generation(40,10,.3)
  metric <- 'AICC'
  custom_function <- NULL
  score_vec <- score_fitness2(gene,data,metric,custom_function = custom_function)
  expect_true(is.list(score_vec))
  expect_equal(length(score_vec),2)
  score_vec <- score_vec[[1]]
  expect_true(!any(is.na(score_vec)))
  expect_true(!any(is.infinite(score_vec)))
  expect_equal(length(score_vec),40)
  expect_true(all(score_vec>=0))

})

test_that("custom Works", {
  data <- matrix(rnorm(11*20),ncol = 11)
  gene <-create_initial_generation(40,10,.3)
  custom_function <- function(gene,data) {return(1)}
  score_vec <- score_fitness2(gene,data,custom_function = custom_function)
  expect_true(is.list(score_vec))
  expect_equal(length(score_vec),2)
  score_vec <- score_vec[[1]]
  expect_true(!any(is.na(score_vec)))
  expect_true(!any(is.infinite(score_vec)))
  expect_equal(length(score_vec),40)
  expect_true(all(score_vec>=0))

})

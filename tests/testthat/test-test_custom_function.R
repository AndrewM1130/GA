test_that("test_test custom good", {
  data <- matrix(rnorm(11*20),ncol = 11)
  generation_matrix <-create_initial_generation(40,10,.3)
  func <- function(gene,data) {return(1)}

  out <- test_user_function(func,generation_matrix = generation_matrix,data = data)
  expect_true(out[[1]])
  expect_equal(out[[2]],NA)
})

test_that("test_test custom Inf", {
  data <- matrix(rnorm(11*20),ncol = 11)
  generation_matrix <-create_initial_generation(40,10,.3)
  func <- function(gene,data) {return(1/0)}

  out <- test_user_function(func,generation_matrix = generation_matrix,data = data)
  expect_equal(out[[1]],"user-provided function returned one or more infinite results")
  expect_equal(out[[2]],NA)
})

test_that("test_test custom NA", {
  data <- matrix(rnorm(11*20),ncol = 11)
  generation_matrix <-create_initial_generation(40,10,.3)
  func <- function(gene,data) {return(NA)}

  out <- test_user_function(func,generation_matrix = generation_matrix,data = data)
  expect_equal(out[[1]],"User-Provided Function returned one or more NA result")
  expect_equal(out[[2]],NA)
})


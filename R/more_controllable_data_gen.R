#' Generates normal data with 50 Covariates.
#'
#' This function creates a matrix that is 51 x 1000 where the first column is
#'  the response variables and the remain 50 are independent variables.
#'  The covariates are generated uniform(0,100)
#'  The parameter for each covariate is defined such that the
#'  first one is 20x0.9e1 followed by 20x0.9e2 with each subsequent
#'  parameter decaying until the last one is 20x0.9e50. This allows us to
#'  validate a genetic algorithm while generating a large dataset
#'
#'
#'
#' @return a 1000x51 matrix
#' @export
#'
#' @examples
#'
#' generate_data()
#'
#'
generate_data <- function() {
  s <- 5
  b <- matrix(round(20*.9^(1:50),2),ncol=1)
  x_mat <- matrix(runif(50*1000,0,100),ncol=50)
  bx <- x_mat %*% b
  y_vec <- rnorm(1000,bx,s)
  return(cbind(y_vec,x_mat))
}


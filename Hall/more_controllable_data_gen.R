

generate_data <- function() {
  s <- 5
  b<-matrix(round(20*.9^(1:50),2),nc=1)
  x_mat<-matrix(runif(50*1000,0,100),nc=50)
  bx <- x_mat %*% b
  y_vec <- rnorm(1000,bx,s)
  return(cbind(y_vec,x_mat))
}


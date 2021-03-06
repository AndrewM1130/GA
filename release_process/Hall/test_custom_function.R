### Overview

### This function confirms that a user-provided function takes genes from 
### a generation_matrix and a data matrix and returns a score for each gene 
###  It also confirms there are not NAs or infinity values.
### This function does not confirm that the scores are in descending order
### aka higher fitness creatures receive a higher fitness scores. It also 
### does not confirm that all possible scores are defined. 

### Definition
### User-provided function must have its first two arguments be 
### (1) generation_matrix and (2) data. 
### generation matrix needs is a population x gene-length matrix and data is 
### a (gene_length +1_ x observation matrix where the first column is 
### the response variables and the other column are the independent variables.
### output needs to be a vector of length gene_length.  
### score_vec cannot have NAs, infinities, and must be numeric.



### test_user_function

test_user_function <- function(func,generation_matrix,data) {
  
  ### ----- Assertions ----- ###
  
  assert_that(is.matrix(generation_matrix) &
                all(generation_matrix==0|generation_matrix==1) &
                min(rowSums(generation_matrix,na.rm = TRUE))>0 &
                all(!is.na(generation_matrix)) &
                all(!is.infinite(generation_matrix)),
              msg = 'generation_matrix needs to be a matrix of 1s and 0s')
  
  assert_that(is.matrix(data) || is.data.frame(data),
              msg = 'data frame is not a matrix or dataframe')
                
  assert_that(all(!is.na(generation_matrix)) &
                all(!is.infinite(generation_matrix)),
              msg = 'data needs to be a matrix without infinity or NAs')
  
  
  ### ----- Initialize Data For Function ----- ###
  
  ###----- Avoiding Coercion-----###
  error_note <- NA
  
  message <- TRUE
  out <- rep(0,nrow(generation_matrix))

  for (i in 1:nrow(generation_matrix)) {
    temp <- try(func(generation_matrix,data))
    
    if (class(temp)=="try-error") {
      error_note <- temp
      message <-"User-Provided Function did not run as exected. Please review package help instructions"
      break()
    }
    out[i]<-temp
    
      
  }
  
  
  
  
  ### ----- Test Assertions ----- ###[]
  if (message==TRUE) {
    
    if (!is.vector(out)) {
      
      message <- "User-Provided Function did not return a vector"
    }
    
    if (length(out) != nrow(generation_matrix)) {
      message <- 'user-provided function did not return
    a vector with correct length'
    }
    
    if (sum(is.infinite(out)) != 0) {
      message <- 'user-provided function returned one or more infinite results'
    }
    
    if (sum(is.na(out)) !=0) {
      message <- "User-Provided Function returned one or more NA result"
    }
    
    if (sum(is.nan(out))!=0) {
      message <- "User-Provided Function returned one or more NaN result"
    }
    
    if (!is.numeric(out)) {
      message <- 'user-provided function did not return a numeric'
    }
      
    
  } 
  
  ### ----- Prepare Data for output ------ ###
  
    output <- vector(mode='list',2)
    names(output) <- c('message','error')
    output[[1]] <- message
    output[[2]] <- error_note 
    
  return(output)
}

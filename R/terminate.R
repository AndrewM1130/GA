#' Tests if early termination conditions are met.
#'
#'This function defines additional termination criteria which will run
#'in addition to the max_iteration.
#'Default is null for all values. User can specify a percentage_convergence
#'such that when diversity falls below a specific threshold the program
#'terminates. User can identify a estimate (from the standard summary()
#'function that if the estimate reaches a specific threshold the
#'program terminates or if the estimate pauses for a certain number of
#'iterations terminates.
#'
#'summary_data_frame - is imported from the main function and updated each
#'iteration
#'
#'estimator - cam be nominated from the summary() function  aka Min.,
#'1st Qu., Median, Mean, 3rd Qu., or Max.
#'
#'percent_converge - terminates once diversity falls below a certain percentage.
#'diversity is sum(unique(genes))/total_genes
#'
#' @param generation_matrix generation matrix with all 0s and 1s with ncol = gene_length and nrow = pop
#' @param summary_data_frame a matrix that captures the summary() data for each generation's scores
#' @param estimator character string referencing the named entry from summary() for instance Min.
#' @param pause_length a numeric indicating how many iterations without improvement in the estimator before you terminate
#' @param score_threshold a numeric indicating a threshold to cutoff if the estimator reaches that value
#' @param percent_converge percentage of diversity when algorithm terminates for instance .1 will terminate if there are only .90 of the population is represented by the same genes
#' @param iteration is the current iteration the main algorithm is on
#' @param metric is the user specified statistic such as R2, AIC, BIC, or AICc
#' @param fittest whether a custom function has the highest value corresponding to the fittest or the lowest value
#'
#' @return TRUE if terminate conditions are met  with a reason, FALSE otherwise
#' @export
#'
#' @examples
#' generation_matrix <- matrix(rbinom(2*10,1,.5),ncol=10)
#' summary_data_frame <- data.frame(matrix(1:12,ncol=6))
#' names(summary_data_frame) <-c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")
#' estimator <- 'Mean'
#' score_threshold <- 2.4
#' iteration <- 2
#'
#' confirm_terminate <- see_if_terminate(generation_matrix,
#' summary_data_frame,estimator = 'Mean',
#' score_threshold = score_threshold, iteration = iteration)
#'
#'

#termination condition

see_if_terminate <- function(generation_matrix,summary_data_frame,estimator = NULL,pause_length = NULL,score_threshold = NULL,percent_converge = NULL,iteration, metric = 'AIC',fittest = 'high'){

  ### ----- Assertions ----- ###



  assert_that(is.matrix(generation_matrix) &
                all(generation_matrix==0|generation_matrix==1) &
                min(rowSums(generation_matrix,na.rm = TRUE))>0 &
                all(!is.na(generation_matrix)) &
                all(!is.infinite(generation_matrix)),
              msg = 'generation_matrix needs to be a matrix of 1s and 0s')

  if (!is.null(estimator)) {
    estimator_auth <- c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")
    assert_that(estimator %in% estimator_auth,msg = 'estimator needs to be from approved list: "Min.","1st Qu.","Median","Mean","3rd Qu.","Max."')

  }

  if (!is.null(pause_length)) {
    assert_that(!is.null(estimator),msg = 'needs an estimator')

    assert_that(is.numeric(pause_length) &
                  is.count(pause_length) &
                  pause_length >1 & length(pause_length)==1,
                msg = 'pause_length needs to be a positive, integer, greater than 1')

  }

  if (!is.null(score_threshold)) {
    assert_that(!is.null(estimator),msg = 'needs an estimator')

    assert_that(is.numeric(score_threshold) &
                  is.matrix(score_threshold)==FALSE &
                  score_threshold >0 &
                  length(score_threshold)==1,
                msg = 'score_threshold needs to be a positive, numeric')

  }

  if (!is.null(percent_converge)) {

    assert_that(!is.null(estimator),msg = 'needs an estimator')

    assert_that(is.numeric(percent_converge) &
                  is.matrix(percent_converge)==FALSE &
                  percent_converge >0 &
                  percent_converge <1 &
                  length(percent_converge)==1,
                msg = 'percent_converge needs to be a positive, numeric between 1 and 0')
  }



  ### ----- Set Variables ----- ###

  terminate1 <- FALSE
  terminate2 <- FALSE
  terminate3 <- FALSE
  terminate_reason <- c(NaN,NaN,NaN)

  if (!is.null(estimator)) {

    index <- which(estimator_auth == estimator)
  }



  ### ----- Score Threshold ----- ###

  if (!is.null(score_threshold)) {

    estimate <- summary_data_frame[iteration,index]

    if (metric=='AIC'|metric=='BIC'|metric=='AICC'|fittest=='low') {

      terminate1 <- estimate <= score_threshold

    } else {
      terminate1 <- estimate >= score_threshold
    }

    if (terminate1 == TRUE) {

      terminate_reason[1] <- "score_threshold"
    }

  }

  ### ----- Pause_Length ----- ###

  if (!is.null(pause_length)) {

    pause_length_split <- iteration - pause_length

    if (pause_length_split > 0) {

      pause_vec <- summary_data_frame[pause_length_split:iteration,index]

      terminate2 <- min(pause_vec==pause_vec[1])

      if (terminate2 == TRUE) {

        terminate_reason[2] <- "pause_length"
      }

    }


  }

  ### ----- Percent_converge ----- ###

  if (!is.null(percent_converge)) {

    terminate3 <-nrow(unique(generation_matrix))/nrow(generation_matrix) < percent_converge


    if (terminate3 == TRUE) {

      terminate_reason[3] <- "percent_converge"
    }
  }


  terminate <- max(terminate1,terminate2,terminate3)

  out <- list(terminate,terminate_reason)

  return(out)

}

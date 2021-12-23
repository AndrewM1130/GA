test_that('score_threshold termination',{
  generation_matrix <-create_initial_generation(40,10,.3)
  summary_data_frame <- data.frame(matrix(1:18,ncol=6))
  names(summary_data_frame) <-c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")
  estimator <- 'Mean'
  iteration<-3
  score_threshold <-13

  out <- see_if_terminate(iteration = iteration, generation_matrix =generation_matrix ,summary_data_frame = summary_data_frame,estimator = estimator,pause_length = NULL,score_threshold = score_threshold,percent_converge = NULL)
  expect_true(out[[1]]==1)
  expect_true(out[[2]][1]=="score_threshold")
  })

test_that('pause_length termination',{
  generation_matrix <-create_initial_generation(40,10,.3)
  summary_data_frame <- data.frame(matrix(1:18,ncol=6))
  summary_data_frame[1:3,4]<-5
  names(summary_data_frame) <-c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")
  estimator <- 'Mean'
  iteration<-3
  pause_length <- 2

  out <- see_if_terminate(iteration = iteration, generation_matrix =generation_matrix ,summary_data_frame = summary_data_frame,estimator = estimator,pause_length = pause_length,score_threshold = NULL,percent_converge = NULL)
  expect_true(out[[1]]==1)
  expect_true(out[[2]][2]=="pause_length")
})

test_that('percent_converge termination',{
  generation_matrix <-matrix(1,nrow = 10,ncol=40)
  summary_data_frame <- data.frame(matrix(1:18,ncol=6))
  summary_data_frame[1:3,4]<-5
  names(summary_data_frame) <-c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")
  estimator <- 'Mean'
  iteration<-3
  percent_converge <-.3

  out <- see_if_terminate(iteration = iteration, generation_matrix =generation_matrix ,summary_data_frame = summary_data_frame,estimator = estimator,pause_length = NULL,score_threshold = NULL,percent_converge = percent_converge)
  expect_true(out[[1]]==1)
  expect_true(out[[2]][3]=="percent_converge")
})

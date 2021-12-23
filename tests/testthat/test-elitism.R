


test_that("elitism and AIC",{

  ### ----- Generate Data
  data <- matrix(rnorm(11*20),ncol = 11)
  gene <-create_initial_generation(40,10,.3)
    elite_prop <- .1
  metric <- 'AIC'
  num_elite <- ceiling(elite_prop * 40)

  initial_score_vec<-score_fitness2(gene = gene,data = data,metric = metric)
  initial_score_vec <- initial_score_vec[[1]]

  ### ----- Grab Top Parents for Comparison
  test_ranking <- cbind(1:length(initial_score_vec),initial_score_vec)
  test_ranking <- test_ranking[order(test_ranking[,2],decreasing = TRUE)]
  test_parent_index <-test_ranking[1:num_elite]
  test_genes <-matrix(gene[test_parent_index,],ncol = ncol(gene))
  test_score_vec<-score_fitness2(gene = test_genes,data = data,metric = metric)
  test_score_vec <- test_score_vec[[1]]

  elite_genes <- apply_elitism(data = data,gene = gene,
                               score_vec = initial_score_vec,
                               elite_prop = elite_prop, metric = metric)
  elite_score_vec<-score_fitness2(gene = elite_genes,data = data,metric = metric)
  elite_score_vec <- elite_score_vec[[1]]

  ### test elite is better than or equal
  expect_equal(sum(test_score_vec>elite_score_vec),0)

  ### test elite correct dimensions
  expect_equal(ncol(elite_genes),10)
  expect_equal(nrow(elite_genes),num_elite)
})

test_that("elitism and R2",{

  ### ----- Generate Data
  data <- matrix(rnorm(11*20),ncol = 11)
  gene <-create_initial_generation(40,10,.3)
  elite_prop <- .1
  metric <- 'R2'
  num_elite <- ceiling(elite_prop * 40)

  initial_score_vec<-score_fitness2(gene = gene,data = data,metric = metric)
  initial_score_vec <- initial_score_vec[[1]]

  ### ----- Grab Top Parents for Comparison
  test_ranking <- cbind(1:length(initial_score_vec),initial_score_vec)
  test_ranking <- test_ranking[order(test_ranking[,2],decreasing = TRUE)]
  test_parent_index <-test_ranking[1:num_elite]
  test_genes <-matrix(gene[test_parent_index,],ncol = ncol(gene))
  test_score_vec<-score_fitness2(gene = test_genes,data = data,metric = metric)
  test_score_vec <- test_score_vec[[1]]

  elite_genes <- apply_elitism(data = data,gene = gene,
                               score_vec = initial_score_vec,
                               elite_prop = elite_prop, metric = metric)
  elite_score_vec<-score_fitness2(gene = elite_genes,data = data,metric = metric)
  elite_score_vec <- elite_score_vec[[1]]

  ### test elite is better than or equal
  expect_equal(sum(test_score_vec>elite_score_vec),0)

  ### test elite correct dimensions
  expect_equal(ncol(elite_genes),10)
  expect_equal(nrow(elite_genes),num_elite)
})

test_that("elitism and BIC",{

  ### ----- Generate Data
  data <- matrix(rnorm(11*20),ncol = 11)
  gene <-create_initial_generation(40,10,.3)
  elite_prop <- .1
  metric <- 'BIC'
  num_elite <- ceiling(elite_prop * 40)

  initial_score_vec<-score_fitness2(gene = gene,data = data,metric = metric)
  initial_score_vec <- initial_score_vec[[1]]

  ### ----- Grab Top Parents for Comparison
  test_ranking <- cbind(1:length(initial_score_vec),initial_score_vec)
  test_ranking <- test_ranking[order(test_ranking[,2],decreasing = TRUE)]
  test_parent_index <-test_ranking[1:num_elite]
  test_genes <-matrix(gene[test_parent_index,],ncol = ncol(gene))
  test_score_vec<-score_fitness2(gene = test_genes,data = data,metric = metric)
  test_score_vec <- test_score_vec[[1]]

  elite_genes <- apply_elitism(data = data,gene = gene,
                               score_vec = initial_score_vec,
                               elite_prop = elite_prop, metric = metric)
  elite_score_vec<-score_fitness2(gene = elite_genes,data = data,metric = metric)
  elite_score_vec <- elite_score_vec[[1]]

  ### test elite is better than or equal
  expect_equal(sum(test_score_vec>elite_score_vec),0)

  ### test elite correct dimensions
  expect_equal(ncol(elite_genes),10)
  expect_equal(nrow(elite_genes),num_elite)
})

test_that("elitism and AICC",{

  ### ----- Generate Data
  data <- matrix(rnorm(11*20),ncol = 11)
  gene <-create_initial_generation(40,10,.3)
  elite_prop <- .1
  metric <- 'AICC'
  num_elite <- ceiling(elite_prop * 40)

  initial_score_vec<-score_fitness2(gene = gene,data = data,metric = metric)
  initial_score_vec <- initial_score_vec[[1]]

  ### ----- Grab Top Parents for Comparison
  test_ranking <- cbind(1:length(initial_score_vec),initial_score_vec)
  test_ranking <- test_ranking[order(test_ranking[,2],decreasing = TRUE)]
  test_parent_index <-test_ranking[1:num_elite]
  test_genes <-matrix(gene[test_parent_index,],ncol = ncol(gene))
  test_score_vec<-score_fitness2(gene = test_genes,data = data,metric = metric)
  test_score_vec <- test_score_vec[[1]]

  elite_genes <- apply_elitism(data = data,gene = gene,
                               score_vec = initial_score_vec,
                               elite_prop = elite_prop, metric = metric)
  elite_score_vec<-score_fitness2(gene = elite_genes,data = data,metric = metric)
  elite_score_vec <- elite_score_vec[[1]]

  ### test elite is better than or equal
  expect_equal(sum(test_score_vec>elite_score_vec),0)

  ### test elite correct dimensions
  expect_equal(ncol(elite_genes),10)
  expect_equal(nrow(elite_genes),num_elite)
})

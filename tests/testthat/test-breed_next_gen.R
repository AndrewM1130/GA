test_that("breed_next_generation",{
  gene <- rbind(c(1,1,1,0,0,0),c(1,1,1,0,0,0),c(0,0,0,1,1,1),c(0,0,0,1,1,1))
  #score vec
  score_vec <- c(1,2,3,4)
  #2 parents, roulette
  a1 <- select_parent(score_vec = score_vec, number_of_parents = 2,method = 'roulette',susN = 1,tourn_size = 2,pop_required = 4)
  #5 parents, roulette
  a2 <- select_parent(score_vec = score_vec, number_of_parents = 5,method = 'roulette',susN = 1,tourn_size = 2,pop_required = 4)

  #2 parents,crossover = uniform, mutation = fixed
  k1 <- breed_next_gen(required_pop = 4, generation_matrix = gene, score_vec = score_vec, number_of_parents = 2,
                       new_parents = a1, mutation = 'fixed', crossover = 'uniform', mutation_rate = .01, minimize_inbreeding = FALSE,
                       ad_max_mutate = .15, ad_min_mutate = .01, ad_inflection = .3, ad_curve = 15, number_of_crossovers = 1)

  #2 parents,crossover = k-point, mutation = fixed
  k2 <- breed_next_gen(required_pop = 4, generation_matrix = gene, score_vec = score_vec, number_of_parents = 2,
                       new_parents = a1, mutation = 'fixed', crossover = 'k_point', mutation_rate = .01, minimize_inbreeding = FALSE,
                       ad_max_mutate = .15, ad_min_mutate = .01, ad_inflection = .3, ad_curve = 15, number_of_crossovers = 1)

  #2 parents,crossover = fitness, mutation = fixed
  k3 <- breed_next_gen(required_pop = 4, generation_matrix = gene, score_vec = score_vec, number_of_parents = 2,
                       new_parents = a1, mutation = 'fixed', crossover = 'fitness', mutation_rate = .01, minimize_inbreeding = FALSE,
                       ad_max_mutate = .15, ad_min_mutate = .01, ad_inflection = .3, ad_curve = 15, number_of_crossovers = 1)

  #5 parents,crossover = uniform, mutation = fixed
  k4 <- breed_next_gen(required_pop = 4, generation_matrix = gene, score_vec = score_vec, number_of_parents = 5,
                       new_parents = a2, mutation = 'fixed', crossover = 'uniform', mutation_rate = .01, minimize_inbreeding = FALSE,
                       ad_max_mutate = .15, ad_min_mutate = .01, ad_inflection = .3, ad_curve = 15, number_of_crossovers = 1)

  #5 parents,crossover = k-point, mutation = fixed
  k5 <- breed_next_gen(required_pop = 4, generation_matrix = gene, score_vec = score_vec, number_of_parents = 5,
                       new_parents = a2, mutation = 'fixed', crossover = 'k_point', mutation_rate = .01, minimize_inbreeding = FALSE,
                       ad_max_mutate = .15, ad_min_mutate = .01, ad_inflection = .3, ad_curve = 15, number_of_crossovers = 1)

  #5 parents,crossover = fitness, mutation = fixed
  k6 <- breed_next_gen(required_pop = 4, generation_matrix = gene, score_vec = score_vec, number_of_parents = 5,
                       new_parents = a2, mutation = 'fixed', crossover = 'fitness', mutation_rate = .01, minimize_inbreeding = FALSE,
                       ad_max_mutate = .15, ad_min_mutate = .01, ad_inflection = .3, ad_curve = 15, number_of_crossovers = 1)

  #2 parents,crossover = uniform, mutation = adapative
  k7 <- breed_next_gen(required_pop = 4, generation_matrix = gene, score_vec = score_vec, number_of_parents = 2,
                       new_parents = a1, mutation = 'adaptive', crossover = 'fitness', mutation_rate = .01, minimize_inbreeding = FALSE,
                       ad_max_mutate = .15, ad_min_mutate = .01, ad_inflection = .3, ad_curve = 15, number_of_crossovers = 1)

  #2 parents,crossover = uniform, mutation = fixed, minimize inbreeding = TRUE
  k8 <- breed_next_gen(required_pop = 4, generation_matrix = gene, score_vec = score_vec, number_of_parents = 2,
                       new_parents = a1, mutation = 'fixed', crossover = 'fitness', mutation_rate = .01, minimize_inbreeding = TRUE,
                       ad_max_mutate = .15, ad_min_mutate = .01, ad_inflection = .3, ad_curve = 15, number_of_crossovers = 1)

  expect_equal(sum(rowSums(k1)==0),0)
  expect_equal(sum(rowSums(k2)==0),0)
  expect_equal(sum(rowSums(k3)==0),0)
  expect_equal(sum(rowSums(k4)==0),0)
  expect_equal(sum(rowSums(k5)==0),0)
  expect_equal(sum(rowSums(k6)==0),0)
  expect_equal(sum(rowSums(k7)==0),0)
  expect_equal(sum(rowSums(k8)==0),0)

  expect_true(are_equal(dim(k1),c(4,6)))
  expect_true(are_equal(dim(k2),c(4,6)))
  expect_true(are_equal(dim(k3),c(4,6)))
  expect_true(are_equal(dim(k4),c(4,6)))
  expect_true(are_equal(dim(k5),c(4,6)))
  expect_true(are_equal(dim(k6),c(4,6)))
  expect_true(are_equal(dim(k7),c(4,6)))
  expect_true(are_equal(dim(k8),c(4,6)))

})

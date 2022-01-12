### ----- LOAD GENETIC ALGORITHM ----- ###
source('genetic_algorithm.R')

### ----- Generate_data ----- ###
source('more_controllable_data_gen.R')
data <- generate_data()
response_vec <- as.vector(data[,1])
independent_vars <- as.matrix(data[,2:ncol(data)])

## --- remove data  ---
# rm(data)

library(assertthat)

### --- User Inputs --- ###

out <- select(
  total_number_generations=50,
  number_of_parents = 2,
  pop = 50,
  gene_length=50,
  prob = 0.05,
  user_genes = NULL,
  response_vec = response_vec,
  independent_vars =independent_vars,
  metric = 'R2',
  family = 'gaussian',
  method = 'roulette',
  susN = 1,
  tourn_size = 4,
  mutation = 'adaptive',
  mutation_rate = 0.05,
  minimize_inbreeding = TRUE,
  crossover = 'uniform',
  elitism = TRUE,
  elite_prop = .2,
  ad_max_mutate = .1,
  ad_min_mutate = .01,
  ad_inflection = .3,
  ad_curve = 15,
  custom_function = NULL,
  estimator = NULL,
  pause_length = NULL,
  percent_converge = NULL,
  score_threshold = NULL,
  fittest = 'high')

plot(out[[3]], type = 'l', ylab = 'True Score', xlab = 'generations', col = 'red')

## compare speed between genetic algorithm & step-wise-AIC methods
system.time({genetic_algorithm(
  total_number_generations=50,
  number_of_parents = 2,
  pop=50,
  gene_length=50,
  prob = 0.05,
  user_genes = NULL,
  response_vec = response_vec,
  independent_vars =independent_vars,
  metric = 'R2',
  family = 'gaussian',
  method = 'roulette',
  susN = 1,
  tourn_size = 4,
  mutation = 'fixed',
  mutation_rate = 0.02,
  minimize_inbreeding = FALSE,
  crossover = 'uniform',
  elitism = TRUE,
  elite_prop = .2,
  ad_max_mutate = .1,
  ad_min_mutate = .01,
  ad_inflection = .3,
  ad_curve = 15,
  custom_function = NULL,
  estimator = 'Mean',
  pause_length = 10,
  percent_converge = NULL,
  score_threshold = NULL,
  parallel = TRUE,
  numCores = 4,
  fittest = 'high')})

## minimal number of efforts
out <- genetic_algorithm(
  total_number_generations = 50,
  pop = 50,
  gene_length = 50,
  prob = 0.05,
  response_vec = response_vec,
  independent_vars = independent_vars,
  number_of_parents = 2)

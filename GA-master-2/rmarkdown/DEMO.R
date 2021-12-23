### ----- Help Example 1 ----- ###
set.seed(200)
pop <- 50
gene_length <- 10
response_vec <-rnorm(100)
independent_vars <- matrix(rnorm(100*10),ncol=10)
total_number_generations <-15
out1<- select(pop = pop,gene_length = gene_length,
       response_vec = response_vec,independent_vars = independent_vars,
       total_number_generations = total_number_generations)

plot(out1[[3]],xlab="generation",ylab="AIC, Most Fit Creature")

### ----- Help Example 2 ----- ###
response_vec <-rnorm(100)
independent_vars <- matrix(rnorm(100*50),ncol=50)
pop <- 50
gene_length <-50
elitism <- TRUE
elite_prop <- .05
mutation_rate <- .1
crossover <- 'k_point'
method <-"rank"
number_generations <-50
out2<- select(pop = pop,gene_length = gene_length,
       response_vec = response_vec,independent_vars = independent_vars,
       total_number_generations = total_number_generations,
       elitism = elitism,mutation_rate = mutation_rate,crossover = crossover,
       method = method)
plot(out2[[3]],xlab="generation",ylab="AIC, Most Fit Creature")

### ----- Bigger Example -----###

data <- generate_data()
response_vec <- as.vector(data[,1])
independent_vars <- as.matrix(data[,2:ncol(data)])
data <- generate_data()
response_vec <- as.vector(data[,1])
independent_vars <- as.matrix(data[,2:ncol(data)])
total_number_generations=50
gene_length=50
prob = 0.05
metric = 'AIC'
family = 'gaussian'
susN = 5
tourn_size = 4
elitism = TRUE
ad_max_mutate = .3
ad_min_mutate = .05
ad_inflection = .3
ad_curve = 15
estimator = 'Mean'
pause_length = 10
percent_converge = .10
number_of_parents = 2
pop=50
mutation = 'adaptive'
mutation_rate = 0.10
minimize_inbreeding = TRUE
crossover = 'uniform'
elite_prop = .10
method = 'rank'

out3 <- select(
  total_number_generations=total_number_generations,
  number_of_parents = number_of_parents,
  pop=pop,
  gene_length=gene_length,
  prob = prob,
  user_genes = NULL,
  response_vec = response_vec,
  independent_vars =independent_vars,
  method = method,
  tourn_size = 4,
  mutation = mutation,
  mutation_rate = mutation_rate,
  minimize_inbreeding = minimize_inbreeding,
  crossover = crossover,
  elitism = TRUE,
  elite_prop = elite_prop,
  ad_max_mutate = ad_max_mutate,
  ad_min_mutate = ad_min_mutate,
  ad_inflection = ad_inflection,
  ad_curve = ad_curve,
  estimator = estimator,
  pause_length = pause_length,
  percent_converge = percent_converge)

plot(out3[[3]],xlab="generation",ylab="AIC, Most Fit Creature")

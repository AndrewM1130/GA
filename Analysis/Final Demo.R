### ----- Demo ----- ###

### ----- Standard Parameters ----- ###
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

### ----- Tested Parameters ----- ###

### Test 1 Fast and Furious

number_of_parents = 2
pop=50
mutation = 'fixed'
mutation_rate = 0.15
minimize_inbreeding = FALSE
crossover = 'uniform'
elite_prop = .3
method = 'rank'



### --- User Inputs --- ###
out1 <- vector(mode = 'list',5)
time1 <- out1

for (i in 1:5) {
  out <- select(
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


  out1[[i]]<-out[[1]]

  time1[[i]] <- system.time(select(
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
    percent_converge = percent_converge))

}

### Test 2 More Parents, less elite, less mutation

number_of_parents = 4
pop=50
mutation = 'fixed'
mutation_rate = 0.10
minimize_inbreeding = FALSE
crossover = 'uniform'
elite_prop = .15
method = 'rank'



### --- User Inputs --- ###
out2 <- vector(mode = 'list',5)
time2 <- out2

for (i in 1:5) {
  out <- select(
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


  out2[[i]]<-out[[1]]

  time2[[i]] <- system.time(select(
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
    percent_converge = percent_converge))
}

  ### Test 3 --2 Parents, low elite, adaptive mutation and minimize inbreeding

  number_of_parents = 2
  pop=50
  mutation = 'adaptive'
  mutation_rate = 0.10
  minimize_inbreeding = TRUE
  crossover = 'uniform'
  elite_prop = .10
  method = 'rank'



  ### --- User Inputs --- ###
  out3 <- vector(mode = 'list',5)
  time3 <- out3

  for (i in 1:5) {
    out <- select(
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


    out3[[i]]<-out[[1]]

    time3[[i]] <- system.time(select(
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
      percent_converge = percent_converge))

  }


### ----- Evaluate ----- ###

test_out_results<- function(list_o) {
  iter <- length(list_o)
  final <- iter
  b<-c(rep(1,25),rep(0,25))

    for (i in 1:iter) {
      if (is.vector(list_o[[i]])) {
        a <- list_o[[1]]

        } else {
          a <- list_o[[i]][1,]
          }
      final[i] <-  sum(abs(a-b))^2
    }
  return(mean(final))
  }

average_times <- function(list_t) {
  iter <- length(list_t)
  a<-rep(0,iter)

  for (i in 1:iter) {
    a[i]<-list_t[[i]][3]
  }
  return(mean(a))
}

time <- c(average_times(time1),average_times(time2),average_times(time3))
SSE <- c(test_out_results(out1),test_out_results(out2),test_out_results(out3))

df <- data.frame(x=time,y=SSE,z=c('T-1','T-2','T-3'))
plot(df$x, df$y,xlab='time',ylab='SSE')
text(df$x[2], df$y[2]+50, labels=df$z[2])
text(df$x[1], df$y[1]-50, labels=df$z[1])
text(df$x[3], df$y[3]+50, labels=df$z[3])
title("Comparing 3 Methods")

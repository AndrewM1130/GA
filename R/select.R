#' Select - Runs a Genetic Algorithm
#'
#' This function optimizes a linear regression using a
#' genetic algorithm. User can enter a custom function or
#' select a metric from (AIC, BIC, AICC, R2) to optimize features to include
#'  in the linear regression. This function has a broad range of genetic
#'   algorithm features including  multiple parent section methods,
#'   crossover options, mutation options and other features such
#'  as elitism,  minimizing-inbreeding, using more than two parents
#'   and selecting from a range of early termination options. See below for details)
#'
#'user_genes allows the user specific genes to the initial generation provided that
#' the matrix consist only of 1s and 0s has no all-zero rows, ncol = gene_length,
#'  and has nrow less than or equal the total population pop
#'
#'custom_function allows the user to specify a custom function instead of lm()
#'or glm().  User-provided function must have its first two arguments be
#'(1) generation_matrix and (2) data. Function needs to take a single row of
#'a generation_matrix and return a single numeric without NAs or infinities.
#'
#'Otherwise, user should specify a metric from R2, AIC, BIC, or AICC.
#'The underlying fitness function for R2 is lm() and the underlying fitness
#'function for AIC, BIC, and AICc is glm(). AIC, BIC, and AICC has the option
#'to specify a specific family of functions however, the data needs to be
#'defined across the support for the given family for instance exponential
#'must be greater than 0.
#'
#'Parents are selected via a number of methods:
#'#'Roulette Method selects parents randomly with a probability proportional
#'to their fitness
#'
#'Rank Method selects parents randomly with a probability proportional to
#'the rank of their fitness
#'
#'Tournament uses tourn_size to randomly group that many creatures together.
#'The most fit in that group becomes a parent.
#'
#'Stochastic Universal Sampling works like roulette but also selects a
#'number (susN) more candidates at a fixed width from the first draw to
#'increase diversity
#'
#'crossover Methods
#'Uniform - each gene is randomly selected from 2 or more parents from a
#'PMF proportional to the number of parents.
#'
#'Fitness - each gene is randomly selected from 2 or more parents from
#'a PMF proportional to the parent's fitness.
#'
#'K-Point - parents genes are broken into k+1 segments, then the offspring
#'inherits portions randomly from the parents. Takes the parameter
#'number_of_crossovers which must be less than 1/2 gene_length.
#'
#'For all methods candidate offspring are accepted / rejected so that
#'they don't have completely 0-vector genes.
#'
#'mutation
#'Fixed - Each offspring has a mutation_rate chance of being selected for
#'mutation. Once selected one gene is switched form one to zero or zero to one.
#'
#'Adaptive - the overall population is measured for diversity. As diversity
#'becomes lower, the mutation rate increases. Once selected for mutation a
#'single gene is switched from one to zero or zero to one. The adaptive
#'function is controlled by a simple logistic function with parameters ad_min
#'and ad_max describing the minimum and maximum mutation rates.
#'ad_inflection controls where the logistics point pivots, and ad_curve
#'controls how rapidly the logistics curve increase.
#'
#'For all mutation methods, candidate offspring are accepted / rejected
#'so that they don't have completely 0-vector genes.
#'
#'Minimize inbreeding.
#'his option reduces (though does not remove) the chance of similar
#'creatures creating offspring together. Each parent is assigned
#'new partner(s) randomly from a PMF proportional to how different
#'their genes are. Parents are drawn without replacement so that if they
#'were selected to become parents, they will still remain parents.
#'
#'Elitism preserves the most-fit creatures from each generation.
#'by selecting a number of most fit creatures equal to the
#'ceiling(pop * elite_prop) and guarantees that they make it to the
#'next generation. Additionally, this features makes a copy of each
#'most-fit creature and conducts one gene mutation randomly on each
#'creature. If the copy is more fit than the original, it is returned
#'instead of the original so that the final returned matrix of elite
#' creatures are at least as fit as the incoming elite creatures.
#'
#'Early Termination Conditions:
#'User can define early termination criteria to include a  percentage_convergence
#'such that when diversity falls below a specific threshold the program
#'terminates. User can identify a estimate (from the standard summary()
#'function that if the estimate reaches a specific threshold the
#'program terminates or if the estimate pauses for a certain number of
#'iterations terminates.
#'
#' estimator - cam be nominated from the summary() function  aka Min.,
#'1st Qu., Median, Mean, 3rd Qu., or Max. so for instance estimator=Max.
#'metric = 'AIC' and score_threshold = 500 would terminate when the Max. AIC falls below 500.
#'
#'diversity is defined as sum(unique(genes))/total_genes
#'
#'
#' @param total_number_generations an integer representing the maximum number of iterations
#' @param number_of_parents an integer defining the number of parents per offspring
#' @param pop an integer defining the number of creatures per a generation
#' @param gene_length an integer representing the number of genes per a creature
#' @param response_vec a numeric vector without infinite or NA values
#' @param independent_vars a matrix or dataframe with ncol equal to number of genes and nrow with the same length as the response_vec
#' @param prob a number 0-1 representing the chance that an individual has any given gene
#' @param user_genes  a matrix consisting of 1s and 0s without any rows with all zeros ncol equal to gene_length and nrow less than or equal to pop
#' @param metric a character from the following 'R2', 'AIC', 'BIC', or 'AICc', the statistic LM or GLM is returning
#' @param family the GLM family of distributions (must have the same support as the data for instance exponential cannot have negative numbers)
#' @param method the method of choosing parents, 'roulette', 'rank', 'tournament', and 'sus'
#' @param susN if sus is selected how many parents are chosen scholastically
#' @param tourn_size if tournament is chosen how many candidates are in the tournament
#' @param mutation a character either 'fixed' or 'adaptive' indicating the type of mutation
#' @param minimize_inbreeding a logical indicating whether or not we minimize inbreeding.
#' @param crossover a character that can be 'uniform', 'fitness', or ‘k_point’ indicating how to carry out crossover.
#' @param mutation_rate a numeric between 0 and 1 indicating how often a creature is selected for mutation.
#' @param ad_max_mutate max mutation rate for adaptive mutation, numeric between 0 and 1 and more than ad_min
#' @param ad_min_mutate min mutation rate for adaptive mutation, numeric between 0 and 1 and less than ad_max
#' @param ad_inflection percentage of diversity in population where adaptive mutation begins to increase rapidly
#' @param ad_curve rate that influence how intensely adaptive mutation changes
#' @param number_of_crossovers number of k_point crossovers, needs to be less than gene_length
#' @param elitism a logical representing whether elitism is requested
#' @param elite_prop the proportion of each generation that is selected for elitism
#' @param custom_function user defined custom mast take vector representing a single creatures genes, data, and return a single numeric
#' @param estimator character string from 'Min.','1st Qu.','Median','Mean','3rd Qu.', or 'Max.'. This term controls termination conditions for instance, terminating after the Mean AIC is above a certain threshold
#' @param pause_length a numeric indicating how many iterations without improvement in the estimator before you terminate
#' @param score_threshold a numeric indicating a threshold to cutoff if the estimator reaches that value
#' @param percent_converge percentage of diversity when algorithm terminates for instance .1 will terminate if there are only .90 of the population is represented by the same genes
#' @param fittest a character either 'high' or 'low' defining whether a custom functions
#'
#'
#'
#' @return returns a list consisting of [[1]] the fittest gene, [[2]] the fittest gene's score, [[3]] the most fit score by generation, [[4]] the summary data by generation and [[5]] the final generation's genes.

#' @export
#'
#' @examples
#'
#'pop <- 50
#'gene_length <- 10
#'response_vec <-rnorm(100)
#'independent_vars <- matrix(rnorm(100*10),ncol=10)
#'total_number_generations <-15
#' select(pop = pop,gene_length = gene_length,
#'response_vec = response_vec,independent_vars = independent_vars,
#'total_number_generations = total_number_generations)
#'
#'response_vec <-rnorm(100)
#'independent_vars <- matrix(rnorm(100*50),ncol=50)
#' pop <- 50
#' gene_length <-50
#' elitism <- TRUE
#' elite_prop <- .05
#' mutation_rate <- .1
#' crossover <- 'k_point'
#' method <-"rank"
#' number_generations <-50
#'
#' select(pop = pop,gene_length = gene_length,
#'response_vec = response_vec,independent_vars = independent_vars,
#'total_number_generations = total_number_generations,
#'elitism = elitism,mutation_rate = mutation_rate,crossover = crossover,
#'method = method)
#'
### ----- function ----- ###

#' @import testthat
#' @import assertthat
#' @import stats

select <- function (total_number_generations,
                               number_of_parents = 2,
                               pop,
                               gene_length,
                               response_vec,
                               independent_vars,
                               prob = 0.05,
                               user_genes = NULL,
                               metric = 'AIC',
                               family = 'gaussian',
                               method = 'roulette',
                               susN = 1,
                               tourn_size = 2,
                               mutation = 'fixed',
                               mutation_rate = 0.02,
                               minimize_inbreeding = FALSE,
                               crossover = 'uniform',
                               number_of_crossovers = 1,
                               elitism = TRUE,
                               elite_prop = .05,
                               ad_max_mutate = .1,
                               ad_min_mutate = .01,
                               ad_inflection = .3,
                               ad_curve = 15,
                               custom_function = NULL,
                               estimator = NULL,
                               pause_length = NULL,
                               percent_converge = NULL,
                               score_threshold = NULL,
                               fittest = 'high'
) {


  ### ---- LOAD SOURCE FUNCTIONS ----

  #source('breed_next_gen.R')
  #source('create_initial_generation.R')
  #source('score_fitness_V2.R')
  #source('select_parent.R')
  #source('elitism.R')
  #source('test_custom_function.R')
  #source('terminate.R')


  ### -----Initial Assertions ----- ###

  assert_that(fittest %in% c('low','high'))

  assert_that(is.numeric(number_of_parents) &
                is.count(number_of_parents) &
                number_of_parents >1 &
                length(number_of_parents)==1,
              msg = 'number_of_parents needs
                to be an integer greater than 1')

  assert_that(is.numeric(pop) &
                is.count(pop) &
                pop >=2 & length(pop)==1,
              msg = 'pop needs to be a positive, integer')

  assert_that(is.numeric(gene_length) &
                is.count(gene_length) &
                gene_length >=2 &
                length(gene_length)==1,
              msg = 'gene_length needs to be a positive, integer')

  assert_that(is.numeric(prob) &
                prob>0 & prob<1 & length(prob)==1 &
                is.matrix(prob)==FALSE,
              msg = 'prob needs to be a numeric between 0 and 1')

  if (!is.null(user_genes)) {

    assert_that(is.matrix(user_genes) & all(user_genes==0|user_genes==1) &
                  min(rowSums(user_genes,na.rm = TRUE))>0 &
                  ncol(user_genes)==gene_length & all(!is.na(user_genes)) &
                  all(!is.infinite(user_genes))&
                  ncol(user_genes)<=pop,
                msg = 'user_genes needs to be a matrix of 1s and 0s
              with ncol = gene_length with nrow <= pop')
  }

  assert_that(all(!is.na(response_vec)),msg = 'NAs in response_vec')
  assert_that(is.vector(response_vec),msg = 'response_vec is not a vector')
  assert_that(is.numeric(response_vec),msg = 'response_vec is not a numeric')
  assert_that(all(!is.infinite(response_vec)),
              msg = 'response_vec has one or more infinite elements')


  assert_that(is.data.frame(independent_vars)|is.matrix(independent_vars),msg = 'please ensure independent_vars is a matrix or dataframe')

  for (i in 1:ncol(independent_vars)) {
    assert_that(all(!is.infinite(independent_vars[,i])), msg = 'one ore more element in independent_vars is infinite')
    assert_that(all(!is.na(independent_vars[,i])), msg = 'one ore more element in independent_vars is NA')
  }

  assert_that(ncol(independent_vars)==gene_length,msg = 'independent_vars needs to be gene_length')

  metric_auth <- c('AIC','R2','BIC','AICC')
  assert_that(metric %in% metric_auth, msg = 'please select AIC or R2 for metric')

  family_auth <- c('binomial','gaussian','gamma','inverse.gaussian','poisson')
  assert_that(family %in% family_auth, msg = 'please check you spelling for your distribution')

  assert_that(is.numeric(number_of_parents) &
                is.count(number_of_parents) &
                number_of_parents >1 &
                length(number_of_parents)==1,
              msg = 'number_of_parents needs
                to be an integer greater than 1')

  method_auth <- c('roulette','sus','tournament','rank')
  assert_that(method %in% method_auth,msg = 'method entered is not valid')

  assert_that(is.numeric(susN) &
                is.count(susN) &
                susN >0 & length(susN)==1,
              msg = 'susN needs to be a positive, integer')

  ### ----- Elitism Check ----- ###

  if (elitism == TRUE) {

    pop_required <- pop-ceiling(pop*elite_prop)

  } else {

    pop_required <- pop

  }


  assert_that(susN < (pop_required/2),
              msg = 'susN needs to be less than half of pop_required')

  assert_that(is.numeric(tourn_size) &
                is.count(tourn_size) &
                tourn_size >0 & length(tourn_size)==1,
              msg = 'tourn_size needs to be a positive, integer')

  assert_that(tourn_size < pop,
              msg = 'tourn_size needs to be less than the number of candidates')

  mutation_auth <- c('fixed','adaptive')
  assert_that(mutation %in% mutation_auth,
              msg = 'please select valid mutation method')

  assert_that(is.numeric(mutation_rate) &
                is.matrix(mutation_rate)==FALSE &
                mutation_rate>0 &
                mutation_rate<1 &
                length(mutation_rate)==1,
              msg = 'mutation_rate needs to be a numeric between 0 and 1')

  assert_that(is.logical(minimize_inbreeding)
              & length(minimize_inbreeding)==1,
              msg = 'minimize_inbreeding is not a logical')

  crossover_auth <- c('uniform','fitness','k_point')
  assert_that(crossover %in% crossover_auth,
              msg = 'please select valid crossover method')

  assert_that(is.logical(elitism)
              & length(elitism)==1,
              msg = 'elitism is not a logical')

  assert_that(is.numeric(ad_max_mutate) &
                is.matrix(ad_max_mutate)==FALSE &
                ad_max_mutate>0 &
                ad_max_mutate<1 &
                length(ad_max_mutate)==1,
              msg = 'ad_max_mutate needs to be a numeric between 0 and 1')

  assert_that(is.numeric(ad_min_mutate) &
                is.matrix(ad_min_mutate)==FALSE &
                ad_min_mutate>0 &
                ad_min_mutate<1 &
                length(ad_min_mutate)==1 &
                ad_min_mutate < ad_max_mutate,
              msg = 'ad_min_mutate needs to be a numeric between 0 and 1 and less than ad_max_mutate')

  assert_that(is.numeric(ad_inflection) &
                is.matrix(ad_inflection)==FALSE &
                ad_inflection>0 &
                ad_inflection<1 &
                length(ad_inflection)==1,
              msg = 'ad_inflection needs to be a numeric between 0 and 1')

  assert_that(is.numeric(ad_curve) &
                is.matrix(ad_curve)==FALSE &
                ad_curve>0 &
                length(ad_curve)==1,
              msg = 'ad_curve needs to be a numeric greater than 0')


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



  ### ----- Combine Data ----- ###

  data <- cbind(response_vec,independent_vars)


  ### ---- Create Initial Generation -----

  generation_matrix <- create_initial_generation(pop = pop,
                                                 gene_length = gene_length,
                                                 prob = prob,
                                                 user_genes = user_genes)


  ### ----- Test Custom Function ----- ###

  if (!is.null(custom_function)) {
    out <- test_user_function(func = custom_function,
                              generation_matrix = generation_matrix,
                              data = data)
    if (out[[1]]!=TRUE) {
      stop(out)
    }
  }


  ### ----- Create Summary Matrix -----###

  summary_data_true <- data.frame(matrix(rep(0,6*(total_number_generations+1)),ncol=6))

  names(summary_data_true)<-c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")

  summary_data_scored <- summary_data_true

  ### ----- Iterate the Following ----- ###

  for (k in 1:total_number_generations) {


    ### --- Score Generation --- ###

    score_vec <- score_fitness2(generation_matrix,
                                data,
                                metric = metric,
                                family=family,
                                custom_function = custom_function,
                                fittest = fittest)


    summary_data_true[k,] <- summary(score_vec[[2]])
    summary_data_scored[k,] <- summary(score_vec[[1]])
    score_vec <- score_vec[[1]]

    ### ----- Post_Scoring_Assertions ----- ###


    assert_that(all(score_vec>0),msg = 'score_vec has one or more neg value')
    assert_that(all(!is.na(score_vec)),msg = 'score_vec has one or more NA value')
    assert_that(all(!is.na(score_vec)),msg = 'score_vec has one or more Inf value')
    assert_that(length(score_vec)==pop,msg = 'score_vec needs to be length pop')
    assert_that(is.vector(score_vec),msg = 'score_vec needs to be a vector')
    assert_that(is.numeric(score_vec),msg = 'score_vec needs to be a numeric')


    ### ----- Elitism ----- ###

    if (elitism == TRUE) {
      next_elite_generation <- apply_elitism(data,
                                             generation_matrix,
                                             score_vec,
                                             elite_prop = elite_prop,
                                             mutate = mutation_rate,
                                             metric =metric,
                                             family = family,
                                             custom_function =custom_function,
                                             fittest = fittest)


      ###----Make sure lone vectors are matrices oriented in the right direction -----###

      if (is.matrix(next_elite_generation)) {
        if (ncol(next_elite_generation)==1) {
          next_elite_generation <- t(next_elite_generation)
        }
      }

      if (is.vector(next_elite_generation)) {
        next_elite_generation <- matrix(next_elite_generation,nrow=1)

      } else {
        next_elite_generation <- as.matrix(next_elite_generation)
      }



      ### ----- Post_Elitism_Assertions ----- ###

      assert_that(is.matrix(next_elite_generation),msg = 'next_elite_generation should be a matrix')
      assert_that(all(next_elite_generation==0|next_elite_generation==1),msg ='next_elite_generationshould  be only 1s and 0s')
      assert_that(min(rowSums(next_elite_generation,na.rm = TRUE))>0, msg ='next_elite_generation has genes with all 0s')
      assert_that(all(!is.na(next_elite_generation)) ,msg ='next_elite_generation has NAs')
      assert_that(all(!is.infinite(next_elite_generation)),msg ='next_elite_generation as Inf')
      assert_that(nrow(next_elite_generation)==(pop-pop_required),msg ='next_elite_generation should  be ncol = pop - pop_required')

    }



    ### ----- Select New Parents ----- ###

    new_parents <- select_parent(score_vec = score_vec,
                                 method = method,
                                 number_of_parents = number_of_parents,
                                 susN = susN,
                                 tourn_size = tourn_size,
                                 pop_required = pop_required)




    ### ----- Post_New_Parents _Assertions ----- ###

    assert_that(length(new_parents)==number_of_parents*pop_required, msg = 'new_parents needs length number_of_parents*pop_required')
    assert_that(is.numeric(new_parents), msg = 'new_parents needs to be numeric')
    assert_that(min(new_parents)>0 & min(new_parents) <=pop, msg = 'all new_parents elements need to be indexes between 1 and pop' )


    ### ----- Breed Next Generation ----- ###

    new_generation_matrix <- breed_next_gen(required_pop = pop_required,
                                            generation_matrix = generation_matrix,
                                            score_vec = score_vec,
                                            number_of_parents = number_of_parents,
                                            new_parents = new_parents,
                                            mutation = mutation,
                                            crossover = crossover,
                                            mutation_rate = mutation_rate,
                                            minimize_inbreeding = minimize_inbreeding,
                                            ad_max_mutate = ad_max_mutate,
                                            ad_min_mutate = ad_min_mutate,
                                            ad_inflection = ad_inflection,
                                            ad_curve = ad_curve,
                                            number_of_crossovers = number_of_crossovers)

    ###----Make sure lone vectors are matrices oriented in the right direction -----###

    if (is.matrix(new_generation_matrix)) {
      if (ncol(new_generation_matrix)==1) {
        new_generation_matrix <- t(new_generation_matrix)
      }
    }

    if (is.vector(new_generation_matrix)) {
      new_generation_matrix <- matrix(new_generation_matrix,nrow=1)

    } else {
      new_generation_matrix <- as.matrix(new_generation_matrix)
    }




    ### ----- Post_Breed_Next_Gen_Assertions ----- ###

    assert_that(is.matrix(new_generation_matrix),msg = 'new_generation_matrix should be a matrix')
    assert_that(all(new_generation_matrix==0|new_generation_matrix==1),msg ='new_generation_matrix  be only 1s and 0s')
    assert_that(min(rowSums(new_generation_matrix,na.rm = TRUE))>0, msg ='new_generation_matrix has genes with all 0s')
    assert_that(all(!is.na(new_generation_matrix)) ,msg ='new_generation_matrix has NAs')
    assert_that(all(!is.infinite(new_generation_matrix)),msg ='new_generation_matrix as Inf')
    assert_that(nrow(new_generation_matrix)==(pop_required),msg ='new_generation_matrix should  be length pop_required')


    ### ----- Save New Matrix ----- ###

    if (elitism == TRUE) {

      generation_matrix <- rbind(next_elite_generation,
                                 new_generation_matrix)

    } else { generation_matrix <- new_generation_matrix
    }


    assert_that(nrow(generation_matrix)==pop, msg = 'generation_matrix should be ncol = pop')
    assert_that(is.matrix(generation_matrix),msg = 'generation_matrix should be a matrix')
    assert_that(all(generation_matrix==0|generation_matrix==1),msg ='generation_matrix  be only 1s and 0s')
    assert_that(min(rowSums(generation_matrix,na.rm = TRUE))>0, msg ='generation_matrix has genes with all 0s')
    assert_that(all(!is.na(generation_matrix)) ,msg ='generation_matrix has NAs')
    assert_that(all(!is.infinite(generation_matrix)),msg ='generation_matrix as Inf')


    ### ----- Check Early Termination Conditions ----- ###

    terminate <- see_if_terminate(generation_matrix = generation_matrix,
                                  summary_data_frame = summary_data_true,
                                  iteration = k,estimator = estimator,
                                  pause_length = pause_length,
                                  score_threshold = score_threshold,
                                  percent_converge = percent_converge,
                                  fittest = fittest,
                                  metric = metric)

    if (terminate[[1]]==1) {
      print('Early Termination Criteria Met')
      print(terminate[[2]])
      break()

    }



  }

  ### ----- Consolidate ----- ###

  final_score_vec <- score_fitness2(generation_matrix,
                                    data,
                                    metric = metric,
                                    family=family,
                                    custom_function = custom_function,
                                    fittest = fittest)

  summary_data_true[total_number_generations+1,] <- summary(final_score_vec[[2]])
  summary_data_scored[total_number_generations+1,] <- summary(final_score_vec[[1]])
  final_score_vec <- final_score_vec[[1]]

  fittest_creature <- which(final_score_vec==max(final_score_vec))

  most_fit <- generation_matrix[fittest_creature,]

  if (metric == 'AIC'|metric =='BIC'|metric=='AICC'|fittest=='low') {
    most_fit_score <- summary_data_true$Min.
  } else {most_fit_score <- summary_data_true$Max.}


  output <- list(most_fit, most_fit_score[length(most_fit_score)],
                 most_fit_score, summary_data_true,
                 summary_data_true, generation_matrix)

  if (!is.null(custom_function)) {
    output_name <- 'custom score'
  } else {
    output_name <- metric
  }

  names(output)<-c('fittest gene',paste("fittest_gene",output_name),
                   paste("Most fit",output_name),
                   paste("Summary Data",output_name),
                   'final generation genes')


  return(output)

}


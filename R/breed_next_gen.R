#' Breeds Next Generation
#'
#'The purpose of this function is to take a vector of new parents,
#'their corresponding fitness score vector and their corresponding genes
#'to output the next generation in the genetic algorithm.  It does this via '
#'several crossover methods including uniform, k-point,
#'fitness-uniform  along with their corresponding parameters.
#'After Crossover is complete mutation is applied in two different methods
#'either adaptive which can increase mutation when diversity becomes too
#'low and fixed which maintains a steady mutation rate. Lastly, there is an
#'option to minimize inbreeding prior to crossover and mutation.
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
#' @param generation_matrix matrix with ncol = gene_length and nrow = total number of creatures, consists only of 1s and 0s, no all-zero rows.
#' @param number_of_parents is an integer defining the number of parents per offspring
#' @param new_parents a numeric vector with elements indexing the next generation of parents, length is total population x number_of_parents.
#' @param score_vec a numeric vector with length equal to total population.
#' @param mutation a character either 'fixed' or 'adaptive' indicating the type of mutation
#' @param minimize_inbreeding a logical indicating whether or not we minimize inbreeding.
#' @param crossover a character that can be 'uniform', 'fitness', or ’k_point’ indicating how to carry out crossover.
#' @param mutation_rate a numeric between 0 and 1 indicating how often a creature is selected for mutation.
#' @param ad_max_mutate max mutation rate for adaptive mutation, numeric between 0 and 1 and more than ad_min
#' @param ad_min_mutate min mutation rate for adaptive mutation, numeric between 0 and 1 and less than ad_max
#' @param ad_inflection percentage of diversity in population where adaptive mutation begins to increase rapidly
#' @param ad_curve rate that influence how intensely adaptive mutation changes
#' @param required_pop the number of offspring required
#' @param number_of_crossovers number of k_point crossovers, needs to be less than gene_length
#'
#' @return returns matrix of 1s and 0s with no all-zero rows representing the genes of the next generation after crossover and mutation
#' @export
#'
#' @examples
#' generation_matrix <- matrix(rbinom(10*10,1,.5),nc=10)
#' new_parents <- sample(1:10,20,replace = TRUE)
#' mutation <- 'fixed'
#' crossover <- 'uniform'
#' number_of_parents <- 2
#' required_pop <- 10
#' score_vec <-abs(rnorm(10))
#'
#'next_generation_matrix <- breed_next_gen(generation_matrix = generation_matrix,
#' new_parents = new_parents, mutation = mutation, crossover = crossover,
#'  number_of_parents = number_of_parents,required_pop = required_pop,
#'  score_vec = score_vec)
#'


# ----- Breed Next Generation -----



breed_next_gen <- function(generation_matrix,
                           new_parents,
                           score_vec,
                           number_of_parents,
                           mutation ='fixed',
                           minimize_inbreeding = FALSE,
                           crossover='uniform',
                           mutation_rate = .01,
                           ad_max_mutate = .1,
                           ad_min_mutate = .01,
                           ad_inflection = .3,
                           ad_curve = 15,
                           required_pop,
                           number_of_crossovers = 1
) {

  ### ----- Assertions ----- ###

  crossover_auth <- c('uniform','fitness','k_point')
  mutation_auth <- c('fixed','adaptive')




  assert_that(length(new_parents)==number_of_parents*required_pop &
                is.numeric(new_parents) &
                all(new_parents>0), msg = 'new_parents is a numeric vector of length required*number_of_parents with no 0-values')

  assert_that(sum(is.na(score_vec))==0,msg = 'NAs in score_vec')
  assert_that(sum(score_vec<0)==0,
              msg = 'score_vec contains negative numbers')
  assert_that(is.vector(score_vec),msg = 'score_vec is not a vector')
  assert_that(is.numeric(score_vec),msg = 'score_vec is not a numeric')
  assert_that(sum(is.infinite(score_vec))==0,
              msg = 'score_vec has an infinite element')

  assert_that(is.numeric(number_of_parents) &
                is.count(number_of_parents) &
                number_of_parents >1 & length(number_of_parents)==1,
              msg = 'number_of_parents needs
                to be an integer greater than 1')

  assert_that(crossover %in% crossover_auth,
              msg = 'please select valid crossover method')

  assert_that(mutation %in% mutation_auth,
              msg = 'please select valid mutation method')

  assert_that(is.logical(minimize_inbreeding),
              msg = 'minimize_inbreeding is not a logical')

  assert_that(is.numeric(mutation_rate) &
                is.matrix(mutation_rate)==FALSE &
                mutation_rate>0 &
                mutation_rate<1 &
                length(mutation_rate)==1,
              msg = 'mutation_rate needs to be a numeric between 0 and 1')

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

  assert_that(is.numeric(required_pop) &
                is.count(required_pop) &
                required_pop >=0 & length(required_pop)==1,
              msg = 'required_pop needs to be a positive, integer')


  assert_that(is.numeric(number_of_crossovers) &
                is.count(number_of_crossovers) &
                number_of_crossovers >=0 &
                number_of_crossovers < .5*ncol(generation_matrix) &
                length(number_of_crossovers)==1,
              msg = 'number_of_crossovers needs to be a positive, integer less than 1/2 gene_length')





  ### ----- Initialize ----- ###

  pop <- required_pop

  gene_length <- ncol(generation_matrix)

  new_generation_matrix <- matrix(rep(0,pop*gene_length),nrow=pop)


  ### ----- MINIMIZE INBREEDING ----- ###

  if (minimize_inbreeding == TRUE) {

    hold<-new_parents

    cleared <- rep(0,length(hold))

    cleared[1:pop]<-hold[1:pop]

    hold <- hold[-(1:pop)]


    for (i in 1:(length(hold)-1)) {
      prob <-rowSums(generation_matrix[hold,]-
                       generation_matrix[cleared[i],])^2+1

      cand <- sample(hold,1,replace = TRUE, prob = prob)
      hold <- hold[-match(cand,hold)]
      cleared[pop+i]<-cand

    }

    cleared[pop*number_of_parents] <- hold

    new_parents <- cleared

  }


  ### ------ CROSOVERS ----- ###


  ### ----- UNIFORM for N Parents ---- ###

  if (crossover == 'uniform') {

    for (i in 1:pop) {


      partners <- (1:number_of_parents-1)*pop+i
      parent_genes <- generation_matrix[new_parents[partners],]
      candidate <- rep(0,gene_length)

      count <- 0

      while(count ==0) {

        for (j in 1:gene_length) {

          candidate[j] <-  sample(parent_genes[,j],1)

        }

        if (sum(candidate)>0) {
          count<-1
          new_generation_matrix[i,] <- candidate
        }
      }

    }

  }



  ### ----- UNIFORM-FITNESS for N Parents ---- ###

  if (crossover == 'fitness') {

    for (i in 1:pop) {


      partners <- (1:number_of_parents-1)*pop+i
      parent_genes <- generation_matrix[new_parents[partners],]
      parent_score <- score_vec[new_parents[partners]]
      candidate <- rep(0,gene_length)

      count <- 0

      while(count ==0) {

        for (j in 1:gene_length) {

          candidate[j] <-  sample(parent_genes[,j],1,prob = parent_score)

        }

        if (sum(candidate)>0) {
          count<-1
          new_generation_matrix[i,] <- candidate
        }
      }

    }

  }

  ### ----- K-POINT CROSSOVER FOR N PARENTS ---- ###

  if (crossover == 'k_point') {

    for (i in 1:pop) {

      partners <- (1:number_of_parents-1)*pop+i
      parent_genes <- generation_matrix[new_parents[partners],]
      candidate <- rep(0,gene_length)

      cut_order <- sample(1:number_of_parents,number_of_crossovers,replace = TRUE)

      end <- 0

      while (end ==0) {
        cut_lengths <-runif(number_of_crossovers)

        cut_lengths <- cut_lengths/sum(cut_lengths)

        split_points <- round(gene_length*cut_lengths)

        if ((sum(split_points)==gene_length)&(min(split_points)!=0)) {
          end <- 1
        }
      }

      low <-  1

      high <- split_points[1]

      count <- 0

      while(count ==0) {

        for (k in 1:number_of_crossovers) {

          parent <- cut_order[k]

          candidate[low:high] <- parent_genes[parent,low:high]

          low <- low+split_points[k]

          if (k<number_of_crossovers) {
            high <- high+split_points[k+1]

          }
        }

        if (sum(candidate)>0) {
          count<-1
          new_generation_matrix[i,] <- candidate
        }
      }

    }

  }

  ### ------ MUTATION -----

  ### ----- fixed -----

  if (mutation == 'fixed') {

    for (i in 1:pop) {
      if (runif(1)<mutation_rate) {

        count <- 0

        gene_cand <- 0

        while (count == 0) {

          gene_cand <- new_generation_matrix[i,]

          mutate_point <-sample(1:gene_length,1)

          gene_cand[mutate_point] <- (gene_cand[mutate_point]+1) %% 2

          if (sum(gene_cand)>0) {

            count <- 1

          }
        }


        new_generation_matrix[i,] <- gene_cand
      }

    }
  }

  ### ----- Adaptive Mutation -----

  if (mutation == 'adaptive') {


    diversity <-nrow(unique(new_generation_matrix))/nrow(new_generation_matrix)

    mutation_rate <- (ad_max_mutate+ad_min_mutate)-ad_max_mutate/(1+exp(-ad_curve*(diversity-ad_inflection)))

    for (i in 1:pop) {

      if (runif(1)<mutation_rate) {

        count <- 0

        gene_cand <- 0

        while (count == 0) {

          gene_cand <- new_generation_matrix[i,]

          mutate_point <-sample(1:gene_length,1)

          gene_cand[mutate_point] <- (gene_cand[mutate_point]+1) %% 2

          if (sum(gene_cand)>0) {

            count <- 1

          }
        }


        new_generation_matrix[i,] <- gene_cand
      }

    }
  }

  return(new_generation_matrix)
}

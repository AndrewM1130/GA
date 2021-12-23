#' Processes the Highest Fitness Creatures to ensure that they Make it into the Next Generation
#'
#'Since the primary selection mechanisms of the genetic algorithm
#'are probabilistic, there is a chance that a high performing
#'gene goes extinct causing the gene pool to become less fit.
#'This function counteracts this by ensuring that the
#' best performing creatures survive into future generations.
#' This function selects a number of most fit creatures equal to the
#'ceiling(pop * elite_prop) and guarantees that they make it to the
#'next generation. Additionally, this function makes a copy of each
#'most-fit creature and conducts one gene mutation randomly on each
#'creature. If the copy is more fit than the original, it is returned
#'instead of the original so that the final returned matrix of elite
#' creatures are at least as fit as the incoming elite creatures.
#'
#' @param data a matrix or dataframe with ncol = gene_length plus 1
#' @param gene matrix with all 0s and 1s with ncol = gene_length and nrow = pop
#' @param score_vec a positive, numeric vector with length equal to the total population
#' @param elite_prop a numeric, the percentage of the population that is selected for elitism
#' @param mutate percentage mutation rate
#' @param metric a character, a user specified statistic such as R2, AIC, BIC, or AICc
#' @param family model family corresponding to GLM. Defaults to 'gaussian'
#' @param custom_function defaults to NULL, if defined takes a single row from a generation_matrix and returns a numeric value
#' @param fittest whether a custom function has the highest value corresponding to the fittest or the lowest value
#'
#'
#'
#' @return a matrix containing all 1s and 0s representing the genes of the elite population
#' @export
#'
#' @examples
#' data <- matrix(rnorm(11*3),nc=11)
#' gene <- matrix(rbinom(10*10,1,.5),nc=10)
#' score_vec <-rnorm(10)
#' elite_prop <- .05
#'
#' elite_gen <- apply_elitism(score_vec = score_vec,gene = gene,data = data,elite_prop = elite_prop)
#'

# ELITISM FUNCTION

apply_elitism <- function(data,gene, score_vec, elite_prop = 0.1,
                          mutate = 0.1,metric = 'AIC',family = 'gaussian',custom_function=NULL, fittest){

  # --------------- check elitism proportion & mutation rate ----------------------

  stopifnot(elite_prop >= 0 && elite_prop <= 1,
            mutate >= 0 && mutate <= 1)

  num_elite <- ceiling(elite_prop * length(score_vec))


  #simplified selection and referenced a score_vec that is high for high values and low for low values
  elite_ranking <- cbind(1:length(score_vec),score_vec)

  elite_ranking <- elite_ranking[order(elite_ranking[,2],decreasing = TRUE)]

  elite_parent_index <-elite_ranking[1:num_elite]

  elite_genes <-matrix(gene[elite_parent_index,],ncol = ncol(gene))

  elite_genes_mut <- as.matrix(elite_genes)

  #parents <- matrix(rep(0,num_elite * ncol(gene)), nrow = num_elite)
  #parents2 <- parents

  #ranking <- rank(-score_vec)

  #for (i in 1:num_elite){
  #  loc[i] <- which.max(-score_vec)
  #  parents[i, ] <- gene[loc[i], ]
  #}

  #new_generation_matrix <- parents




  ## --------- mutation here ------------------------


  #mutation_vector <- sample(1:ncol(gene),num_elite,replace = TRUE)


  # fixed for and while loop so that only viable offspring moved on.

  for (i in 1:num_elite){

    count <- 0

    gene_cand <- elite_genes_mut[i,]

    while (count==0) {

      mutate_point <-sample(1:length(gene_cand),1)

      gene_cand[mutate_point] <- (gene_cand[mutate_point]+1) %% 2

      if (sum(gene_cand, na.rm = TRUE) > 0) {
        count <- 1
      }

    }
    elite_genes_mut[i,] <- gene_cand
  }

  ## ------------------ compare mutated parents with parents ------------------

  score1 <- score_fitness2(elite_genes,data,metric,family,custom_function, fittest)

  score2 <- score_fitness2(elite_genes_mut,data,metric,family,custom_function, fittest)

  for (i in 1:num_elite){
    if (score1[[1]][i] < score2[[1]][i]){
      elite_genes[i,] <- elite_genes_mut[i,]
    }
  }

  return(elite_genes)

}


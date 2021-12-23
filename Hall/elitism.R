# ELITISM FUNCTION

apply_elitism <- function(data,gene, score_vec, elite_prop = 0.1,
                          mutate = mutation_rate,metric,family,custom_function, fittest){

  # --------------- check elitism proportion & mutation rate ----------------------

  stopifnot(elite_prop >= 0 && elite_prop <= 1,
            mutate >= 0 && mutate <= 1)

  num_elite <- ceiling(elite_prop * length(score_vec))


  #simplified selection and referenced a score_vec that is high for high values and low for low values
  elite_ranking <- cbind(1:length(score_vec),score_vec)

  elite_ranking <- elite_ranking[order(elite_ranking[,2],decreasing = TRUE)]

  elite_parent_index <-elite_ranking[1:num_elite]

  elite_genes <-matrix(gene[elite_parent_index,],nc = ncol(gene))

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


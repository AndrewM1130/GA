#' Creates the Initial Gene Pool for Genetic Algorithm
#'
#'This  function creates the initial population to be used in
#'the genetic algorithm.  It checks to make sure there are now 0-vectors
#'because 0-vectors are meaningless in the context of LM
#'and will cause LM to fail. Function allows users to specify
#'number of population gene length, probability of genes being represented
#'and specify initial population.

#' @param pop an integer representing the total population maintained for each generation of the genetic algorithm
#' @param gene_length an integer defining how many genes are encoded for each creature, needs to be the same length as the number of columns in the independent variable matrix
#' @param prob a number 0-1 representing the chance that an individual has any given gene
#' @param user_genes a matrix consisting of 1s and 0s without any rows with all zeros ncol equal to gene_length and nrow less than or equal to pop
#'
#' @return returns a matrix of 1s and 0s without any rows with all zeros representing the initial genes for the first generation of the genetic algorithm.
#' @export
#'
#' @examples
#' pop <- 500
#' gene_length <- 50
#' prob <-.25
#'
#' generation_matrix <- create_initial_generation(pop = pop, gene_length = gene_length,prob = prob)

create_initial_generation <- function(pop,
                                      gene_length,
                                      prob=.5,
                                      user_genes = NULL) {


  ### ----- Assertions ----- ###

  assert_that(is.numeric(pop) &
                is.count(pop) &
                pop >=2 & length(pop)==1,
              msg = 'pop needs to be a positive, integer')

  assert_that(is.numeric(gene_length) &
                is.count(gene_length) &
                gene_length >=2 & length(gene_length)==1,
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









  ### ----- Initialize Generation_Matrix ----- ###

  generation_matrix <- matrix(rep(0,pop*gene_length),nrow=pop)


  ### ----- Generate Matrix if there is user-specified genes ----- ###

  suppressWarnings (

    if (!is.null(user_genes)) {

      ## assertions for user input
      stopifnot(is.matrix(user_genes))

      ## ----- code for unique gene sequences by user
      user_rows <- nrow(user_genes)
      genes_needed <- pop - user_rows
      generation_matrix[1:user_rows,] <- user_genes
      pop_gen_count <- user_rows+1


      ### ----- ensure there are no 0-vector genes ----- ###

      while (pop_gen_count <= genes_needed+1) {


        ### ----- generate candidate gene ----- ###

        gene_cand<- rbinom(gene_length,1,prob)

        if (sum(gene_cand) > 0) {

          generation_matrix[pop_gen_count,]<-gene_cand

          pop_gen_count<-pop_gen_count+1

        }

      }

    } else {


      ### ----- generate matrix if no user-specified genes ----- ###

      genes_needed <- pop
      pop_gen_count <- 1


      ### ----- ensure there are no 0-vector genes ----- ###

      while (pop_gen_count <= genes_needed) {


        ### ----- generate candidate gene ----- ###

        gene_cand<- rbinom(gene_length,1,prob)

        if ( sum(gene_cand) > 0) {

          generation_matrix[pop_gen_count,]<-gene_cand

          pop_gen_count<-pop_gen_count+1

        }
      }
    }
  )

  return(generation_matrix)

}


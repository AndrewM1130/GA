#' Scores fitness for an entire generation of creatures
#'
#' This takes a generation_matrix representing all the genes of a single
#' generation, the data, and user input describing the metric such as R2,
#' AIC, BIC, and AICC and can even use a custom function defined by the user.
#' The underlying fitness function for R2 is lm() and the underlying fitness
#' function for AIC, BIC, and AICc is glm().  AIC, BIC, and AICC has the option
#' to specify a specific family of functions however, the data needs to be
#' defined across the support for the given family for instance exponential
#' must be greater than 0.
#'
#' @param gene matrix with all 0s and 1s with ncol = gene_length and nrow = pop
#' @param data a matrix or dataframe with ncol = gene_length plus 1
#' @param metric a character, a user specified statistic such as R2, AIC, BIC, or AICc
#' @param family , model family corresponding to GLM. Defaults to 'gaussian'
#' @param custom_function defaults to NULL, if defined takes a single row from a generation_matrix and returns a numeric value
#' @param fittest whether a custom function has the highest value corresponding to the fittest or the lowest value
#'
#' @return returns a list with the first element being a vector of processed fitness scores to for continued use by the algorithm and element 2 being true (user specified) values user reference, for instance true AIC values
#' @export
#'
#' @examples
#' gene <- matrix(rbinom(2*10,1,.5),nc=10)
#' data <- matrix(rnorm(11*3),nc=11)
#' pop_required <- 10
#' metric <- 'AIC'
#'
#' score_vec <- score_fitness2(data = data,gene = gene,metric = metric)
#'

score_fitness2<-function(gene,data,metric = 'AIC',family='gaussian',custom_function = NULL, fittest = 'high') {


  ### ------ Check to ensure Data is in row order not column order (matters when processing 1x creature----- ####

  if (is.matrix(gene)) {
    if (ncol(gene)==1) {
      gene <- t(gene)
    }
  }

  if (is.vector(gene)) {
    gene <- matrix(gene,nrow=1)

  } else {
    gene <- as.matrix(gene)
  }

  ncgen <- ncol(gene)
  nrgen <- nrow(gene)
  p_vec <-rowSums(gene)
  fitness <- rep(0,nrgen)

  ## ------------------------   Build functions from strings  ---------------------------------


  ### ----- RUN NOT PARALLEL ----- ###



  ### ----- CUSTOM FUNCTION ----- ###
  if (!is.null(custom_function)) {



    for (i in 1:nrgen){

      fitness[i] <- custom_function(gene[i,],data)

    }

    if (fittest == 'low') {
      fitness <- 1/fitness

    }

  } else {
    ### ----- CREATE EXPRESSIONS FOR LM/GLM ----- ###

    todo <- rep(0,nrgen)

    for (i in 1:nrgen) {
      paste_0 <- 'data[,1] ~ '
      for (j in 1:ncgen) {

        if (gene[i,j] == 1) {

          paste_0 <- paste0(paste_0,'data[,',j+1,']', sep  = '+')

        }
      }
      paste_0 <- substr(paste_0, 1, nchar(paste_0)-1)
      todo[i] <- paste_0
    }

    ### ----- R2 AND LM ----- ###
    if (metric == 'R2') {

      data<-as.data.frame(data)
      for (i in 1:nrgen) {
        fitness[i] <-summary(lm(todo[i],data))$r.squared
      }


    } else {

      ### ----- RUN GLM ----- ###
      for (i in 1:nrgen) {
        temp <- glm(as.formula(todo[i]), family = family)
        fitness[i] <- temp$aic
      }

    }
  }



  true_fitness <- fitness

  ### ----- CALC AIC ----- ###

  if (any(fitness<0)) {
    fitness <- fitness-3*min(fitness)
  }

  if (metric == 'AIC' & is.null(custom_function)) {
    fitness <- 1/(fitness)
  }

  ### ----- CALC BIC ----- ###
  if (metric == 'BIC'& is.null(custom_function)) {
    for (i in 1:nrgen) {
      fitness[i] <- 1/(fitness[i]-2*p_vec[i]+p_vec[i]*log(nrow(data)))

    }
  }

  ### ----- CALC AICC ----- ###
  if (metric == 'AICC'& is.null(custom_function)) {
    for (i in 1:nrgen) {
      fitness[i] <- 1/(fitness[i]+(2*p_vec[i]*(p_vec[i]+1))/(nrow(data)-p_vec[i]-1))

    }
  }





  out <- list(fitness,true_fitness)

  return(out)

  ### END OF FUNCTION
}






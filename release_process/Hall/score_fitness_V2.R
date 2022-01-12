score_fitness2<-function(gene,data,
                         metric = 'AIC',
                         family='gaussian',
                         custom_function = NULL, 
                         fittest = 'high') {


  ### ------ Check to ensure Data is in row order not column order (matters when processing 1x creature----- ####

  if (is.matrix(gene)) {
    if (ncol(gene)==1) {
      gene <- t(gene)
    }
  }

  if (is.vector(gene)) {
    gene <- matrix(gene,nr=1)

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






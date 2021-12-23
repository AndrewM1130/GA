## Lower AIC scores are better, and AIC penalizes models that use more parameters. 
## So if two models explain the same amount of variation, the one with fewer parameters 
## will have a lower AIC score and will be the better-fit model.

## family decides the type of regression - documentation here
## https://www.statmethods.net/advstats/glm.html

require(doParallel)
require(iterators)
source('test_custom_function.R')

## test custom functions here
test <- function(gene,data){
  a <- rep(1, nrow(gene))
  return(a)
}

### ----- Score Fitness -----
score_fitness <- function(gene, data, 
                          metric = c('R',' R2', 'AIC', 'BIC', 'AICC'),
                          family = 'gaussian',
                          parallel = FALSE,
                          numCores = 1) {
  
  # ----------- sanity check -------------------------------
  
  # stopifnot(is.matrix(gene), ## Input 'x' must be a matrix.
  #           is.data.frame(data) || is.matrix(data))    ## Input 'data' must be a matrix or data frame
  
  if (!(metric %in% list('R',' R2', 'AIC', 'BIC', 'AICC'))) {
    print('Custom metric detected ...')
    
    if (test_user_function(test,gene,data)$message == 'TRUE'){
      print('Custom fitness function succesful.')
      custom <- metric
      
    }
    
    else {
      print('Aborting genetic algorithm')
    }
    
  }
  
  ## ------------------------ Set up parallel computing abilities  ---------------------------------
  
  ## parallel = FALSE means 1 core
  ## parallel = TRUE will do this section in parallel 
  
  if (parallel == TRUE){
    registerDoParallel(numCores)
  }
  
  if (parallel == FALSE){
    registerDoParallel(1)
  }
   
  ## ------------------------   Build functions from strings  ---------------------------------
  
  fitness <- rep(0,nrow(gene))
  todo <- rep(0,nrow(gene))
  
  for (i in 1:nrow(gene)) {
    paste_0 <- 'data[,1] ~ '
    for (j in 1:ncol(gene)) {
      
      if (gene[i,j] == 1) {
        
        paste_0 <- paste0(paste_0,'data[,',j+1,']', sep  = '+')
        
      }
    }
    paste_0 <- substr(paste_0, 1, nchar(paste_0)-1)
    todo[i] <- paste_0
  }
  
  ## ------------------------- Default fitness functions here  --------------------------------
  
  if (metric == 'R') {
    y <- foreach(i = 1:nrow(gene), .combine = rbind) %dopar% {
      score <- lm(as.formula(as.formula(todo[i], env = parent.frame())))
      fitness[i] <- summary(score)$r.squared
    }  #
    return(as.vector(y))
  }   
  
  if (metric == 'R2') {
    y <- foreach(i = 1:nrow(gene), .combine = rbind) %dopar% {
      score <- lm(as.formula(as.formula(todo[i], env = parent.frame())))
      fitness[i] <- summary(score)$r.squared
    }  
    return(as.vector(y))
   }
  
  if (metric == 'AIC') {
    for (i in 1:nrow(gene)){
      score <- glm(as.formula(todo[i]))
      fitness[i] <- -score$aic #make all AIC negative -> larger AIC is better
    }
    return(fitness)
  }
  
  if (metric == 'BIC') {
    for (i in 1:nrow(gene)){
      score <- glm(as.formula(todo[i]))
      fitness[i] <- -score$aic 
    }
    return(fitness)
  }
  
  if (metric == 'AICC') {
    for (i in 1:nrow(gene)){
      score <- glm(as.formula(todo[i]))
      fitness[i] <- -score$aic 
    }
    return(fitness)
  }
  
# 
#   if (metric == 'AIC') {
#     y <- foreach(i = 1:nrow(gene), .combine = rbind) %dopar% {
#       score <- glm(as.formula(todo[i]))
#       fitness[i] <- -score$aic #make all AIC negative -> larger AIC is better
#     }
#     return(as.vector(y))
#   }
  
  
  ## ------------------------- Custom fitness functions  --------------------------------
  
  if (metric == 'custom') {
      y <- do.call('test', list(gene,data))
  }
    return(as.vector(y))
}


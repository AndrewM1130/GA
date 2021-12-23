### ----- Overall Design ----- ###

### This function's purpose is to take a score vector and to return
### a vector of the next generation of parents.  TO do this to do this it
### the number of parents required per an offspring, and the general
### population required. The resulting new_parents vector will be of length
### pop_required * number_of_parents. 


### ----- Definitions ----- ###

### Roulette Method selects parents randomly with a probability proportional
### to their fitness
### Rank Method selects paretns randomly with a probability proportional to
### the rank of their fitness
### Tournament uses tourn_size to randomly group that many creatures together. 
### The most fit in that group becomes a parent.
### Stochastic Universal Sampling worrks like roulette but also selects a 
### number (susN) more candidates at a fixed width from the first draw to
### increase diversity

### ----- select next parents -----

# This function selects which creatures become parents for the next generation.

select_parent <- function(score_vec,
                          number_of_parents,
                          method = 'roulette', 
                          susN = 5, 
                          tourn_size= 4,
                          pop_required) {
  
  
  ### ----- Assertions ----- ###
  
    assert_that(sum(is.na(score_vec))==0,msg = 'NAs in score_vec')
    assert_that(sum(score_vec<0)==0,
                msg = 'score_vec contains negative numbers')
    assert_that(is.vector(score_vec),msg = 'score_vec is not a vector')
    assert_that(is.numeric(score_vec),msg = 'score_vec is not a numeric')
    assert_that(sum(is.infinite(score_vec))==0,
                msg = 'score_vec has an infinite element')
    
    assert_that(is.numeric(susN) &
                  is.count(susN) &
                  susN >0 & length(susN)==1,
                msg = 'susN needs to be a positive, integer')
    
    assert_that(susN < (pop_required/2),
                msg = 'susN needs to be less than half of pop_required')
    
    assert_that(is.numeric(number_of_parents) & 
                  is.count(number_of_parents) &
                  number_of_parents >1 & 
                  length(number_of_parents)==1,
                msg = 'number_of_parents needs
                to be an integer greater than 1')
    
    assert_that(is.numeric(tourn_size) &
                  is.count(tourn_size) &
                  tourn_size >0 & length(tourn_size)==1,
                msg = 'tourn_size needs to be a positive, integer')
    
    assert_that(tourn_size < length(score_vec),
                msg = 'tourn_size needs to be less than the number of candidates')
    
    assert_that(is.numeric(pop_required) &
                  is.count(pop_required) &
                  pop_required >=0 & length(pop_required)==1,
                msg = 'pop_required needs to be a positive, integer')
    
    method_auth <- c('roulette','sus','tournament','rank')
    
    assert_that(method %in% method_auth,msg = 'method entered is not valid')
    
    
    
  pop <- length(score_vec)
  
  standard_length <- number_of_parents*pop_required  
    
  
  ### --- roulette method reproduction---
  
  if (method == 'roulette') {
    
    new_parents <- sample(1:pop,standard_length,replace = TRUE,score_vec)
    
    return(new_parents)
    
  }  
  
  ### ----- Stochastic Universal Sampling -----
  
  if (method == 'sus') {
    
    sus_length <- susN*ceiling(standard_length/susN)
    
    new_parents <- rep(0,sus_length)
    
    prob <- 1/susN
    
    
    pmf <- score_vec/sum(score_vec)
    
    cdf <- rep(0,pop)
    
    cdf[1] <- pmf[1]
    
    for (i in 2:pop) {
      
      cdf[i]<-cdf[i-1]+pmf[i]
    }
    
    cdf<-cdf/sum(cdf)
    
      
    
    for (i in 1:(sus_length/susN)) {
      
        sus_look_vec <- (0:(susN-1))*prob + rep(1,susN)*runif(1,max = prob)
      
      for (j in 1:susN) {
        
        candidate <- max(sum(sus_look_vec[j]<cdf),1)
        
        new_parents[j+susN*(i-1)] <- candidate 
        
      }
      
      }
    
    new_parents <- new_parents[1:standard_length]
    
    return(new_parents)
    
  }    
  
  ### ----- tournament method -----
  
  if (method == 'tournament') {
    
    new_parents <- rep(0,standard_length)
    
    for (i in 1:(standard_length)) {
      
      tourny <- sample(1:length(score_vec),tourn_size)
      
      new_parents[i] <- which(score_vec==max(score_vec[tourny]))
      
    }
    
    return(new_parents)
    
  }    
  
  #### ----- Rank Method ---
  
  if (method == 'rank') {
    
    rank_prob <- rank(score_vec)
    
    new_parents <- sample(1:pop,standard_length,replace = TRUE,rank_prob)
    
    return(new_parents)
      
    }
  
}

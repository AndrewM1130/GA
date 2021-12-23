
### ----- Overall Design ----- ###

### This  function creates the initial population to be used in 
### the genetic algorithm.  It checks to make sure there are now 0-vectors
### because 0-vectors are meaningless in the context of LM
### and will cause LM to fail. Function allows users to specify 
### number of population gene length, probability of genes being represented 
### and specify initial population.



### ----- Definitions ----- ###

### pop is total population (user inputted)
### gene_length is length of genome (user inputted)
### prob is the probability of a gene being present. default is .5
### user_genes (default is NA) allows the user to specify some initial genes


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

  generation_matrix <- matrix(rep(0,pop*gene_length),nr=pop)

  
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
      
      while (pop_gen_count <= genes_needed) {
      
          
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


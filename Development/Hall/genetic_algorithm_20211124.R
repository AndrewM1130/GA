#NOTES

### Need to Check check individuals during breed next generation to ensure
### they are still viable aka sum(gene) <> 0
### Same while intiiating first population


### --- Libraries ---
library(magrittr)

### ---- LOAD SOURCE FUNCTIONS ----

source('breed_next_gen.R')
source('create_initial_generation.R')
source('score_fitness.R')
source('select_parent.R') 
source('random_data_generator.R')

### ----- user parameters-----

###-----Data Using data --- ###

data <- generate_random_data()
gene_length <- ncol(data)-1
pop <- 100
mutation_rate <- .5
total_number_generations <- 100
initial_gene_prob =.5




### ------------------------------------------------ ###
### we still need need to  make the overall function ### 
### ------------------------------------------------ ###


### ---- Create Initial Generation -----

generation_matrix <- create_initial_generation(pop = pop, gene_length = gene_length, prob = initial_gene_prob)



### ----- Create Summary Matrix -----###

summary_data_frame <- data.frame(matrix(rep(0,6*total_number_generations),nc=6))

names(summary_data_frame)<-c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")



### ----- Iterate the Following ----- ###

for (k in 1:total_number_generations) {
  
  
### --- Score Generation --- ###
  
#  score_vec<-rep(0,pop)
  
#  for (i in 1:pop) {
    
#    score_vec[i] <- score_fitness(generation_matrix[i,],data)
    
#  }

score_vec <- score_fitness(generation_matrix,data)

summary_data_frame[k,] <- summary(score_vec)

  
### ----- Select New Parents ----- ###
  
new_parents <- select_parent(score_vec = score_vec,method = 'roulette')
  
### ----- Breed Next Generation ----- ###

new_generation_matrix <-breed_next_gen(generation_matrix = generation_matrix,
                                       new_parents = new_parents,
                                       mutation = 'fixed',
                                       crossover = 'single_cross',
                                       elitism = TRUE,
                                       mutation_rate = mutation_rate)
  
### ----- Save New Matrix ----- ###

generation_matrix <- new_generation_matrix

  }
  



plot(x=1:total_number_generations,y=summary_data_frame$Max., xlab="generation", ylab="most fit candidate")
### ---- LOAD SOURCE FUNCTIONS ----

source('breed_next_gen.R')
source('create_initial_generation.R')
source('score_fitness.R')
source('select_parent.R') 


### ----- user parameters-----


max_weight <- 400
gene_length<-100
pop <- 500
mutation_rate <- .05
total_number_generations <- 100
initial_gene_prob =.6
item_w <- abs(rnorm(gene_length,mean = 10,sd=3))
item_v <- abs(rnorm(gene_length, mean =100, sd =100))

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
  
  score_vec<-rep(0,pop)
  
  for (i in 1:pop) {
    score_vec[i]<-score_fitness(generation_matrix[i,],item_w = item_w,item_v=item_v,max_weight = max_weight)
    }
  
  summary_data_frame[k,] <- summary(score_vec)

  
### ----- Select New Parents ----- ###
  
new_parents <- select_parent(score_vec = score_vec,method = 'rank')
  
### ----- Breed Next Generation ----- ###

new_generation_matrix <-breed_next_gen(generation_matrix = generation_matrix,
                                       new_parents = new_parents,
                                       mutation = 'fixed',
                                       crossover = 'single_cross',
                                       elitism = TRUE,
                                       mutation_rate = .05)
  
### ----- Save New Matrix ----- ###

generation_matrix <- new_generation_matrix

  }
  



plot(x=1:total_number_generations,y=summary_data_frame$Max., xlab="generation", ylab="most fit candidate")

sum(item_w*new_generation_matrix[1,])
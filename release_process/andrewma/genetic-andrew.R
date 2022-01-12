### --- fitness criteria --- 

item_w <- abs(rnorm(gene_length,mean = 10,sd=3))
item_v <- abs(rnorm(gene_length, mean =100, sd =100))
max_weight <- 400

### --- genetic algorithm criteria ---

gene_length<-10
pop <- 10
mutation_rate <- .05
total_number_generations <- 10

fitness <- function(gene,data){
  
  gene  <-  c(gene)
  
  score <- 0
  
  test <- lm(data[,1:11 * gene]) %>% summary
  
  score <- test$r.squared

  }
  
### --- Generate Population ---



### --- completely random initial candidates

generation_matrix <- matrix(rbinom(pop*gene_length,1,.6),nr=pop)

# --- rejection sampling initial candidates ---

#generation_matrix <- matrix(rep(0,pop*gene_length),nr=pop)

#pop_gen_count <- 1
#tot_while_count<-1

#while (pop_gen_count <= pop) {
  
  #gene_cand<- rbinom(gene_length,1,.6)
  
  #if (fitness(gene_cand,item_w,item_v,max_weight)>100*100*.25) {
  
    #generation_matrix[pop_gen_count,]<-gene_cand
  
    #pop_gen_count<-pop_gen_count+1
  
  #}
 #tot_while_count<-tot_while_count+1 
 
#}



### ---- ITERATE THE FOLLOWING ---

summary_data_frame <- data.frame(matrix(rep(0,6*pop),nc=6))
names(summary_data_frame)<-c("Min.","1st Qu.","Median","Mean","3rd Qu.","Max.")

for (k in 1:total_number_generations) {
  
  
  ### --- Score Generation ---
  
  score_vec<-rep(0,pop)
  
  for (i in 1:pop) {
    score_vec[i]<-fitness(gene = generation_matrix[i,], data = data)
    }
  
  summary_data_frame[k,] <- summary(score_vec)
  
  
  ### --- routlette method reproduction---
  
  # Crerate a pmf then cdf to sample potential parents from
  pmf <- score_vec/sum(score_vec)
  
  cdf<-pmf # preps the cdf see next step
  
  # makes the CDF
  for (i in 2:pop) {
    cdf[i]<-cdf[i]+cdf[i-1]
    }                
  
  
  ### generate new parents 2 for each offspring in next generation
  
  new_parents <- rep(0,2*pop)                     
  
  for (i in 1:(2*pop)) {
    new_parents[i]<-sum(cdf<=runif(1))+1  
    }
  
  
  #single_cross_over
  
  new_generation_matrix <- matrix(rep(0,pop*gene_length),nr=pop)  
  
  for (i in 1:pop) {
    gene_A <- generation_matrix[new_parents[i],]
    gene_B <- generation_matrix[new_parents[100+i],]
    crossover_point <- sample(2:gene_length-1,1) 
    new_generation_matrix[i,]<-c(gene_A[1:crossover_point],gene_B[(crossover_point+1):gene_length])
    }

  # ---- mutate -----

for (i in 1:pop) {
  if (runif(1)<mutation_rate) {
    new_generation_matrix[i,sample(1:gene_length,1)] <- (new_generation_matrix[i,sample(1:gene_length,1)]+1) %% 2
  }
  
}
 ### --- ELITISM OPTION---
  new_generation_matrix[1,] <- generation_matrix[which(score_vec==max(score_vec))[1],]
  
  
  generation_matrix <- new_generation_matrix
  
}

plot(x=1:total_number_generations,y=summary_data_frame$Max., xlab="generation", ylab="most fit candidate")

#sum(*new_generation_matrix[1,]item_w)

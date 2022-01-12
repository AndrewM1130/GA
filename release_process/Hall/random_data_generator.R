# generate random data

### this program first creates a polynomial by generating random coefficients.
### Coefficients and noise are generated using separate normal distirbutions
### the variables used in two way and three way interactions are drawn randomly
### Observations of each variable are a runif(0,1) for each observation.


### User specifies number of variables, number of total observations, 
### number of two way interactions, number of threeway interactions, 
### and mean and varaicne for single coefficents, two way interactions
### three way interations, and noise. 

### Output is a string with data matrix, single coefficients, two_wya interactions
### and threeway interactions





###  ---- USer specified Data ----
generate_random_data<- function() {
number_of_variables <-50
number_of_two_way_interactions <- 20
number_of_three_way_interactions <-5
number_of_observations <-500

single_coefficients_mean <-0
single_coefficients_variance <-30

two_way_interaction_mean <- 10
two_way_interaction_variance <-1

three_way_interaction_mean <- -10
three_way_interaction_variance <- 1

noise_mean <- 0
noise_variance <- 1


### ---- Generate single coefficient interactions ---

single_coefficients <- rnorm(number_of_variables,single_coefficients_mean,sqrt(single_coefficients_variance))



### ---- Generate two way interactions ---

done <- 0
problem_count <- 0

while (done == 0) {
  ### didnt feel like ensuring two way interactiosn were valid so we draw twice
  ### as many, see if they fit, and keep the required amount. 
  sample_cand <- matrix(sample(1:number_of_variables,4*number_of_two_way_interactions, replace = TRUE),nc=2)
  sample_cand <- unique(sample_cand)
  if (nrow(sample_cand)>=number_of_two_way_interactions) {
    done <- 1
  }
  
  if (problem_count==100) {
    done <- 1
    
  }
  problem_count <- problem_count+1
}

two_way_matrix <- matrix(rep(0,3*number_of_two_way_interactions),nc=3)

two_way_matrix[1:number_of_two_way_interactions,1:2]<-sample_cand[1:number_of_two_way_interactions,1:2]

two_way_matrix[,3]<-rnorm(number_of_two_way_interactions,two_way_interaction_mean,sqrt(two_way_interaction_variance))




### ---- Generate three way interactions

done <- 0
problem_count <- 0

while (done == 0) {
  ### didnt feel like ensuring two way interactiosn were valid so we draw twice
  ### as many, see if they fit, and keep the required amount. 
  sample_cand <- matrix(sample(1:number_of_variables,6*number_of_three_way_interactions, replace = TRUE),nc=3)
  sample_cand <- unique(sample_cand)
  if (nrow(sample_cand)>=number_of_three_way_interactions) {
    done <- 1
  }
  
  if (problem_count==100) {
    done <- 1
    
  }
  problem_count <- problem_count+1
}

three_way_matrix <- matrix(rep(0,4*number_of_three_way_interactions),nc=4)

three_way_matrix[1:number_of_three_way_interactions,1:3]<-sample_cand[1:number_of_three_way_interactions,1:3]

three_way_matrix[,4]<-rnorm(number_of_three_way_interactions,three_way_interaction_mean,sqrt(three_way_interaction_variance))


### ---- Generate Observations ----

output_data <- matrix(runif(number_of_observations*number_of_variables),nr=number_of_observations)
                      
one_vec <- rowSums(output_data*single_coefficients)

two_vec <- rep(0,number_of_observations)

for (i in 1:number_of_observations) {
two_vec[i]<-  sum(output_data[i,two_way_matrix[,1]]*output_data[i,two_way_matrix[,2]]*two_way_matrix[,3])
 
}

three_vec <- rep(0,number_of_observations)

for (i in 1:number_of_observations) {
  three_vec[i]<-  sum(output_data[i,three_way_matrix[,1]]*output_data[i,three_way_matrix[,2]]*output_data[i,three_way_matrix[,3]]*three_way_matrix[,4])
  
}

results <-one_vec+two_vec+three_vec+rnorm(number_of_observations,noise_mean,sqrt(noise_variance))

final_out<- list(output_data,results,single_coefficients,two_way_matrix,three_way_matrix)
names(final_out) <- c("data","results","single_coefficents","twp-way interactions","three-way interactions")
return(data.frame(final_out$results,final_out$data))
}

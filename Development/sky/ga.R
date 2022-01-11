# notes:
# - problem statement says the default fitness function should be AIC
#   users should be able to provide fitness function
#   AIC() is in `stats` library
# - selection mechanisms: use `cumsum()` to compute CDF

# fitness function
# default: AIC
# input fun should take in argument m: model
# return fitness score
fitness <- function(m, fun = `AIC`) {
  return(fun(m))
}

# tests:
lm1 <- lm(Fertility ~ ., data = swiss)
fitness(lm1)
fitness(lm1, BIC)

# basic selection methods
# 1. select one parent with prob proportional to fitness
#    select other parent completely random

# test score_vec
# score_vec <- abs(rnorm(10))
n <- length(score_vec) # length of score_vec == pop size
pmf <- score_vec / sum(score_vec)
parent_1 <- which.max(rmultinom(1, 1, pmf))
parent_2 <- which.max(rmultinom(1, 1, rep(1/n, n)))
while (parent_1 == parent_2) {
  parent_2 <- which.max(rmultinom(1, 1, rep(1/n, n)))
}

# 2. select each parent independently with prob proportional
#    to fitness
parent_1 <- which.max(rmultinom(1, 1, pmf))
parent_2 <- which.max(rmultinom(1, 1, pmf))
while (parent_1 == parent_2) {
  parent_2 <- which.max(rmultinom(1, 1, pmf))
}

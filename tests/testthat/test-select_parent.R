test_that("select_new_parents",{

  #score vec
  score_vec <- c(1,2,3,4)

  #select parents

  #2 parents, roulette
  a1 <- select_parent(score_vec = score_vec, number_of_parents = 2,method = 'roulette',susN = 1,tourn_size = 2,pop_required = 4)
  #5 parents, roulette
  a2 <- select_parent(score_vec = score_vec, number_of_parents = 5,method = 'roulette',susN = 1,tourn_size = 2,pop_required = 4)
  #2 parents rank
  a3 <- select_parent(score_vec = score_vec, number_of_parents = 2,method = 'rank',susN = 1,tourn_size = 2,pop_required = 4)
  #2 parents, sus
  a4 <- select_parent(score_vec = score_vec, number_of_parents = 2,method = 'sus',susN = 1,tourn_size = 2,pop_required = 4)
  #2 tournament
  a5 <- select_parent(score_vec = score_vec, number_of_parents = 2,method = 'tournament',susN = 1,tourn_size = 2,pop_required = 4)

  #new_parent correct length
  b1 <- length(a1)
  b2 <- length(a2)
  b3 <- length(a3)
  b4 <- length(a4)
  b5 <- length(a5)

  #new_parent less than 1 (bad index for a parent)
  c1<- min(a1)<1
  c2<- min(a2)<1
  c3<- min(a3)<1
  c4<- min(a4)<1
  c5<- min(a5)<1

  #new_parent greater than 4 (bad index for a parent)
  d1<- max(a1)>4
  d2<- max(a2)>4
  d3<- max(a3)>4
  d4<- max(a4)>4
  d5<- max(a5)>4

  #new_parent correct length
  expect_equal(b1,8)
  expect_equal(b2,20)
  expect_equal(b3,8)
  expect_equal(b4,8)
  expect_equal(b5,8)

  #new_parent less than 1 (bad index for a parent)
  expect_false(c1)
  expect_false(c2)
  expect_false(c3)
  expect_false(c4)
  expect_false(c5)

  #new_parent greater than 4 (bad index for a parent)
  expect_false(d1)
  expect_false(d2)
  expect_false(d3)
  expect_false(d4)
  expect_false(d5)

})

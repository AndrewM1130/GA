test_that("initial generation",{
  a<- create_initial_generation(pop = 50, gene_length = 10,prob = .5)
  b<- create_initial_generation(pop = 50, gene_length = 10,prob = .5,user_genes = matrix(1,nrow=1,ncol=10))


    expect_equal(ncol(a),10)
    expect_equal(ncol(b),10)
    expect_equal(nrow(a),50)
    expect_equal(nrow(b),50)
    expect_error(create_initial_generation(pop = 50, gene_length = 10,prob = .5,user_genes = matrix(1,nrow=1,ncol=11)))
    expect_error(create_initial_generation(pop = 50, gene_length = 10,prob = .5,user_genes = matrix(0,nrow=1,ncol=10)))
    expect_true(all(rowSums(a)>0))
    expect_true(all(rowSums(b)>0))

})

test_that("Similarity computation", code = {
    bics <- list(bicluster(row=c(1, 2), column = c(1, 2)),
                bicluster(row=c(2, 3), column=c(2, 3)))
    
    # Different margins
    sim1 <- similarity_matrix(bics, MARGIN = "both")
    expect_equal(sim1, matrix(c(1, .25, .25, 1), nrow=2, 
                              dimnames=(list(c("bicluster1", "bicluster2"), 
                                             c("bicluster1", "bicluster2")))))
    
    sim2 <- similarity_matrix(bics, MARGIN = "row")
    expect_equal(sim2, matrix(c(1, .5, .5, 1), nrow=2, 
                              dimnames=(list(c("bicluster1", "bicluster2"), 
                                             c("bicluster1", "bicluster2")))))
    
    sim3 <- similarity_matrix(bics, MARGIN = "column")
    expect_equal(sim3, matrix(c(1, .5, .5, 1), nrow=2, 
                              dimnames=(list(c("bicluster1", "bicluster2"), 
                                             c("bicluster1", "bicluster2")))))
    
    # Different metrics
    sim <- similarity_matrix(bics, MARGIN = "both", metric = 2)
    expect_equal(round(sim, 2), matrix(c(1, 0.14, 0.14, 1), nrow=2, 
                                    dimnames=(list(c("bicluster1", 
                                                     "bicluster2"), 
                                                c("bicluster1", 
                                                  "bicluster2")))))
    
    sim <- similarity_matrix(bics, MARGIN = "both", metric = 3)
    expect_equal(sim, matrix(c(1, .25, .25, 1), nrow=2, 
                                       dimnames=(list(c("bicluster1", 
                                                        "bicluster2"), 
                                                      c("bicluster1", 
                                                        "bicluster2")))))
    
    sim <- similarity_matrix(bics, MARGIN = "both", metric = 4)
    expect_equal(sim, matrix(c(1, 0, 0, 1), nrow=2, 
                             dimnames=(list(c("bicluster1", 
                                              "bicluster2"), 
                                            c("bicluster1", 
                                              "bicluster2")))))
    
})

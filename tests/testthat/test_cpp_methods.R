test_that("C++ methods", code = {
    bics <- list(bicluster(row=c(1, 2), column = c(1, 2)),
                bicluster(row=c(2, 3), column=c(2, 3)))
    
    sim1 <- similarity_matrix(bics, MARGIN = "both")
    
    bn <- bicluster_net(adjacency_matrix = sim1, 
                       threshold=.3, 
                       algorithms = "test")
    
    expect_equal(detect_elements(bics), 3)
    
    m <- matrix(c(1:9), nrow=3)
    
    fg <- full_graph(bics, m)
    
    expect_equal(sum(fg), 24)
}
)

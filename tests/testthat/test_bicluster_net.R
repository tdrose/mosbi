test_that("Bicluster network", code = {
    bics <- list(bicluster(row=c(1, 2), column = c(1, 2)),
                bicluster(row=c(2, 3), column=c(2, 3)))
    
    sim1 <- similarity_matrix(bics, MARGIN = "both")
    
    bn <- bicluster_net(adjacency_matrix = sim1, 
                       threshold=.3, 
                       algorithms = "test")
    
    
    expect_equal(apply_threshold(bn), matrix(c(1, 0, 0, 1), nrow=2, 
                             dimnames=(list(c("bicluster1", 
                                              "bicluster2"), 
                                            c("bicluster1", 
                                              "bicluster2")))))
    
    expect_equal(get_adjacency(bn), sim1)
    
    expect_s3_class(bicluster_net_to_igraph(bn), "igraph")
    
    expect_equal(get_louvain_communities(bn), list())
}
)

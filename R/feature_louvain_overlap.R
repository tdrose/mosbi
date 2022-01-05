#' Overlap of features/samples in different louvain communities
#' 
#' The function calculates how often features or samples occur across 
#' all calculated louvain communities
#' 
#' @param overlap_tables List of tables as returned by \code{\link{attr_overlap}}.
#' @param mat The data matrix used as input for the biclustering algorithms.
#' 
#' @return List of tables as returned by \code{\link{attr_overlap}}, 
#' extended by a column showing how often elements occur across all tables.
#' 
#' @examples
#' # a = data.frame(type=c("row", "row", "row", "column", "column", "column"), 
#' #   ID=c(1,2,3,1,2,3), Fraction=c(1,1,1,.5, .5, .5))
#' # b = data.frame(type=c("row", "row", "row", "column", "column", "column"), 
#' #   ID=c(3,2,4,1,5,3), Fraction=c(1,1,1,.5, .5, .5))
#' # inl <- list(a, b)
#' # feature_louvain_overlap(outl, matrix(1:100, nrow=10))
#' 
#' @export
feature_louvain_overlap <- function(overlap_tables, mat){
    
    outl <- overlap_tables
    
    # add new column to all tables
    for (i in 1:length(outl)){
        outl[[i]]$overlap <- 0
    }
    
    # Loop over rows
    for (r in 1:nrow(mat)){
        
        counter <- 0
        
        # Loop over tables
        for(j in 1:length(outl)){
            if(length(outl[[j]]$overlap[outl[[j]]$type=="row" & 
                                        outl[[j]]$ID==r]) > 0){
                counter <- counter + 1
            }
        }
        
        if (counter > 0){
            for(j in 1:length(outl)){
                if(length(outl[[j]]$overlap[outl[[j]]$type=="row" & 
                                            outl[[j]]$ID==r]) > 0){
                    outl[[j]]$overlap[outl[[j]]$type=="row" & 
                                          outl[[j]]$ID==r] <- counter
                }
                
            }
        }
    }
    
    # Loop over columns
    for (c in 1:ncol(mat)){
        counter <- 0
        
        # Loop over tables
        for(j in 1:length(outl)){
            if(length(outl[[j]]$overlap[outl[[j]]$type=="column" & 
                                        outl[[j]]$ID==c]) > 0){
                counter <- counter + 1
            }
        }
        
        if (counter > 0){
            for(j in 1:length(outl)){
                if(length(outl[[j]]$overlap[outl[[j]]$type=="column" & 
                                            outl[[j]]$ID==c]) > 0){
                    outl[[j]]$overlap[outl[[j]]$type=="column" & 
                                          outl[[j]]$ID==c] <- counter
                }
                
            }
        }
    }
    
    return(outl)
}

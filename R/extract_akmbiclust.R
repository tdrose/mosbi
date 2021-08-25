#' @include bicluster.R
NULL

#' Extract a list of bicluster objects from an akmbiclust biclustering object.
#'
#' @param bics A result object from akmbiclust.
#' @param transposed \code{True}, if the bicluster calculation was
#' performed on a tranposed matrix.
#' @param mat Original matrix, that was used for biclustering.
#' @param filterfun A function to filter biclusters.
#' Only if the function returns \code{True}, the bicluster is added to the
#' returned list. The function has to accept a the
#' bicluster (given as submatrix
#' of \code{mat}) \code{filterfun(bicluster_matrix, ...)}.
#' @param ... Other parameters forwarded to the \code{filterfun}.
#' @return A list of \code{\link{bicluster}} objects, which have to be
#' valid (See \code{\link{validate_bicluster}}.
#' 
#' @examples
#' # Function called in
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # Not run: run_akmbiclust(m, k=10)
#' 
#' @export
getAkmbiclustClusters <- function(bics, mat,
                                transposed = FALSE,
                                filterfun = NULL, ...) {
    numClus <- max(bics$row_labels)

    tmp <- bicluster()
    res <- list()

    for (i in seq(1, numClus)) {
        if (transposed) {
            tmp <- bicluster(
                column = (seq(1, ncol(mat)))[bics$row_labels == i],
                row = (seq(1, nrow(mat)))[bics$col_labels == i],
                algorithm = "akmbiclust"
            )
        } else {
            tmp <- bicluster(
                row = (seq(1, nrow(mat)))[bics$row_labels == i],
                column = (seq(1, ncol(mat)))[bics$col_labels == i],
                algorithm = "akmbiclust"
            )
        }
        tmp <- set_bicluster_names(tmp, mat)
        # Check if cluster fits the constraints
        if (validate_bicluster(tmp)) {
            if (is.null(filterfun)) {
                res[[length(res) + 1]] <- tmp
            } else {
                if (filterfun(bicluster_to_matrix(mat, tmp), ...)) {
                    res[[length(res) + 1]] <- tmp
                }
            }
        }
    }
    return(res)
}

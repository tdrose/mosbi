#' @include bicluster.R
NULL

NoBiclust <- function(bics) {
    return(dim(bics@RowxNumber)[2])
}

#' Extract a list of bicluster objects from a biclust object.
#'
#' @param bics A biclust object.
#' @param transposed \code{True}, if the bicluster calculation was
#' performed on a tranposed matrix.
#' @param mat Original matrix, that was used for biclustering.
#' @param method Name of the used biclustering algorithm. Should be one of
#' the following: "biclust", "biclust-bimax", "biclust-cc", "biclust-plaid",
#' "biclust-quest", "biclust-spectral", "biclust-xmotifs" or "biclust-qubic",
#' "biclust-unibic".
#' @param filterfun A function to filter biclusters. Only if the function
#' returns \code{True}, the bicluster is added to the returned list.
#' The function has to accept a the bicluster (given as submatrix
#' of \code{mat}) \code{filterfun(bicluster_matrix, ...)}.
#' @param ... Other parameters forwarded to the \code{filterfun}.
#' @return A list of \code{\link{bicluster}} objects, which have
#' to be valid (See \code{\link{validate_bicluster}}.
#' 
#' @examples
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # res <- biclust::biclust(m, method = biclust::BCBimax())
#' # getBiclustClusters(res, m)
#' 
#' @export
getBiclustClusters <- function(bics, mat, method = "biclust",
                            transposed = FALSE,
                            filterfun = NULL, ...) {
    if (!requireNamespace("biclust")) {
        stop("Package biclust not available.
        Please install before using this funcion.")
    }


    numClus <- NoBiclust(bics)

    tmp <- bicluster()
    res <- list()

    # Biclust returns boolean bicluster affiliation
    rows <- seq(1, dim(bics@RowxNumber)[1])
    cols <- seq(dim(bics@NumberxCol)[2])

    for (i in seq(1, numClus)) {

        # generate bicluster object
        if (transposed) {
            tmp <- bicluster(
                column = rows[bics@RowxNumber[, i]],
                row = cols[bics@NumberxCol[i, ]],
                algorithm = method
            )
        } else {
            tmp <- bicluster(
                row = rows[bics@RowxNumber[, i]],
                column = cols[bics@NumberxCol[i, ]],
                algorithm = method
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

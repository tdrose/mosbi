#' @include bicluster.R
NULL

NoIsa <- function(bics) {
    return(dim(bics$rows)[2])
}

#' Extract a list of bicluster objects from an isa2 biclustering object.
#'
#' @param bics A biclust object.
#' @param transposed \code{True}, if the bicluster calculation was performed
#' on a tranposed matrix.
#' @param mat Original matrix, that was used for biclustering.
#' @param filterfun A function to filter biclusters. Only if the function
#' returns \code{True}, the bicluster is added to the returned list.
#' The function has to accept a the bicluster (given as submatrix
#' of \code{mat}) \code{filterfun(bicluster_matrix, ...)}.
#' @param ... Other parameters forwarded to the \code{filterfun}.
#' @return A list of \code{\link{bicluster}} objects, which have
#' to be valid (See \code{\link{validate_bicluster}}.
#' 
#' @examples
#' # Function part of:
#' m <- matrix(rnorm(10000), nrow=100)
#' # Not run: run_isa(m)
#' 
#' @export
getIsaClusters <- function(bics, mat,
                            transposed = FALSE,
                            filterfun = NULL, ...) {
    if (!requireNamespace("isa2")) {
        stop("Package isa2 not available.
        Please install before using this funcion.")
    }

    numClus <- NoIsa(bics)

    tmp <- bicluster()
    res <- list()

    # Isa provides factors as bicluster affiliation
    rows <- seq(1, dim(bics$rows)[1])
    cols <- seq(1, dim(bics$columns)[1])

    for (i in seq(1, numClus)) {
        if (transposed) {
            tmp <- bicluster(
                column = rows[as.logical(bics$rows[, i])],
                row = cols[as.logical(bics$columns[, i])],
                algorithm = "isa2"
            )
        } else {
            tmp <- bicluster(
                row = rows[as.logical(bics$rows[, i])],
                column = cols[as.logical(bics$columns[, i])],
                algorithm = "isa2"
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

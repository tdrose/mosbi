#' @include bicluster.R
NULL

NoBicARE <- function(bics) {
    return(dim(bics$bicCol)[1])
}

#' Extract a list of bicluster objects from an BicARE biclustering object.
#'
#' @param bics A BicARE bicluster object.
#' @param mat Original matrix, that was used for biclustering.
#' @param transposed \code{True}, if the bicluster calculation was
#' performed on a tranposed matrix.
#' @param filterfun A function to filter biclusters. Only if the
#' function returns \code{True}, the bicluster is added to the returned list.
#' The function has to accept a the bicluster (given as submatrix
#' of \code{mat}) \code{filterfun(bicluster_matrix, ...)}.
#' @param ... Other parameters forwarded to the \code{filterfun}.
#' @return A list of \code{\link{bicluster}} objects, which have to
#' be valid (See \code{\link{validate_bicluster}}.
#' 
#' @examples
#' # Note that BicARE packackage is not included in the mosbi package
#' m <- matrix(rnorm(10000), nrow=100)
#' # res <- BicARE::FLOC(m)
#' # getBicAREbiclusters(res, m)
#' 
#' @export
getBicAREbiclusters <- function(bics, mat,
                                transposed = FALSE,
                                filterfun = NULL, ...) {
    numClus <- NoBicARE(bics)

    tmp <- bicluster()
    res <- list()

    # BicARE returns boolean bicluster affiliation
    rows <- seq(1, (dim(bics$bicRow)[2]))
    cols <- seq(1, (dim(bics$bicCol)[2]))

    for (i in seq(1, numClus)) {

        # generate bicluster object
        if (transposed) {
            tmp <- bicluster(
                column = rows[as.logical(bics$bicRow[i, ])],
                row = cols[as.logical(bics$bicCol[i, ])],
                algorithm = "BicARE"
            )
        } else {
            tmp <- bicluster(
                row = rows[as.logical(bics$bicRow[i, ])],
                column = cols[as.logical(bics$bicCol[i, ])],
                algorithm = "BicARE"
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

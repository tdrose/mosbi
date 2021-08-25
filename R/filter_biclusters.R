
#' Filter biclusters based on a user defined filter function.
#' 
#' If the function returns \code{True}, the bicluster is added to the output
#' list of biclusters.
#' Every bicluster is validated, before forwarding to the filter-function.
#'
#' @param bics A list of valid bicluster objects.
#' @param mat Original matrix, that was used for biclustering.
#' @param filterfun A function to filter biclusters. Only if the function
#' returns \code{True}, the bicluster is added to the returned list.
#' The function has to accept a the bicluster (given as submatrix
#' of \code{mat}) \code{filterfun(bicluster_matrix, ...)}.
#' @param ... Other parameters forwarded to the \code{filterfun}.
#' @return A filtered list ob bicluster objects with
#' length(returned_list)<=length(bics).
#' 
#' @examples
#' # m <- matrix(runif(100), nrow=10)
#' b <- list(bicluster(row=c(3,4), column=c(3,4)),
#'     bicluster(row=c(3,4,5,6), column=c(3,4,5,6)),
#'     bicluster(row=c(3,4,5,6), column=c(3,6)))
#' # filter_biclusters(b, m, function(x) sum(x) < 0)
#'
#' @export
filter_biclusters <- function(bics, mat, filterfun, ...) {
    out_list <- list()
    for (i in seq(1, length(bics))) {
        if (validate_bicluster(bics[[i]])) {
            if (filterfun(bicluster_to_matrix(mat, bics[[i]]), ...)) {
                out_list[[length(out_list) + 1]] <- bics[[i]]
            }
        }
    }

    return(out_list)
}

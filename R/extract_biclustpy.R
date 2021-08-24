#' @include bicluster.R
NULL

#' Extract a list of bicluster objects from an biclustpy output file.
#'
#' @param bics A biclust object.
#' @param transposed \code{True}, if the bicluster calculation was performed
#' on a tranposed matrix.
#' @param mat Original matrix, that was used for biclustering.
#' @param filterfun A function to filter biclusters. Only if the
#' function returns \code{True}, the bicluster is added to the returned list.
#' The function has to accept a the bicluster (given as submatrix
#' of \code{mat}) \code{filterfun(bicluster_matrix, ...)}.
#' @param ... Other parameters forwarded to the \code{filterfun}.
#' @return A list of \code{\link{bicluster}} objects, which have
#' to be valid (See \code{\link{validate_bicluster}}.
#' 
#' @examples
#' m <- matrix(rnorm(10000), nrow=100)
#' getBiclustpyClusters("PathToFileOfBiclustpyResults", m)
#' 
#' @export
getBiclustpyClusters <- function(bics, mat,
                                transposed = FALSE,
                                filterfun = NULL, ...) {
    x <- xml2::read_xml(bics)
    b2 <- xml2::xml_children(x)
    numClus <- length(b2)
    tmp <- bicluster()
    res <- list()
    for (i in seq(1, numClus)) {
        if (length(xml2::as_list(xml2::xml_children(b2[i]))[[1]]) == 0) {
            tmp_r <- as.numeric(c())
        } else {
            tmp_r <- as.numeric(strsplit(xml2::as_list(
                xml2::xml_children(b2[i])
            )[[1]][[1]], " ")[[1]]) + 1
        }
        if (length(xml2::as_list(xml2::xml_children(b2[i]))[[2]]) == 0) {
            tmp_c <- as.numeric(c())
        } else {
            tmp_c <- as.numeric(strsplit(xml2::as_list(
                xml2::xml_children(b2[i])
            )[[2]][[1]], " ")[[1]]) + 1
        }
        # generate bicluster object
        if (transposed) {
            tmp <- bicluster(
                column = tmp_r,
                row = tmp_c, algorithm = "biclustpy")
        } else {
            tmp <- bicluster(
                row = tmp_r,
                column = tmp_c, algorithm = "biclustpy")
        }
        tmp <- set_bicluster_names(tmp, mat)
        # Check if cluster fits the constraints
        if (validate_bicluster(tmp)) {
            if (is.null(filterfun)) {
                res[[length(res) + 1]] <- tmp
            } else {
                if (filterfun(bicluster_to_matrix(mat, tmp), ...)) {
                    res[[length(res) + 1]] <- tmp}}}}
    return(res)
}

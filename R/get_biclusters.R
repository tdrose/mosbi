#' @include bicluster.R
NULL

is_biclust <- function(method){
    return((method == "biclust") | (method == "biclust-bimax") |
               (method == "biclust-cc") |
               (method == "biclust-plaid") | (method == "biclust-quest") |
               (method == "biclust-qubic") | (method == "biclust-spectral") |
               (method == "biclust-xmotifs") | (method == "biclust-unibic"))
}

extract_bf <- function(bics, mat, method, transposed, filterfun, ...){
    b2 <- getallBFClusters(bics)
    numClus <- NoBFBiclusters(bics)
    tmp <- list()
    for (i in seq(1, numClus)) {
        if (transposed) {
            if (validate_bicluster(transpose_bicluster(b2[[i]]))) {
                if (is.null(filterfun)) {
                    tmp[[length(tmp) + 1]] <- set_bicluster_names(
                        transpose_bicluster(b2[[i]]), mat
                    )
                } else {
                    if (filterfun(bicluster_to_matrix(mat, tmp), ...)) {
                        tmp[[length(tmp) + 1]] <- set_bicluster_names(
                            transpose_bicluster(b2[[i]]), mat)}}}
        } else {
            if (validate_bicluster(b2[[i]])) {
                if (is.null(filterfun)) {
                    tmp[[length(tmp) + 1]] <- set_bicluster_names(
                        b2[[i]], mat)
                } else {
                    if (filterfun(bicluster_to_matrix(mat, tmp), ...)) {
                        tmp[[length(tmp) + 1]] <- set_bicluster_names(
                            b2[[i]], mat)}}}}}
    
    return(tmp)
}

#' Extract biclusters from different algorithms/packages
#'
#' Converts biclusters output of different algorithms/packages in to lists
#' of \link{bicluster} objects. Many algoritms can be directly executed using
#' the \code{run_...} methods from this package.
#' This directly returns the converted results. Not all algorithms are shipped
#' with this package, like Bi-Force, which is running in Java as a standalone
#' tool or BicARE,
#' which required an full import using \code{library(BicARE)} in order to run.
#' But their results can be converted using this function.
#' @param bics A resulting object from a biclustering
#' algorithm (extracted biclusters for fabia) or filename for stored
#' biclustering results.
#' @param mat Original matrix, that was used for biclustering.
#' @param method Used biclustering package. One of "biclust"
#'                (can be further specified as "biclust-bimax",
#'                "biclust-cc", "biclust-plaid", "biclust-quest",
#'                "biclust-qubic",
#'                "biclust-spectral", "biclust-xmotifs", "biclust-unibic"),
#'                "BicARE", "isa", "fabia", "biforce", "biclustpy", "qubic2"
#'                or "akmbiclust".
#' @param transposed Indicate, whether a transposed version of the matrix is
#' used for biclustering. The \code{matrix} should not be transposed,
#' when this argument is set to \code{True}.
#' @param filterfun A function to filter biclusters.
#' Only if the function returns \code{True}, the bicluster is added to
#' the returned list. The function has to accept a bicluster
#' (given as submatrix of \code{mat}) \code{filterfun(bicluster_matrix, ...)}.
#' @param ... Other parameters forwarded to the \code{filterfun}.
#' @return A list of \code{\link{bicluster}} objects, which are
#' valid (See \code{\link{validate_bicluster}}).
#' 
#' @examples
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # res <- isa2::isa(m)
#' # get_biclusters(res, m, "isa")
#' 
#' @export
get_biclusters <- function(bics, mat, method,
                            transposed = FALSE, filterfun = NULL, ...) {
    if (is_biclust(method)) {
        tmp <- getBiclustClusters(bics, mat = mat, method = method,
            transposed = transposed, filterfun = filterfun, ...)
        return(tmp)
    } else if (method == "BicARE") {
        tmp <- getBicAREbiclusters(bics, mat = mat, transposed = transposed,
            filterfun = filterfun, ...)
        return(tmp)
    } else if (method == "isa") {
        tmp <- getIsaClusters(bics, transposed = transposed, mat = mat,
            filterfun = filterfun, ...)
        return(tmp)
    } else if (method == "fabia") {
        tmp <- getFabiaClusters(bics, transposed = transposed, mat = mat,
            filterfun = filterfun, ...)
        return(tmp)
    } else if (method == "biforce") {
        return(extract_bf(bics, mat, method, transposed, filterfun, ...))
    } else if (method == "biclustpy") {
        tmp <- getBiclustpyClusters(bics,
            transposed = transposed, mat = mat, filterfun = filterfun, ...)
        return(tmp)
    } else if (method == "qubic2") {
        qb2 <- getQUBIC2biclusters(bics, transposed = transposed)
        tmp <- list()
        for (i in seq(1, length(qb2))) {
            if (validate_bicluster(qb2[[i]])) {
                if (is.null(filterfun)) {
                    tmp[[length(tmp) + 1]] <- set_bicluster_names(
                        qb2[[i]], mat)
                } else {
                    if (filterfun(bicluster_to_matrix(mat, tmp), ...)) {
                        tmp[[length(tmp) + 1]] <- set_bicluster_names(
                            qb2[[i]], mat)}}}}
        return(tmp)
    } else if (method == "akmbiclust") {
        tmp <- getAkmbiclustClusters(bics, transposed = transposed, mat = mat,
            filterfun = filterfun, ...)
        return(tmp)
    } else {
        print("Method not available")
        return(list())}}

#' @include bicluster.R
NULL

NoFabia <- function(bics) {
    return(dim(bics$bic)[1])
}

#' Extract a list of bicluster objects from an fabia biclustering object.
#'
#' @param bics Extracted fabia biclusters.
#' @param transposed \code{True}, if the bicluster calculation was performed
#' on a tranposed matrix.
#' @param mat Original matrix, that was used for biclustering.
#' @param filterfun A function to filter biclusters. Only if the function
#' returns \code{True}, the bicluster is added to the returned list.
#' The function has to accept a the bicluster (given as submatrix
#' of \code{mat}) \code{filterfun(bicluster_matrix, ...)}.
#' @param ... Other parameters forwarded to the \code{filterfun}.
#' @return A list of \code{\link{bicluster}} objects, which have to
#' be valid (See \code{\link{validate_bicluster}}.
#' 
#' @examples
#' # m <- matrix(rnorm(10000), nrow=100)
#' # res <- fabia::extractBic(fabia::fabia(m, p=5))
#' # getFabiaClusters(res, m)
#' 
#' 
#' @export
getFabiaClusters <- function(bics, mat, transposed = FALSE,
                            filterfun = NULL, ...) {
    if (!requireNamespace("fabia")) {
        stop("Package fabia not available.
        Please install before using this funcion.")}
    numClus <- NoFabia(bics)
    tmp <- bicluster()
    res <- list()
    rows <- seq(1, dim(mat)[1])
    if (is.null(rownames(mat))) {
        if (!transposed) {
            names(rows) <- paste0("gene", rows)
        } else {
            names(rows) <- paste0("sample", rows)
        }
    } else {names(rows) <- rownames(mat)}
    cols <- seq(1, dim(mat)[2])
    if (is.null(colnames(mat))) {
        if (!transposed) {
            names(cols) <- paste0("sample", cols)
        } else {names(cols) <- paste0("gene", cols)}
    } else {names(cols) <- colnames(mat)}
    for (i in seq(1, numClus)) {
        if (transposed) {
            tmp <- bicluster(
                column = cols[bics$bic[i, ]$bixn],
                row = rows[bics$bic[i, ]$biypn],
                algorithm = "fabia")
        } else {
            tmp <- bicluster(
                row = rows[bics$bic[i, ]$bixn],
                column = cols[bics$bic[i, ]$biypn],
                algorithm = "fabia")}
        tmp <- set_bicluster_names(tmp, mat)
        if (validate_bicluster(tmp)) {
            if (is.null(filterfun)) {
                res[[length(res) + 1]] <- tmp
            } else {
                if (filterfun(bicluster_to_matrix(mat, tmp), ...)) {
                    res[[length(res) + 1]] <- tmp}}}}
    return(res)}

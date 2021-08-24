#' @include bicluster.R
NULL

#' Convert a bicluster object to an acutal submatrix of the original matrix.
#' @param m Matrix on which the bicluster was computed
#' @param bic Bicluster object
#' @return A matrix.
#' 
#' @examples
#' bicluster_to_matrix(matrix(seq(1:16), nrow=4), 
#'     bicluster(row=c(1,2), column=c(1,2)))
#' @export
methods::setGeneric("bicluster_to_matrix", function(m, bic) {
    methods::standardGeneric("bicluster_to_matrix")
})

#' Convert a bicluster object to an acutal submatrix of the original matrix.
#' @param m Matrix on which the bicluster was computed
#' @param bic Bicluster object
#' @return A matrix.
#' 
#' #' @examples
#' bicluster_to_matrix(matrix(seq(1:16), nrow=4), 
#'     bicluster(row=c(1,2), column=c(1,2)))
#'     
#' @export
methods::setMethod(
    "bicluster_to_matrix",
    c(m = "matrix", bic = "bicluster"),
    function(m, bic) {
        # if(!validate_bicluster(bic)){
        #  stop("Not a valid bicluster")
        # }
        out <- m[bic@row, bic@column]
        if (is.null(rownames(out))) {
            rownames(out) <- as.character(bic@row)
        }
        if (is.null(colnames(out))) {
            colnames(out) <- as.character(bic@column)
        }
        return(out)
    }
)

#' Get the dimensions of a bicluster.
#'
#' @param x A bicluster object.
#' @return A numeric vector with the lengths of the rows and
#' columns of the bicluster.
#' 
#' @examples
#' dim(bicluster(row=c(1,2), column=c(1,2)))
#' @export
methods::setMethod(
    "dim",
    c(x = "bicluster"),
    function(x) {
        return(c(length(x@row), length(x@column)))
    }
)

#' Add row-/colnames to a bicluster object.
#'
#' @param bic A bicluster object.
#' @param m The matrix, that was used for the
#' biclustering. (Works only if matrix has row-/colnames.)
#' @return The updated bicluster object.
#' 
#' @examples
#' m <- matrix(c(1,2,3,4), nrow=2)
#' rownames(m) <- c("r1", "r2")
#' rownames(m) <- c("c1", "c2")
#' set_bicluster_names(bicluster(row=c(1,2), column=c(1,2)), m)
#' 
#' @export
methods::setGeneric("set_bicluster_names", function(bic, m) {
    methods::standardGeneric("set_names")
})

#' Add row-/colnames to a bicluster object.
#'
#' @param bic A bicluster object.
#' @param m The matrix, that was used for the
#' biclustering. (Works only if matrix has row-/colnames.)
#' @return The updated bicluster object.
#' 
#' #' @examples
#' m <- matrix(c(1,2,3,4), nrow=2)
#' rownames(m) <- c("r1", "r2")
#' rownames(m) <- c("c1", "c2")
#' set_bicluster_names(bicluster(row=c(1,2), column=c(1,2)), m)
#' 
#' @export
methods::setMethod(
    "set_bicluster_names",
    c(bic = "bicluster", m = "matrix"),
    function(bic, m) {
        if (!is.null(rownames(m))) {
            bic@rowname <- rownames(m)[bic@row]
        }
        if (!is.null(colnames(m))) {
            bic@colname <- colnames(m)[bic@column]
        }
        return(bic)
    }
)

#' Plot a heatmap of a bicluster
#'
#' Uses the \code{stats::\link[stats]{heatmap}} function.
#'
#' @param bic A bicluster object.
#' @param m The matrix, that was used for the
#' biclustering. (Works only if matrix has row-/colnames.)
#' @param ... Arguments forwarded to \code{stats::\link[stats]{heatmap}}.
#' @return A plot object
#' 
#' @examples
#' m <- matrix(c(1,2,3,4), nrow=2)
#' rownames(m) <- c("r1", "r2")
#' rownames(m) <- c("c1", "c2")
#' bicluster_heatmap(bicluster(row=c(1,2), column=c(1,2)), m)
#' 
#' @export
methods::setGeneric("bicluster_heatmap", function(bic, m, ...) {
    methods::standardGeneric("bicluster_heatmap")
})

#' Plot a heatmap of a bicluster
#'
#' Uses the \code{stats::\link[stats]{heatmap}} function.
#'
#' @param bic A bicluster object.
#' @param m The matrix, that was used for the
#' biclustering. (Works only if matrix has row-/colnames.)
#' @param ... Arguments forwarded to \code{stats::\link[stats]{heatmap}}.
#' @return A plot object
#' 
#' @examples
#' m <- matrix(c(1,2,3,4), nrow=2)
#' rownames(m) <- c("r1", "r2")
#' rownames(m) <- c("c1", "c2")
#' bicluster_heatmap(bicluster(row=c(1,2), column=c(1,2)), m)
#' 
#' @export
methods::setMethod(
    "bicluster_heatmap",
    c(bic = "bicluster", m = "matrix"),
    function(bic, m, ...) {
        return(stats::heatmap(bicluster_to_matrix(m, bic, ...)))
    }
)

#' Randomize a matrix
#'
#' Randomize a matrix bu shuffling all rows and columns.
#'
#' @param m A matrix.
#' @return A randomized version of the input matrix.
#' 
#' @examples
#' m <- matrix(c(1,2,3,4), nrow=2)
#' randomize_matrix(m)
#' @export
randomize_matrix <- function(m) {
    out <- matrix(sample(m), nrow = nrow(m))
    rownames(out) <- rownames(m)
    colnames(out) <- colnames(m)
    return(out)
}

#' Get Algorithms
#'
#' Get a unique vector of algorithms from a
#' list of \code{\link{bicluster}} objects.
#'
#' @param bics a list of \code{\link{bicluster}} objects.
#' @return A character vector with algorithm names
#' 
#' @examples
#' b <- list(bicluster(row=c(1,2,3,4), column=c(1,2,3,4), algorithm="isa"),
#'     bicluster(row=c(3,4,5,6), column=c(3,4,5,6), algorithm="QUBIC"))
#' @export
get_algorithms <- function(bics) {
    return(unique(vapply(bics, function(x) {
        return(x@algorithm)
    }, "")))
}

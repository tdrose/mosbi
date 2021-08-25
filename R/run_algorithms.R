#' @include bicluster.R
NULL


#' Run the fabia biclustering algorithm
#'
#' The function executes the \link[fabia]{fabia} biclustering algorithm,
#' returning a list of biclusters converted into bicluster objects
#' compatible with this package.
#' If the algorithm fails to run, an empty list is returned.
#'
#' @param data_matrix A numeric matrix.
#' @param minRow Same parameters as in \link{filter_bicluster_size}.
#' @param minCol Same parameters as in \link{filter_bicluster_size}.
#' @param thresZ See parameter from the \link[fabia]{extractBic} function.
#' @param thresL See parameter from the \link[fabia]{extractBic} function.
#' @param ... Other parameters forwarded to the \link[fabia]{fabia} function.
#' @return a list of \link{bicluster} objects.
#' 
#' @examples
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(1000), nrow=10)
#' # run_fabia(m, p=5)
#'
#' @export
run_fabia <- function(data_matrix, minRow = 2, minCol = 2, thresZ = 0.5,
                    thresL = NULL, ...) {
    if (!requireNamespace("fabia", quietly = TRUE)) {
        stop("Package \"fabia\" needed for this function to work.
        Please install it.",
            call. = FALSE
        )
    }
    out <- tryCatch(filter_bicluster_size(
        get_biclusters(
            fabia::extractBic(
                fabia::fabia(data_matrix, ...),
                thresZ, thresL
            ),
            data_matrix,
            method = "fabia"
        ),
        minRow, minCol
    ),
    error = function(e) {
        print("Fabia failed to run.")
        return(list())
    }
    )

    return(out)
}

#' Run the isa biclustering algorithm
#'
#' The function executes the \link[isa2]{isa} biclustering algorithm,
#' returning a list of biclusters converted into bicluster objects
#' compatible with this package.
#' If the algorithm fails to run, an empty list is returned.
#'
#' @param data_matrix A numeric matrix.
#' @param minRow Same parameters as in \link{filter_bicluster_size}.
#' @param minCol Same parameters as in \link{filter_bicluster_size}.
#' @param ... Other parameters forwarded to the \link[isa2]{isa} function.
#' @return a list of \link{bicluster} objects.
#' 
#' @examples
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # Not run: run_isa(m)
#'
#' @export
run_isa <- function(data_matrix, minRow = 2, minCol = 2, ...) {
    if (!requireNamespace("isa2", quietly = TRUE)) {
        stop("Package \"isa2\" needed for this function to work.
        Please install it.",
            call. = FALSE
        )
    }
    out <- tryCatch(filter_bicluster_size(
        get_biclusters(isa2::isa(data_matrix, ...),
            data_matrix,
            method = "isa"
        ),
        minRow, minCol
    ),
    error = function(e) {
        print("isa failed to run.")
        return(list())
    }
    )

    return(out)
}

#' Run the QUBIC biclustering algorithm
#'
#' The function executes the \link[QUBIC]{BCQU} biclustering algorithm,
#' returning a list of biclusters converted into bicluster objects
#' compatible with this package.
#' If the algorithm fails to run, an empty list is returned.
#'
#' @param data_matrix A numeric matrix.
#' @param minRow Same parameters as in \link{filter_bicluster_size}.
#' @param minCol Same parameters as in \link{filter_bicluster_size}.
#' @param ... Other parameters forwarded to the \link[QUBIC]{BCQU} function.
#' @return a list of \link{bicluster} objects.
#' 
#' @examples
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # run_qubic(m)
#'
#' @export
run_qubic <- function(data_matrix, minRow = 2, minCol = 2, ...) {
    if (!requireNamespace("QUBIC", quietly = TRUE)) {
        stop("Package \"QUBIC\" needed for this function to work.
        Please install it.",
            call. = FALSE
        )
    }
    if (!requireNamespace("biclust", quietly = TRUE)) {
        stop("Package \"biclust\" needed for this function to work.
        Please install it.",
            call. = FALSE
        )
    }
    out <- tryCatch(filter_bicluster_size(
        get_biclusters(
            biclust::biclust(data_matrix, method = QUBIC::BCQU(), ...),
            data_matrix,
            method = "biclust-qubic"
        ),
        minRow, minCol
    ),
    error = function(e) {
        print("qubic failed to run.")
        return(list())
    }
    )

    return(out)
}

#' Run the Plaid biclustering algorithm
#'
#' The function executes the \link[biclust]{BCPlaid} biclustering algorithm,
#' returning a list of biclusters converted into bicluster objects
#' compatible with this package.
#' If the algorithm fails to run, an empty list is returned.
#'
#' @param data_matrix A numeric matrix.
#' @param minRow Same parameters as in \link{filter_bicluster_size}.
#' @param minCol Same parameters as in \link{filter_bicluster_size}.
#' @param ... Other parameters forwarded to the \link[biclust]{BCPlaid}
#' function.
#' @return a list of \link{bicluster} objects.
#' 
#' @examples
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # run_plaid(m)
#'
#' @export
run_plaid <- function(data_matrix, minRow = 2, minCol = 2, ...) {
    if (!requireNamespace("biclust", quietly = TRUE)) {
        stop("Package \"biclust\" needed for this function to work.
        Please install it.",
            call. = FALSE
        )
    }
    out <- tryCatch(filter_bicluster_size(
        get_biclusters(
            biclust::biclust(data_matrix, method = biclust::BCPlaid(), ...),
            data_matrix,
            method = "biclust-plaid"
        ),
        minRow, minCol
    ),
    error = function(e) {
        print("Plaid failed to run.")
        return(list())
    }
    )

    return(out)
}

#' Run the Bimax biclustering algorithm
#'
#' The function executes the \link[biclust]{BCBimax} biclustering algorithm,
#' returning a list of biclusters converted into bicluster
#' objects compatible with this package.
#' If the algorithm fails to run, an empty list is returned.
#'
#' @param data_matrix A numeric matrix.
#' @param minRow Same parameters as in \link{filter_bicluster_size}.
#' @param minCol Same parameters as in \link{filter_bicluster_size}.
#' @param ... Other parameters forwarded to the \link[biclust]{BCBimax}
#' function.
#' @return a list of \link{bicluster} objects.
#' 
#' @examples
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # run_bimax(m)
#'
#' @export
run_bimax <- function(data_matrix, minRow = 2, minCol = 2, ...) {
    if (!requireNamespace("biclust", quietly = TRUE)) {
        stop("Package \"biclust\" needed for this function to work.
        Please install it.",
            call. = FALSE
        )
    }
    out <- tryCatch(filter_bicluster_size(
        get_biclusters(
            biclust::biclust(data_matrix, method = biclust::BCBimax(), ...),
            data_matrix,
            method = "biclust-bimax"
        ),
        minRow, minCol
    ),
    error = function(e) {
        print("Bimax failed to run.")
        return(list())
    }
    )

    return(out)
}

#' Run the spectral biclustering algorithm
#'
#' The function executes the \link[biclust]{BCSpectral}
#' biclustering algorithm, returning a list of biclusters converted into
#' bicluster objects compatible with this package.
#' If the algorithm fails to run, an empty list is returned.
#'
#' @param data_matrix A numeric matrix.
#' @param minRow Same parameters as in \link{filter_bicluster_size}.
#' @param minCol Same parameters as in \link{filter_bicluster_size}.
#' @param ... Other parameters forwarded to
#' the \link[biclust]{BCSpectral} function.
#' @return a list of \link{bicluster} objects.
#' 
#' @examples
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # run_spectral(m)
#'
#' @export
run_spectral <- function(data_matrix, minRow = 2, minCol = 2, ...) {
    if (!requireNamespace("biclust", quietly = TRUE)) {
        stop("Package \"biclust\" needed for this function to work.
        Please install it.",
            call. = FALSE
        )
    }
    out <- tryCatch(filter_bicluster_size(
        get_biclusters(
            biclust::biclust(data_matrix, method = biclust::BCSpectral(), ...),
            data_matrix,
            method = "biclust-spectral"
        ),
        minRow, minCol
    ),
    error = function(e) {
        print("Spectral failed to run.")
        return(list())
    }
    )

    return(out)
}

#' Run the Quest biclustering algorithm
#'
#' The function executes the \link[biclust]{BCQuest} biclustering algorithm,
#' returning a list of biclusters converted into bicluster objects
#' compatible with this package.
#' If the algorithm fails to run, an empty list is returned.
#'
#' @param data_matrix A numeric matrix.
#' @param minRow Same parameters as in \link{filter_bicluster_size}.
#' @param minCol Same parameters as in \link{filter_bicluster_size}.
#' @param ... Other parameters forwarded to the \link[biclust]{BCQuest}
#' function.
#' @return a list of \link{bicluster} objects.
#' 
#' @examples
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # run_quest(m)
#'
#' @export
run_quest <- function(data_matrix, minRow = 2, minCol = 2, ...) {
    if (!requireNamespace("biclust", quietly = TRUE)) {
        stop("Package \"biclust\" needed for this function to work.
        Please install it.",
            call. = FALSE
        )
    }
    out <- tryCatch(filter_bicluster_size(
        get_biclusters(
            biclust::biclust(data_matrix, method = biclust::BCQuest(), ...),
            data_matrix,
            method = "biclust-quest"
        ),
        minRow, minCol
    ),
    error = function(e) {
        print("Quest failed to run.")
        return(list())
    }
    )

    return(out)
}

#' Run the CC biclustering algorithm
#'
#' The function executes the \link[biclust]{BCCC} biclustering algorithm,
#' returning a list of biclusters converted into bicluster objects
#' compatible with this package.
#' If the algorithm fails to run, an empty list is returned.
#'
#' @param data_matrix A numeric matrix.
#' @param minRow Same parameters as in \link{filter_bicluster_size}.
#' @param minCol Same parameters as in \link{filter_bicluster_size}.
#' @param ... Other parameters forwarded to the \link[biclust]{BCCC} function.
#' @return a list of \link{bicluster} objects.
#' 
#' @examples
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # run_cc(m)
#'
#' @export
run_cc <- function(data_matrix, minRow = 2, minCol = 2, ...) {
    if (!requireNamespace("biclust", quietly = TRUE)) {
        stop("Package \"biclust\" needed for this function to work.
        Please install it.",
            call. = FALSE
        )
    }
    out <- tryCatch(filter_bicluster_size(
        get_biclusters(
            biclust::biclust(data_matrix, method = biclust::BCCC(), ...),
            data_matrix,
            method = "biclust-cc"
        ),
        minRow, minCol
    ),
    error =
        function(e) {
            print("CC failed to run.")
            return(list())
        }
    )

    return(out)
}

#' Run the Xmotifs biclustering algorithm
#'
#' The function executes the \link[biclust]{BCXmotifs} biclustering algorithm,
#' returning a list of biclusters converted into bicluster objects
#' compatible with this package.
#' If the algorithm fails to run, an empty list is returned.
#'
#' @param data_matrix A numeric matrix.
#' @param minRow Same parameters as in \link{filter_bicluster_size}.
#' @param minCol Same parameters as in \link{filter_bicluster_size}.
#' @param ... Other parameters forwarded to the \link[biclust]{BCXmotifs}
#' function.
#' @return a list of \link{bicluster} objects.
#' 
#' @examples
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # run_xmotifs(m)
#'
#' @export
run_xmotifs <- function(data_matrix, minRow = 2, minCol = 2, ...) {
    if (!requireNamespace("biclust", quietly = TRUE)) {
        stop("Package \"biclust\" needed for this function to work.
        Please install it.",
            call. = FALSE
        )
    }
    out <- tryCatch(filter_bicluster_size(
        get_biclusters(
            biclust::biclust(data_matrix, method = biclust::BCXmotifs(), ...),
            data_matrix,
            method = "biclust-xmotifs"
        ),
        minRow, minCol
    ),
    error = function(e) {
        print("Xmotifs failed to run.")
        return(list())
    }
    )

    return(out)
}

#' Run the UniBic biclustering algorithm
#'
#' The function executes the \link[runibic]{BCUnibic} biclustering algorithm,
#' returning a list of biclusters converted into bicluster objects
#' compatible with this package.
#' If the algorithm fails to run, an empty list is returned.
#' 
#' Due to inavailability of runibic on windows, 
#' execute the result of the function in R to run the UniBic in R.
#' 
#'
#' # @param data_matrix A numeric matrix.
#' 
#' # @param minRow Same parameters as in \link{filter_bicluster_size}.
#' 
#' # @param minCol Same parameters as in \link{filter_bicluster_size}.
#' 
#' # @param ... Other parameters forwarded to the \link[runibic]{BCUnibic}
#' function.
#' 
#' # @return a list of \link{bicluster} objects.
#' 
#' @return Function as a string, which can be executed.
#' @examples
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # run_unibic(m, nbic=10)
#'
#' @export
get_runibic_function <- function(){
    cat('Copy and execute the following function in R, to get the run_unibic function:
    
    run_unibic <- function(data_matrix, minRow = 2, minCol = 2, ...) {
        if (!requireNamespace("runibic", quietly = TRUE)) {
            stop("Package \"runibic\" needed for this function to work.
            Please install it.",
                call. = FALSE
            )
        }
        out <- tryCatch(filter_bicluster_size(
            get_biclusters(runibic::BCUnibic(data_matrix, ...),
                data_matrix,
                method = "biclust-unibic"
            ),
            minRow, minCol
        ),
        error = function(e) {
            print("UniBic failed to run.")
            return(list())
        }
        )
    
        return(out)}')
}


#' Run the akmbiclust biclustering algorithm
#'
#' The function executes the \link[akmbiclust]{akmbiclust} biclustering
#' algorithm, returning a list of biclusters converted into bicluster
#' objects compatible with this package.
#' If the algorithm fails to run, an empty list is returned.
#'
#' @param data_matrix A numeric matrix.
#' @param minRow Same parameters as in \link{filter_bicluster_size}.
#' @param minCol Same parameters as in \link{filter_bicluster_size}.
#' @param ... Other parameters forwarded
#' to the \link[akmbiclust]{akmbiclust} function.
#' @return a list of \link{bicluster} objects.
#' 
#' @examples
#' m <- matrix(seq(1:16), nrow=4)
#' # set.seed(10)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # Not run: run_akmbiclust(m, k=10)
#'
#' @export
run_akmbiclust <- function(data_matrix, minRow = 2, minCol = 2, ...) {
    if (!requireNamespace("akmbiclust", quietly = TRUE)) {
        stop("Package \"akmbiclust\" needed for this function to work.
        Please install it.",
            call. = FALSE
        )
    }
    out <- tryCatch(filter_bicluster_size(
        get_biclusters(
            akmbiclust::akmbiclust(data_matrix, ...),
            data_matrix,
            method = "akmbiclust"
        ),
        minRow, minCol
    ),
    error = function(e) {
        print("akmbiclust failed to run.")
        return(list())
    }
    )

    return(out)
}

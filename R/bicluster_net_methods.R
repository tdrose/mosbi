#' @include bicluster.R
NULL

#' Apply a threshold to a bicluster similarity adjacency matrix or a
#' co-occurrence adjacency matrix.
#'
#' All values lower than the threshold will be replaced by a 0.
#'
#' @param bic_net An object of class \code{bicluster_net}
#' or \code{cooccurrence_net}.
#' @return An adjacency matrix with the applied threshold.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # apply_threshold(bn)
#' 
#' @export
methods::setGeneric("apply_threshold", function(bic_net) {
    methods::standardGeneric("apply_threshold")
})

#' Apply a threshold to a bicluster similarity adjacency matrix.
#'
#' All values lower than the threshold will be replaced by a 0.
#'
#' @param bic_net An object of class \code{bicluster_net}.
#' @return An adjacency matrix with the applied threshold.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # apply_threshold(bn)
#' 
#' @export
methods::setMethod(
    "apply_threshold",
    c(bic_net = "bicluster_net"),
    function(bic_net) {
        tmp <- bic_net@adjacency_matrix
        return(replace_values_float(tmp, bic_net@threshold,
            replace_higher = FALSE
        ))
    }
)


#' Get Adjacency matrix
#'
#' Return Adjacency matrix from bicluster network
#'
#' @param bic_net An object of class \code{bicluster_net}.
#' @return Raw unfiltered adjacency matrix.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # get_adjacency(bn)
#' 
#' @export
methods::setGeneric("get_adjacency", function(bic_net) {
    methods::standardGeneric("get_adjacency")
})

#' Get Adjacency matrix
#'
#' Return Adjacency matrix from bicluster network
#'
#' @param bic_net An object of class \code{bicluster_net}.
#' @return Raw unfiltered adjacency matrix.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # get_adjacency(bn)
#' 
#' @export
methods::setMethod(
    "get_adjacency",
    c(bic_net = "bicluster_net"),
    function(bic_net) {
        return(bic_net@adjacency_matrix)
    }
)

#' Get bicluster network algorithms
#'
#' Return algorithms from bicluster network
#'
#' @param bic_net An object of class \code{bicluster_net}.
#' @return Algorithm names as characters.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # get_bic_net_algorithms(bn)
#' 
#' @export
methods::setGeneric("get_bic_net_algorithms", function(bic_net) {
    methods::standardGeneric("get_bic_net_algorithms")
})

#' Get bicluster network algorithms
#'
#' Return algorithms from bicluster network
#'
#' @param bic_net An object of class \code{bicluster_net}.
#' @return Algorithm names as characters.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # get_bic_net_algorithms(bn)
#' 
#' @export
methods::setMethod(
    "get_bic_net_algorithms",
    c(bic_net = "bicluster_net"),
    function(bic_net) {
        return(bic_net@algorithms)
    }
)

#' Apply a threshold to a co-occurrence adjacency matrix.
#'
#' All values lower than the threshold will be replaced by a 0.
#'
#' @param bic_net An object of class \code{cooccurrence_net}.
#' @return An adjacency matrix with the applied threshold.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # fn <- feature_network(bics, m)
#' # apply_threshold(fn)
#' 
#' @export
methods::setMethod(
    "apply_threshold",
    c(bic_net = "cooccurrence_net"),
    function(bic_net) {
        tmp <- bic_net@adjacency_matrix
        return(replace_values_float(tmp, bic_net@threshold,
            replace_higher = FALSE
        ))
    }
)


#' Convert Bicluster network to an igraph graph object
#'
#' The function converts a \code{\link{bicluster_net}} object into an igraph
#' graph object.
#' The \code{threshold} is used as a cutoff for the edges of the network.
#'
#' @param bic_net An object of class \code{bicluster_net}.
#' @return An \code{igraph::\link[igraph]{graph}} object.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # bicluster_net_to_igraph(bn)
#' 
#' @export
methods::setGeneric("bicluster_net_to_igraph", function(bic_net) {
    methods::standardGeneric("bicluster_net_to_igraph")
})

#' Convert Bicluster network to an igraph graph object
#'
#' The function converts a \code{\link{bicluster_net}} object into an igraph
#' graph object.
#' The \code{threshold} is used as a cutoff for the edges of the network.
#'
#' @param bic_net An object of class \code{bicluster_net}.
#' @return An \code{igraph::\link[igraph]{graph}} object.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # bicluster_net_to_igraph(bn)
#' 
#' @export
methods::setMethod(
    "bicluster_net_to_igraph",
    c(bic_net = "bicluster_net"),
    function(bic_net) {
        return(igraph::simplify(igraph::graph.adjacency(
            apply_threshold(bic_net),
            mode = "undirected",
            weighted = TRUE
        )))
    }
)


#' Convert a co-occurence network to an igraph graph object
#'
#' The function converts a \code{\link{cooccurrence_net}} object into an
#' igraph graph object.
#' The \code{threshold} is used as a cutoff for the edges of the network.
#'
#' @param occ_net An object of class \code{cooccurrence_net}.
#' @return An \code{igraph::\link[igraph]{graph}} object.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # fn <- feature_network(bics, m)
#' # cooccurrence_net_to_igraph(fn)
#' 
#' 
#' @export
methods::setGeneric("cooccurrence_net_to_igraph", function(occ_net) {
    methods::standardGeneric("cooccurrence_net_to_igraph")
})

#' Convert a co-occurrence to an igraph graph object
#'
#' The function converts a \code{\link{cooccurrence_net}} object into an
#' igraph graph object.
#' The \code{threshold} is used as a cutoff for the edges of the network.
#'
#' @param occ_net An object of class \code{cooccurrence_net}.
#' @return An \code{igraph::\link[igraph]{graph}} object.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # fn <- feature_network(bics, m)
#' # cooccurrence_net_to_igraph(fn)
#' 
#' @export
methods::setMethod(
    "cooccurrence_net_to_igraph",
    c(occ_net = "cooccurrence_net"),
    function(occ_net) {
        return(igraph::simplify(igraph::graph.adjacency(
            apply_threshold(bic_net = occ_net),
            mode = "undirected",
            weighted = TRUE
        )))
    }
)

# Plot ----

#' Plot a bicluster network
#'
#' Converts the object into a \link[igraph]{graph} and uses its plot function.
#'
#' @param x An object of class \code{\link{bicluster_net}}.
#' @param y Not used.
#' @param ... Plot parameters forwarded
#' to \code{igraph::\link[igraph]{plot.igraph}}
#' @return An \link[igraph]{graph} plot.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # plot(bn)
#' 
#' @export
methods::setMethod(
    "plot",
    c(x = "bicluster_net", y = "missing"),
    function(x, y, ...) {
        igraph::plot.igraph(bicluster_net_to_igraph(x), ...)
    }
)

#' Plot a co-occurrence network
#'
#' Converts the object into a \link[igraph]{graph} and uses its plot function.
#'
#' @param x An object of class \code{\link{cooccurrence_net}}.
#' @param y Not used.
#' @param ... Plot parameters forwarded
#' to \code{igraph::\link[igraph]{plot.igraph}}
#' @return An \link[igraph]{graph} plot.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # fn <- feature_network(bics, m)
#' # plot(fn)
#' 
#' @export
methods::setMethod(
    "plot",
    c(x = "cooccurrence_net", y = "missing"),
    function(x, y, ...) {
        igraph::plot.igraph(
            cooccurrence_net_to_igraph(occ_net = x), ...
        )
    }
)


get_communities <- function(com) {
    num_com <- length(com)
    ms <- igraph::membership(com)
    out <- list()
    for (i in seq(1, num_com)) {
        out[[i]] <- (seq(1, length(ms)))[igraph::membership(com) == i]
    }
    return(out)
}

# Louvain communities ----

#' Get louvain communities from a bicluster network
#'
#' Extracts the louvain communities from a \code{\link{bicluster_net}}
#' or \code{\link{cooccurrence_net}} object using the louvain modularity
#' optimization from the \code{igraph}
#' package (\code{\link[igraph]{cluster_louvain}}).
#'
#' @param bic_net A \code{\link{bicluster_net}}
#' or \code{\link{cooccurrence_net}}object.
#' @param min_size Minimum size of a louvain community to be
#' returned (minimum value is 2).
#' @param bics Optional. Is only use for the class \code{\link{bicluster_net}}.
#'  The respective list of biclusters to identify,
#'  from which algorithms a community originates.
#' @return A list of \code{\link{bicluster_net}}
#' or \code{\link{cooccurrence_net}} objects.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # get_louvain_communities(bn)
#' 
#' @export
methods::setGeneric("get_louvain_communities", function(bic_net,
                                                        min_size = 2,
                                                        bics = NULL) {
    methods::standardGeneric("get_louvain_communities")
})


#' Get louvain communities from a bicluster network
#'
#' Extracts the louvain communities from a \code{\link{bicluster_net}}
#' object using the louvain modularity optimization from the \code{igraph}
#' package (\code{\link[igraph]{cluster_louvain}}).
#'
#' @param bic_net A \code{\link{bicluster_net}} object.
#' @param min_size Minimum size of a louvain community to
#' be returned.
#' @param bics Optional. The respective list of biclusters to identify,
#' from which algorithms a community originates.
#' @return A list of \code{\link{bicluster_net}} objects.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # get_louvain_communities(bn)
#' 
#' @export
methods::setMethod("get_louvain_communities",
    # c(bic_net="bicluster_net", min_size="numeric"),
    signature = "bicluster_net",
    definition = function(bic_net, min_size = 2, bics = NULL) {
        
        net <- bicluster_net_to_igraph(bic_net)
        coms <- mosbi:::get_communities(igraph::cluster_louvain(net))
        out_l <- list()
        for (i in seq(1, length(coms))) {
            if (length(coms[[i]]) >= min_size) {
                if (!is.null(bics)) {
                    tmp_b <- bicluster_net(
                        adjacency_matrix =
                            bic_net@adjacency_matrix[
                                coms[[i]],
                                coms[[i]]
                            ],
                        threshold = bic_net@threshold
                    )
                    algs <- get_algorithms(
                        select_biclusters_from_bicluster_network(
                            tmp_b,
                            bics
                        )
                    )
                    out_l[[length(out_l) + 1]] <- bicluster_net(
                        adjacency_matrix = tmp_b@adjacency_matrix,
                        threshold = tmp_b@threshold, algorithms = algs
                    )
                } else {
                    if (length(coms[[i]]) > 1) {
                        out_l[[length(out_l) + 1]] <- bicluster_net(
                            adjacency_matrix =
                                bic_net@adjacency_matrix[coms[[i]], coms[[i]]],
                            threshold = bic_net@threshold
                        )
                    } else { # For the case of unconnected biclusters
                        uc_tmp <- matrix(bic_net@adjacency_matrix[coms[[i]], coms[[i]]])
                        rownames(uc_tmp) <- c(paste0("bicluster", coms[[i]]))
                        colnames(uc_tmp) <- c(paste0("bicluster", coms[[i]]))
                        
                        out_l[[length(out_l) + 1]] <- bicluster_net(
                            adjacency_matrix = uc_tmp,
                            threshold = bic_net@threshold
                        )
                        
                    }
                }
            }
        }
        return(out_l)
    }
)

#' Get louvain communities from a co-occurrence network
#'
#' Extracts the louvain communities from a \code{\link{cooccurrence_net}}
#' object using the louvain modularity optimization from the \code{igraph}
#' package (\code{\link[igraph]{cluster_louvain}}).
#'
#' @param bic_net A \code{\link{cooccurrence_net}} object.
#' @param min_size Minimum size of a louvain community to be
#' returned (minimum value is 2).
#' @param bics Not used.
#' @return A list of \code{\link{cooccurrence_net}} objects.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # fn <- feature_network(bics, m)
#' # get_louvain_communities(fn)
#' 
#' @export
methods::setMethod("get_louvain_communities",
    # c(bic_net="bicluster_net", min_size="numeric"),
    signature = "cooccurrence_net",
    definition = function(bic_net, min_size = 2, bics = NULL) {
        if (min_size < 2) {
            min_size <- 2
            warning("Minimum value for parameter 'min_size' is 2.
                The input value has been replaced by 2.")
        }
        net <- cooccurrence_net_to_igraph(bic_net)
        coms <- get_communities(igraph::cluster_louvain(net))
        out_l <- list()
        for (i in seq(1, length(coms))) {
            if (length(coms[[i]]) >= min_size) {
                out_l[[length(out_l) + 1]] <- cooccurrence_net(
                    adjacency_matrix =
                        bic_net@adjacency_matrix[coms[[i]], coms[[i]]],
                    threshold = bic_net@threshold
                )
            }
        }
        return(out_l)
    }
)





extract_biclusters_from_rownames <- function(bic_net) {
    return(as.numeric(substring(rownames(bic_net@adjacency_matrix), 10)))
}


#' Create a subset of biclusters based on a bicluster network
#'
#' The function returns an adapted bicluster list based on
#' a \code{\link{bicluster_net}} object.
#' This might be necessary e.g. after \code{\link{get_louvain_communities}}
#' was used a community consists only of a subset of the biclusters.
#' @param bic_net A \code{\link{bicluster_net}}.
#' @param bics A list of \code{\link{bicluster}} objects as returned
#' by \code{\link{get_biclusters}}.
#' @return A subsetted list of \code{\link{bicluster}} objects
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # lc <- get_louvain_communities(bn)
#' # select_biclusters_from_bicluster_network(lc[[1]], bics)
#' 
#' @export
methods::setGeneric(
    "select_biclusters_from_bicluster_network",
    function(bic_net, bics) {
        methods::standardGeneric("select_biclusters_from_bicluster_network")
    }
)

#' Create a subset of biclusters based on a bicluster network
#'
#' The function returns an adapted bicluster list based on
#' a \code{\link{bicluster_net}} object.
#' This might be necessary e.g. after \code{\link{get_louvain_communities}}
#' was used and a community consists only of a subset of the biclusters.
#' @param bic_net A \code{\link{bicluster_net}}.
#' @param bics A list of \code{\link{bicluster}} objects as returned
#' by \code{\link{get_biclusters}}.
#' @return A subsetted list of \code{\link{bicluster}} objects.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # lc <- get_louvain_communities(bn)
#' # select_biclusters_from_bicluster_network(lc[[1]], bics)
#' 
#' @export
methods::setMethod(
    "select_biclusters_from_bicluster_network",
    c(bic_net = "bicluster_net", bics = "list"),
    function(bic_net, bics) {
        bics[extract_biclusters_from_rownames(bic_net)]
    }
)

check_pie <- function(bic_pie) {
    is_all_zero <- function(x) all(x == 0)

    if (any(vapply(bic_pie, is_all_zero, FALSE))) {
        stop("Each bicluster needs at least one member with a
    class affiliation.")
    }
}

bicluster_pie <- function(bic, types, MARGIN = "column", named = TRUE) {
    attrs <- c()
    if (MARGIN == "column") {
        if (named) {
            attrs <- bic@colname
        } else {
            attrs <- bic@column
        }
    } else {
        if (named) {
            attrs <- bic@rowname
        } else {
            attrs <- bic@row
        }
    }
    tn <- names(types)
    types <- as.character(types)
    names(types) <- tn
    mapped <- types[attrs]
    if (any(is.na(mapped))) {
        stop("Problem in class_vector. Every element in a bicluster must have 
            an entry in the class_vector.")
    }
    num_attrs <- sort(unique(types))
    out <- rep(0, length(num_attrs))
    tmp <- table(mapped)

    for (i in seq(1, length(num_attrs))) {
        if (!is.na(tmp[num_attrs[i]])) {
            out[i] <- tmp[num_attrs[i]]
        }
    }

    return(out)
}

biclusters_pie <- function(bics, types, MARGIN = "column", named=TRUE) {
    tmp <- lapply(bics, bicluster_pie, types = types, MARGIN = MARGIN, 
                    named=named)
    check_pie(tmp)
    return(tmp)
}


#' Plot a bicluster network with piecharts as nodes.
#'
#' @param bic_net A \code{\link{bicluster_net}} object.
#' @param bics The corresponding list of biclusters from \code{bic_net}.
#' After calculating communities with \code{\link{get_louvain_communities}}
#' it is necessary to get the subset of biclusters
#' using \code{\link{select_biclusters_from_bicluster_network}}.
#' @param class_vector A (named) vector with class affinities. Every occuring
#' element in the biclustes must have a non NA value in this list.
#' @param colors Colors used for the classes. Must be a vector with colors in
#' the order of sort(unique(class_vector)).
#' @param named Indicates if \code{rowname}/\code{colname} of the bicluster
#' objects should be used instead of the indizes.
#' @param MARGIN Must be "row" or "column". Indicates which dimension of the
#' bicluster should be used for coloring.
#' @param new_layout If \code{FALSE}, the plot accepts a network layout as a
#' parameter, other wise a new layout is computed.
#' @param ... Additional parameters forwarded
#' to \code{\link[igraph]{plot.igraph}}.
#' @return If \code{new_layout}, a new network layout is returned that can
#' be used for other plots.
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # groups <- ifelse(runif(100)< 0.5, "group1", "group2")
#' # cols <- c("group1"="blue", "group2"="grey")
#' # plot_piechart_bicluster_network(bn, bics, groups, cols, named=FALSE)
#' 
#' @export
plot_piechart_bicluster_network <- function(bic_net, bics, class_vector,
                                            colors, named = TRUE,
                                            MARGIN = "column",
                                            new_layout = TRUE, ...) {
    if (new_layout) {
        l <- igraph::layout_nicely(bicluster_net_to_igraph(bic_net))
        plot(bicluster_net_to_igraph(bic_net),
            layout = l, vertex.shape = "pie",
            vertex.pie = biclusters_pie(bics, class_vector, MARGIN = MARGIN, 
                                        named = named),
            vertex.pie.color = list(colors), ...
        )
        invisible(l)
    } else {
        plot(bicluster_net_to_igraph(bic_net),
            vertex.shape = "pie",
            vertex.pie = biclusters_pie(bics, class_vector, MARGIN = MARGIN, 
                                        named = named),
            vertex.pie.color = list(colors), ...
        )
    }
}


#' Plot a bicluster network colored by algorithms.
#'
#' In the plot each bicluster is colored by the algorithm, that generated it.
#'
#' @param bic_net A \code{\link{bicluster_net}} object.
#' @param bics The corresponding list of biclusters from \code{bic_net}.
#' @param new_layout If \code{FALSE}, the plot accepts a network layout as
#' a parameter, other wise a new layout is computed.
#' @param ... Plot parameters forwarded
#' to \code{igraph::\link[igraph]{plot.igraph}}
#' After calculating communities with \code{\link{get_louvain_communities}}
#' it is necessary to get the subset of biclusters
#' using \code{\link{select_biclusters_from_bicluster_network}}.
#' @return If \code{new_layout}, a new network layout is returned
#' that can be used for other plots.
#' @importFrom RColorBrewer brewer.pal
#' 
#' @examples 
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # plot_algo_network(bn, bics)
#' 
#' @export
plot_algo_network <- function(bic_net, bics, new_layout = TRUE, ...) {
    algocolors <- c(
        "fabia" = "red",
        "isa2" = "green",
        "biclust-qubic" = "blue",
        "QUBIC2" = "darkblue",
        "biforce" = "grey",
        "biclustpy" = "white",
        "BicARE" = "magenta",
        "biclust-plaid" = "black",
        "biclust-quest" = "yellow",
        "biclust-spectral" = "pink",
        "biclust-bimax" = "brown",
        "biclust-cc" = "orange",
        "biclust-xmotifs" = "darkorange",
        "biclust-unibic" = "cyan"
    )

    algs <- unique(vapply(bics, function(x) {
        return(x@algorithm)
    }, ""))



    if (new_layout) {
        l <- igraph::layout_nicely(bicluster_net_to_igraph(bic_net))
        plot(bicluster_net_to_igraph(bic_net),
            vertex.color = algocolors[algohistogram(bics)], layout = l, ...
        )
        graphics::legend("topright",
            legend = names(algocolors[algs]),
            fill = algocolors[algs], title = "Algorithms", bg = "white"
        )
        invisible(l)
    } else {
        plot(bicluster_net_to_igraph(bic_net),
            vertex.color = algocolors[algohistogram(bics)], ...
        )
        graphics::legend("topright",
            legend = names(algocolors[algs]),
            fill = algocolors[algs], title = "Algorithms", bg = "white"
        )
    }
}

#' Node sizes for plotting bicluster networks.
#'
#' When plotting bicluster networks, node sizes adapted to bicluster sizes
#' can improve visual inspection.
#' Node sizes are computed using the following formula:
#' \code{(atan( (x - min(x)) / (max(x) - min(x)) + offset ) * base_size)}.
#' With x being defined a vector of bicluster sizes defined
#' by the \code{MARGIN} parameter.
#'
#' @param bics A list of \code{\link{bicluster}} objects.
#' @param base_size Is multiplied with the atan result for the node size
#' @param offset Offset for the atan calculation. Has to be > 0.
#' Smaller values result in higher differences of node sizes.
#' @param MARGIN "column", "row" or "both" are taken into account
#' for the size of a bicluster bicluster
#' @return Vector of node sizes as floats.
#' 
#' @examples
#' m <- matrix(seq(1:16), nrow=4)
#' # m <- matrix(rnorm(10000), nrow=100)
#' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' # bn <- bicluster_network(bics, m)
#' # nz <- node_size(bics)
#' # plot_algo_network(bn, bics, vertex.size=nz)
#' # plot(bn, vertex.size=node_size(bics, offset=.1, base_size=15))
#' 
#' @export
node_size <- function(bics, base_size = 10, offset = .2, MARGIN = "column") {
    if (MARGIN == "column") {
        x <- colhistogram(bics)
    } else if (MARGIN == "row") {
        x <- rowhistogram(bics)
    } else {
        x <- rowhistogram(bics) * colhistogram(bics)
    }
    return(atan((x - min(x)) / (max(x) - min(x)) + offset) * base_size)
}

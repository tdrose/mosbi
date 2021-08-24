#' A S4 class to store biclusters.
#'
#' @slot row A vector of row.
#' @slot column A vector of columns.
#' @slot rowname A vector of names for the rows in \code{row}.
#' @slot colname A vector of names for the columns in \code{column}.
#' @slot algorithm Algorithm that predicted this bicluster.
#' 
#' @examples 
#' bicluster(row=c(1,2), column=c(1,2), 
#'     rowname=c("a", "b"), colname=c("e", "f"))
#'     
#' @importFrom methods new
#' @export bicluster
#' @exportClass bicluster
bicluster <- methods::setClass(
    # Class name
    "bicluster",

    # Defining slot type
    representation(
        row = "numeric",
        column = "numeric",
        rowname = "character",
        colname = "character",
        algorithm = "character"
    ),

    # Initializing slots
    prototype = list(
        row = as.numeric(NULL),
        column = as.numeric(NULL),
        rowname = as.character(NULL),
        colname = as.character(NULL),
        algorithm = as.character(NULL)
    )
)


#' A S4 class to store bicluster networks.
#'
#' Object that is returned e.g. be the
#' function \code{\link{bicluster_network}}.
#'
#'
#' @slot adjacency_matrix Adjacency matrix of bicluster similarities.
#' @slot threshold Estimated threshold for the bicluster similarity adjacency
#' matrix. All values lower than that in the matrix should be discarded.
#' (Note that the indicated threshold is not applied
#' to the \code{adjacency_matrix})
#' @slot algorithms List of algorithms that contributed to this bicluster
#' network.
#' 
#' @examples
#' bicluster_net(adjacency_matrix=matrix(seq(1:16), nrow=4),
#'     threshold=4)
#' @importFrom methods new
#' @export bicluster_net
#' @exportClass bicluster_net
bicluster_net <- methods::setClass(
    # Class name
    "bicluster_net",

    # Defining slot type
    representation(
        adjacency_matrix = "matrix",
        threshold = "numeric",
        algorithms = "character"
    ),

    # Initializing slots
    prototype = list(
        adjacency_matrix = matrix(),
        threshold = 0,
        algorithms = character()
    )
)


#' A S4 class to store co-occurence networks.
#'
#' Object that is returned e.g. be the function \code{\link{feature_network}}.
#'
#'
#' @slot adjacency_matrix Adjacency matrix of row- and column-element
#' co-occurences.
#' @slot threshold Estimated threshold for the co-occurence adjacency matrix.
#' All values lower than that in the matrix should be
#' discarded. (Note that the indicated threshold is not
#' applied to the \code{adjacency_matrix})
#' 
#' @examples
#' cooccurrence_net(adjacency_matrix=matrix(seq(1:16), nrow=4),
#'     threshold=4)
#'     
#' @importFrom methods new
#' @export cooccurrence_net
#' @exportClass cooccurrence_net
cooccurrence_net <- methods::setClass(
    # Class name
    "cooccurrence_net",

    # Defining slot type
    representation(
        adjacency_matrix = "matrix",
        threshold = "numeric"
    ),

    # Initializing slots
    prototype = list(
        adjacency_matrix = matrix(),
        threshold = 0
    )
)

#' @include bicluster.R
NULL

#' Convert communities into ensemble biclusters
#'
#' After calculation of communities with
#' the \code{\link{get_louvain_communities}} function, the result can be
#' converted into a list of \code{\link{bicluster}} objects with this function.
#' Only biclusters are returned which have a minimum dimension of 2x2.
#'
#' @param coms A list of communities (\code{\link{bicluster_net}}s) as
#' outputted by \code{\link{get_louvain_communities}}.
#' @param bics The list biclusters that was used for calculation
#' with \code{\link{bicluster_network}}.
#' @param mat The numeric matrix, that was used for biclustering.
#' @param row_threshold Minimum fraction of biclusters of a community
#' in which a row needs to occur so that it will be part of the
#' outputted ensemble bicluster.
#' @param col_threshold Minimum fraction of biclusters of a community
#' in which a column needs to occur so that it will be part of the
#' outputted ensemble bicluster.
#' @param threshold_sorted Return the rows and columns in sorted by
#' decreasing fraction.
#' @return A list of \code{\link{bicluster}} objects.
#' 
#' @examples
#' b <- list(bicluster(row=c(1,2,3,4), column=c(1,2,3,4)),
#'     bicluster(row=c(3,4,5,6), column=c(3,4,5,6)))
#' # m <- matrix(runif(100), nrow=10)
#' # tm = matrix(c(0,1,1,0), nrow=2)
#' # bn <- list(bicluster_net(adjacency_matrix=tm, threshold=.5))
#' # ensemble_biclusters(bn, b, m)
#'
#' @export
ensemble_biclusters <- function(coms, bics, mat, row_threshold = .1,
                                col_threshold = .1, threshold_sorted = FALSE) {
    out_l <- list()
    m_names <- has_names(mat)
    counter <- 1
    # Loop over communities
    for (i in seq(1, length(coms))) {
        # Select biclusters of this community
        tmp_bics <- select_biclusters_from_bicluster_network(coms[[i]], bics)
        if (m_names) {
            atov <- attr_overlap(tmp_bics, named = TRUE)
            atov_r <- atov[atov$type == "row", ]
            ensemble_rows <- atov_r[atov_r$Fraction > row_threshold, ]
            atov_c <- atov[atov$type == "column", ]
            ensemble_cols <- atov_c[atov_c$Fraction > col_threshold, ]
            if (threshold_sorted) {
                ensemble_cols <- 
                    ensemble_cols[order(-ensemble_cols$Fraction), ]
                ensemble_rows <- 
                    ensemble_rows[order(-ensemble_cols$Fraction), ]
            }
            if ((length(atov_r) > 1) & (length(atov_c) > 1)) {
                out_l[[counter]] <- bicluster(
                    row = ensemble_rows$ID,
                    column = ensemble_cols$ID,
                    rowname = ensemble_rows$names,
                    colname = ensemble_cols$names,
                    algorithm = "metabiclustering")
                counter <- counter + 1
            }
        } else {
            atov <- attr_overlap(tmp_bics, named = FALSE)
            atov_r <- atov[atov$type == "row", ]
            ensemble_rows <- atov_r[atov_r$Fraction > row_threshold, ]
            atov_c <- atov[atov$type == "column", ]
            ensemble_cols <- atov_c[atov_c$Fraction > col_threshold, ]
            if ((length(atov_r) > 1) & (length(atov_c) > 1)) {
                out_l[[counter]] <- bicluster(
                    row = ensemble_rows$ID,
                    column = ensemble_cols$ID,
                    algorithm = "metabiclustering"
                )
                counter <- counter + 1}}}
    return(out_l)}

#' @include bicluster.R
NULL


#' Filter a matrix
#'
#' All values below the threshold will be replaces by 0.
#'
#' @param mat A Numeric matrix.
#' @param threshold All values below will be replaces by 0.
#' @return A filtered numeric matrix.
#' 
#' @examples
#' m <- matrix(rnorm(10000), nrow=100)
#' filter_matrix(m, threshold=1)
#'
#' @export
filter_matrix <- function(mat, threshold = 1) {
    tmp <- mat
    tmp[mat < threshold] <- 0
    return(tmp)
}

plot_error_model <- function(sn_ratio, org_edges, info_curve,
                            n_randomizations, tmp_l, threshold) {
    graphics::par(mfrow = c(1, 2))
    if (sn_ratio) {
        plot(org_edges[, 1], info_curve,
            xlab = "Threshold",
            ylab = "Signal to Noise ratio"
        )
        graphics::abline(v = threshold, col = "red", lty = 2)
    } else {
        plot(org_edges[, 1], info_curve,
            xlab = "Threshold",
            ylab = "Estimated fraction of false positives"
        )
        graphics::abline(v = threshold, col = "red", lty = 2)
    }

    plot(org_edges[, 1], org_edges[, 2] + 1,
        type = "l", col = "red",
        xlab = "Threshold", ylab = "Remaining edges", log = "y",
        main = "Threshold estimation"
    )
    graphics::legend(
        x = "topright",
        legend = c(
            "Org. network",
            "Randomizations", "Est. threshold"
        ),
        col = c("red", "black", "red"), lty = c(1, 1, 2)
    )
    for (j in seq(1, n_randomizations)) {
        graphics::lines(tmp_l[[j]])
    }
    graphics::abline(v = threshold, col = "red", lty = 2)

    graphics::par(mfrow = c(1, 1))
}

random_similarities <- function(n_randomizations, bics, mat, prob_scale, 
                                metric, prl, max_mat, MARGIN, n_steps){
    rand_sim <- 0
    tmp_l <- list()
    tmp_v <- c()
    for (i in seq(1, n_randomizations)) {
        tmp_sample <- sample_biclusters(bics, mat)
        if (MARGIN == "mean") {
            rand_sim_r <- similarity_matrix(tmp_sample,
                                            MARGIN = "row", metric,
                                            prob_scale = prob_scale,
                                            mat_row = nrow(mat), 
                                            mat_col = ncol(mat)
            )
            rand_sim_c <- similarity_matrix(tmp_sample,
                                            MARGIN = "column",
                                            metric, prob_scale = prob_scale,
                                            mat_row = nrow(mat), 
                                            mat_col = ncol(mat)
            )
            rand_sim <- (rand_sim_r + rand_sim_c) / 2
        } else {
            rand_sim <- similarity_matrix(tmp_sample, MARGIN, metric,
                                          prob_scale = prob_scale,
                                          mat_row = nrow(mat),
                                          mat_col = ncol(mat), prl = prl)}
        tmp <- network_edge_strength_float(rand_sim, n_steps, max_mat)
        tmp_l[[i]] <- tmp
        tmp_l[[i]][, 2] <- tmp_l[[i]][, 2] + 1
        tmp_v <- c(tmp_v, tmp[, 2])
    }
    
    return(list("tmp_l"=tmp_l, "tmp_v"=tmp_v))
}

org_similarity <- function(MARGIN, bics, metric, prob_scale, mat, prl){
    org_sim <- 0
    if (MARGIN == "mean") {
        org_sim_r <- similarity_matrix(bics,
                                       MARGIN = "row", metric,
                                       prob_scale = prob_scale,
                                       mat_row = nrow(mat), mat_col = ncol(mat))
        org_sim_c <- similarity_matrix(bics,
                                       MARGIN = "column",
                                       metric, prob_scale = prob_scale,
                                       mat_row = nrow(mat), mat_col = ncol(mat))
        org_sim <- (org_sim_r + org_sim_c) / 2
    } else {
        org_sim <- similarity_matrix(bics, MARGIN, metric,
                                     prob_scale = prob_scale,
                                     mat_row = nrow(mat), mat_col = ncol(mat),
                                     prl = prl)}
    
    return(org_sim)
}


#' Generate a bicluster network
#'
#' The function computes a bicluster network based on a selected
#' similarity metric.
#' A similarity cut-off is calculated using randomized
#' biclusters (the bicluster size distribution is kept).
#'
#' @param bics A list of bicluster objects.
#' @param mat The matrix used for biclustering.
#' @param n_randomizations The number of randomizations for
#' cut-off estimation. (The mean of all randomizations is used).
#' @param MARGIN Margin over which the similarity is computed.
#' Can be "row", "column", "mean" (In this case the mean of
#' row and column similarity is used) or "both" (In this case
#' the similarity between all the datapoints of biclusters is used).
#' @param metric The similarity metric same as
#' in \code{\link{similarity_matrix}}.
#' @param n_steps Number of points where the difference
#' between randomizations and the real data is evaluated.
#' @param plot_edge_dist Show the plots for cut-off estimation
#' with the error model.
#' @param sn_ratio If \code{TRUE}, the signal to noise ratio is
#' computed, otherwise the \code{error_threshold} is used to to
#' estimate the cut-off at which only \code{error_threshold*100}
#' percent of the edges are estimated to be random overlaps.
#' @param error_threshold If \code{sn_ratio==FALSE} this threshold
#' is used to estimate the threshold at which
#' only \code{error_threshold*100} percent of the edges are
#' estimated to be random overlaps.
#' @param prob_scale Scale similarity by the probability of an
#' overlap equal of higher to the observed one. The scaling is
#' done by multiplying the similarity
#' with \code{(1 - (1 / (1 - log(overlap_probability, base=100))))}.
#' The probability is computed using the f
#' unction \code{\link{p_overlap_2d_higher}}
#' for \code{MARGIN =="both"} and  \code{\link{p_overlap_higher}}
#' otherwise. Can be helpful for big imbalances of bicluster sizes.
#' @param return_plot_data Please do not use outside of the package.
#' @param prl Compute the similarity matrix using multiple
#' cores (works only for \code{MARGIN="both"}). The number of core can be
#' defined by executing: \code{RcppParallel::setThreadOptions(numThreads = 4)}
#' before running this function.
#'
#' @return An object of class \code{\link{bicluster_net}}.
#' 
#' @examples 
#' m <- matrix(rnorm(10000), nrow=100)
#' bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' bicluster_network(bics, m)
#'
#' @export
bicluster_network <- function(bics, mat, n_randomizations = 5, MARGIN = "both",
                            metric = 4, n_steps = 100, plot_edge_dist = TRUE,
                            sn_ratio = TRUE,
                            error_threshold = .05, return_plot_data = FALSE,
                            prob_scale = FALSE, prl = FALSE) {
    if (n_randomizations < 1) { stop("n_randomization must be >= 1.")}
    # Original similarity
    org_sim <- org_similarity(MARGIN, bics, metric, prob_scale, mat, prl)
    org_edges <- network_edge_strength_float(org_sim, n_steps)
    max_mat <- max(org_sim)
    steps <- dim(org_edges)[1]
    # Random similarities
    tmp = random_similarities(n_randomizations, bics, mat, prob_scale, 
                              metric, prl, max_mat, MARGIN, n_steps)
    tmp_l = tmp[["tmp_l"]]
    tmp_v = tmp[["tmp_v"]]
    sample_mat <- matrix(tmp_v, nrow = steps)
    # Average Edge count
    mean_random_edges <- apply(sample_mat, 1, mean)
    threshold <- 0
    info_curve <- c()
    if (sn_ratio) {
        info_curve <- (org_edges[, 2] + 1) / (mean_random_edges + 1)
        threshold <- org_edges[which.max(info_curve), 1]
    } else {
        info_curve <- mean_random_edges / org_edges[, 2]
        if (min(info_curve) > error_threshold) {
            error_threshold <- min(info_curve)
            warning("Threshold cannot be reached replaced by minimum value: ",
                round(error_threshold, 4))}
        tmp_t <- min((
            seq(1, length(mean_random_edges)))[info_curve <= error_threshold])
        threshold <- org_edges[tmp_t, 1]
    }
    cat("Esimated cut-off: ", threshold, "\n")
    # Remaining edges Plot
    if (plot_edge_dist) {
        plot_error_model(sn_ratio, org_edges, info_curve, n_randomizations,
            tmp_l, threshold)}
    algs <- get_algorithms(bics)
    if (return_plot_data) {
        return(list(
            bicluster_net(adjacency_matrix = org_sim, threshold = threshold, 
                        algorithms = algs),
            list(sn_ratio, org_edges, info_curve,
                n_randomizations, tmp_l, threshold)))
    } else {
        return(bicluster_net(adjacency_matrix = org_sim,
            threshold = threshold, algorithms = algs))}}




random_f_similarities <- function(bics, mat, n_randomizations, rr, rc, cc, w, 
                                  n_steps, max_mat){
    tmp_l <- list()
    tmp_v <- c()
    # Random similarities
    for (i in seq(1, n_randomizations)) {
        tmp_sample <- sample_biclusters(bics, mat)
        rand_sim <- full_graph(tmp_sample, mat,
                               rr_weight = rr, rc_weight = rc,
                               cc_weight = cc, weighting = w)
        tmp <- network_edge_strength_float(rand_sim, n_steps, max_mat)
        tmp_l[[i]] <- tmp
        tmp_v <- c(tmp_v, tmp[, 2])
    }
    
    return(list("tmp_l"=tmp_l, "tmp_v"=tmp_v))
}

#' Generate a co-occurence network
#'
#' The function computes a co-occurence network, based on the
#' function \code{\link{full_graph}}.
#' A similarity threshold is calculated using randomized
#' biclusters (the bicluster size distribution is kept).
#'
#' @param bics A list of bicluster objects.
#' @param mat The matrix used for biclustering.
#' @param n_randomizations The number of randomizations for cut-off
#' estimation. (The mean of all randomizations is used).
#' @param n_steps Number of points where the difference between
#' randomizations and the real data is evaluated.
#' @param plot_edge_dist Show the plots for threshold estimation.
#' @param sn_ratio If \code{TRUE}, the signal to noise ratio is
#' computed, otherwise the \code{error_threshold} is used to to
#' estimate the threshold at which only \code{error_threshold*100} percent
#' of the edges are estimated to be random overlaps.
#' @param error_threshold If \code{sn_ratio==FALSE} this cut-off is
#' used to estimate the cut-off at which only \code{error_threshold*100}
#' percent of the edges are estimated to be random overlaps.
#' @param return_plot_data Please do not use outside of the package.
#' @param rr See \code{\link{full_graph}}.
#' @param rc See \code{\link{full_graph}}.
#' @param cc See \code{\link{full_graph}}.
#' @param w See parameter weighting of \code{\link{full_graph}}.
#'
#' @return An object of class \code{\link{cooccurrence_net}}.
#' 
#' @examples 
#' m <- matrix(rnorm(10000), nrow=100)
#' bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
#' feature_network(bics, m)
#'
#' @export
feature_network <- function(bics, mat, n_randomizations = 5, n_steps = 100,
                            plot_edge_dist = TRUE, sn_ratio = 1,
                            error_threshold = .05,
                            return_plot_data = FALSE, rr = 1.,
                            rc = 1., cc = 1., w = 0) {
    if (n_randomizations < 1) {stop("n_randomization must be >= 1.")}
    org_sim <- 0
    rand_sim <- 0
    org_sim <- full_graph(bics, mat, rr_weight = rr,
        rc_weight = rc, cc_weight = cc, weighting = w)
    org_edges <- network_edge_strength_float(org_sim, n_steps)
    max_mat <- max(org_sim)
    steps <- dim(org_edges)[1]
    tmp <- random_f_similarities(bics, mat, n_randomizations, rr, rc, cc, w, 
                                n_steps, 
                                max_mat)
    tmp_l = tmp[["tmp_l"]]
    tmp_v = tmp[["tmp_v"]]
    sample_mat <- matrix(tmp_v, nrow = steps)
    mean_random_edges <- apply(sample_mat, 1, mean)
    threshold <- 0
    info_curve <- c()
    if (sn_ratio) {
        info_curve <- org_edges[, 2] / mean_random_edges
        threshold <- org_edges[which.max(info_curve), 1]
    } else {
        info_curve <- mean_random_edges / org_edges[, 2]
        if (min(info_curve) > error_threshold) {
            error_threshold <- min(info_curve)
            warning("Threshold cannot be reached, replaced by minimum value: ",
                round(error_threshold, 4))}
        tmp_t <- min((
            seq(1, length(mean_random_edges)))[info_curve <= error_threshold])
        threshold <- org_edges[tmp_t, 1]}
    cat("Esimated cut-off: ", threshold, "\n")
    if (plot_edge_dist) {
        plot_error_model(sn_ratio, org_edges, info_curve,
            n_randomizations, tmp_l, threshold)}
    if (return_plot_data) {
        return(list(cooccurrence_net(
                adjacency_matrix = org_sim,
                threshold = threshold),
            list(sn_ratio, org_edges, info_curve,
                n_randomizations, tmp_l, threshold)))
    } else {
        return(cooccurrence_net(
            adjacency_matrix = org_sim,
            threshold = threshold))}}

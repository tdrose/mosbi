// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
#include <Rcpp.h>
using namespace Rcpp;
using namespace RcppParallel;

#ifndef __DISTANCE_MATRIX__
#define __DISTANCE_MATRIX__

bool iinv(int &i, IntegerVector vec);
bool iinv(const int &i, const RVector<int> vec);

//' Detect the number of elements in a list of biclusters.
//' 
//' Finds the highest element in a list of bicluster objects.
//'
//' @param bics A list of bicluster objects.
//' @param MARGIN Choose if the distance is computed over \code{"row"} 
//' or \code{"column"}.
//' @return Return highest row or column index from a list of biclusters.
//' 
//' @examples
//' b <- list(bicluster(row=c(1,2,3,4), column=c(1,2,3,4)),
//'         bicluster(row=c(3,4,5,6), column=c(3,4,5,6)))
//' detect_elements(b)
//' 
//' @export
// [[Rcpp::export]]
int detect_elements(List bics, String MARGIN = "row");

void evaluate_metric(const int& metric, NumericMatrix& out, int& counter, 
                     int& elements1, int& elements2, int& r, int& c);
void evaluate_metric(const int& metric, RMatrix<double>& out, int& counter, 
                     int& elements1, int& elements2, int& r, int& c);
NumericMatrix rowcol_similarity(List bics, String MARGIN = "row", 
                                const int metric = 1, bool prob_scale=false, 
                                const int mat_row=0, const int mat_col=0);
NumericMatrix both_similarity(List bics, const int metric=1, 
                              bool prob_scale=false, const int& mat_row=0, 
                              const int& mat_col=0);
NumericMatrix both_similarity_prl(List bics, const int metric=1, 
                                  bool prob_scale=false, 
                                  const int& mat_row=0, const int& mat_col=0);

//' Compute similarities between biclusters
//' 
//' This function computes a similarity matrix between biclusters using
//' different similarity metrics.
//' 
//' @param bics A list of bicluster objects.
//' @param MARGIN Choose if the distance is computed over \code{"row"}
//' , \code{"column"} or \code{"both"}.
//' @param metric Integer indicating which metric is used. 
//' 1: Bray-Curtis similarity (default), 2: Jaccard index, 
//' 3: overlap coefficient, 4: Fowlkes–Mallows index.
//' @param prob_scale Scale similarity by the probability of an 
//' overlap equal of higher to the observed one. The scaling is 
//' done by multiplying the similarity 
//' with \code{(1 - (1 / (1 - log(overlap_probability, base=100))))}. 
//' The probability is comupted using the 
//' function \code{\link{p_overlap_2d_higher}} 
//' for \code{MARGIN =="both"} and  \code{\link{p_overlap_higher}} 
//' otherwise. Can be helpful for big imbalances of bicluster sizes.
//' @param mat_row If \code{prob_scale == TRUE}, the number of rows of the 
//' input matrix for biclustering must be given.
//' @param mat_col If \code{prob_scale == TRUE}, the number of columns of 
//' the input matrix for biclustering must be given.
//' @param prl Compute the similarity matrix using multiple 
//' cores (works only for \code{MARGIN="both"}). The number of 
//' core can be defined by 
//' executing: \code{RcppParallel::setThreadOptions(numThreads = 4)} 
//' before running this function.
//' @return A numeric matrix of the similarities between all given biclusters.
//' 
//' @examples
//' b <- list(bicluster(row=c(1,2,3,4), column=c(1,2,3,4)),
//'         bicluster(row=c(3,4,5,6), column=c(3,4,5,6)))
//' similarity_matrix(b)
//'
//' @export
// [[Rcpp::export]]
NumericMatrix similarity_matrix(List bics, String MARGIN = "both", 
                                const int metric = 1, bool prob_scale=false, 
                                const int mat_row=0, const int mat_col=0, 
                                bool prl=false);


//' Compute distances between biclusters
//' 
//' This function computes a distance matrix between biclusters using 
//' different dissimilarity metrics.
//' 
//' @param bics A list of bicluster objects.
//' @param MARGIN Choose if the distance is computed over \code{"row"} 
//' or \code{"column"}.
//' @param metric Integer indicating which metric is used. 1: Bray-Curtis 
//' dissimilarity (default), 2: Jaccard distance, 3: 1-overlap coefficient  
//' 4: 1 - Fowlkes–Mallows index.
//' @return A numeric matrix of the dissimilarities between all 
//' given biclusters.
//' 
//' @examples
//' b <- list(bicluster(row=c(1,2,3,4), column=c(1,2,3,4)),
//'         bicluster(row=c(3,4,5,6), column=c(3,4,5,6)))
//' distance_matrix(b)
//'
//' @export
// [[Rcpp::export]]
NumericMatrix distance_matrix(List bics, String MARGIN = "row", 
                              const int metric = 1);

//' Probability for an overlap of two samples.
//' 
//' The probability is computed using the 
//' forumla \eqn{\frac{{y \choose x}\times {n-y \choose k-x}}{{n \choose k}}}.
//' 
//' @param x Overlap.
//' @param y Size of sample 1.
//' @param k Size of Sample 2.
//' @param n Number of all elements sampled from.
//' @return Overlap probability.
//' 
//' @examples
//' p_overlap(10, 20, 30, 100)
//'
//' @export
// [[Rcpp::export]]
double p_overlap(const int& x, const int& y, const int& k, const int& n);

//' Probability for an overlap higher or equal to the observed 
//' one of two samples
//' 
//' Is computed by adding up probabilities for all possible 
//' overlaps equal or higher to the observed one using the 
//' function \code{\link{p_overlap}}.
//' 
//' @param x Overlap.
//' @param y Size of sample 1.
//' @param k Size of Sample 2.
//' @param n Number of all elements sampled from.
//' @return Overlap probability.
//' 
//' @examples
//' p_overlap_higher(10, 20, 30, 100)
//' 
//' @export
// [[Rcpp::export]]
double p_overlap_higher(const int& x, const int& y, 
                        const int& k, const int& n);

//' Probability for an overlap of two dimensional samples
//' 
//' Is computed by calculating the overlap probability for each 
//' dimension independently and multiplying them using the 
//' function \code{\link{p_overlap}}.
//' 
//' @param ov_x Overlap in the first dimension.
//' @param ov_y Overlap in the second dimension.
//' @param s1x First sample of the first dimension.
//' @param s1y First sample of the second dimension.
//' @param s2x Second sample of first dimension.
//' @param s2y Second sample of the second dimension.
//' @param mat_x Number of all elements from the first 
//' dimension sampled from.
//' @param mat_y Number of all elements from the second 
//' dimension sampled from.
//' @return Overlap probability.
//' 
//' @examples
//' p_overlap_2d(10, 10, 20, 20, 30, 30, 100, 100)
//' 
//' @export
// [[Rcpp::export]]
double p_overlap_2d(const int& ov_x, const int& ov_y, 
                    const int& s1x, const int& s1y, const int& s2x, 
                    const int& s2y, const int& mat_x, const int& mat_y);

//' Probability for an overlap higher or equal to the observed one 
//' of two dimensional samples
//' 
//' Is computed by adding up probabilities for all combinations of 
//' the observed or higher overlaps using the 
//' function \code{\link{p_overlap_2d}}.
//' 
//' @param ov_x Overlap in the first dimension.
//' @param ov_y Overlap in the second dimension.
//' @param s1x First sample of the first dimension.
//' @param s1y First sample of the second dimension.
//' @param s2x Second sample of first dimension.
//' @param s2y Second sample of the second dimension.
//' @param mat_x Number of all elements from the first 
//' dimension sampled from.
//' @param mat_y Number of all elements from the second 
//' dimension sampled from.
//' @return Overlap probability
//' 
//' @examples
//' p_overlap_2d_higher(10, 10, 20, 20, 30, 30, 100, 100)
//' 
//' @export
// [[Rcpp::export]]
double p_overlap_2d_higher(const int& ov_x, const int& ov_y, 
                           const int& s1x, const int& s1y, 
                           const int& s2x, const int& s2y, 
                           const int& mat_x, const int& mat_y);

double scaling_function(const double& p, const double& base=100);

// Parallel Worker
struct SimWorker : public Worker
{
  const RVector<int> row1, row2;
  
  double value;
  
  SimWorker(const IntegerVector row1, const IntegerVector row2);
  SimWorker(const SimWorker& sum, Split)
    : row1(sum.row1), row2(sum.row2), value(0) {}
  
  void operator()(std::size_t begin, std::size_t end);
  
  void join(const SimWorker& osw);
};

#endif
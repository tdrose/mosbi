// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
#include <Rcpp.h>
#include <algorithm>
#include <functional>

using namespace Rcpp;
using namespace RcppParallel;

//' Replace values in an integer adjacency matrix.
//'
//' Replace values in an integer matrix, that are lower than a certain 
//' threshold.
//'
//' @param mat An integer matrix
//' @param threshold All values in the matrix lower than this values are 
//' replaced by 0.
//' @param replace_higher If set to true, all values >= \code{threshold} 
//' are replaced by 1.
//' @return An integer matrix with (partially) replaced values.
//' 
//' @examples
//' replace_values(matrix(seq(1, 16), nrow=4), threshold=4)
//'
//' @export
// [[Rcpp::export]]
IntegerMatrix replace_values(IntegerMatrix mat, int threshold, 
                             bool replace_higher=true){
  for (int r=0; r<mat.nrow();r++){
    IntegerMatrix::Row ro = mat(r, _);
    for(int c=0;c<mat.ncol();c++){
      if(ro[c]<threshold){
        ro[c]=0;
      } else {
        if(replace_higher){
          ro[c]=1;
        }
      }
    }
  }
  return(mat);
}

//' Replace values in a adjacency matrix.
//' 
//' Same as \code{\link{replace_values}}, but for (positive) non-integer
//' matrices.
//'
//' Replace values in a numeric matrix, that are lower than a certain 
//' threshold.
//'
//' @param mat A numeric matrix
//' @param threshold All values in the matrix lower than this values are 
//' replaced by 0.
//' @param replace_higher If set to true, all values >= \code{threshold} 
//' are replaced by 1.
//' @return A numeric matrix with (partially) replaced values.
//' 
//' @examples
//' replace_values(matrix(rnorm(100), nrow=10), threshold=1)
//'
//' @export
// [[Rcpp::export]]
NumericMatrix replace_values_float(NumericMatrix mat, 
                                   double threshold, 
                                   bool replace_higher=true){
  NumericMatrix ma(mat.nrow(), mat.ncol());
  
  for (int r=0; r<mat.nrow();r++){
    NumericMatrix::Row ro = mat(r, _);
    NumericMatrix::Row roo = ma(r, _);
    for(int c=0;c<mat.ncol();c++){
      if(ro[c]<threshold){
        roo[c]=0;
      } else {
        if(replace_higher){
          roo[c]=1;
        } else{
          roo[c]=ro[c];
        }
      }
    }
  }
  
  CharacterVector tmp_c, tmp_r;
  if((rownames(mat)!=R_NilValue)&(colnames(mat)!=R_NilValue)){
    
    tmp_r = rownames(mat);
    tmp_c = colnames(mat);
    
    colnames(ma) = tmp_r;
    rownames(ma) = tmp_c;
  }
  return(ma);
}

//' Count edges in an adjacency matrix using different cut-off thresholds.
//' 
//' Computes the how many edges remain in a network if edges with a weight 
//' lower than a certain threshold are removed.
//' The number of remaining edges between 1 and max(adjm) are calculated.
//' It is assumend that the matrix is symmetric and therefore the number 
//' of edges divided by two.
//' Uses the function \code{\link{replace_values}}.
//' 
//' @param adjm A symmetrix numeric matrix.
//' @return A numeirc matrix of \code{dim(max(adjm), 2)}. The first column 
//' indicated the applied threshold, the second column the remaining edges.
//' 
//' @examples
//' m <- matrix(rnorm(10000), nrow=100)
//' bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
//' fn <- feature_network(bics, m)
//' network_edge_strength(apply_threshold(fn))
//' 
//' @export
// [[Rcpp::export]]
IntegerMatrix network_edge_strength(IntegerMatrix adjm){
  int maximum = max(adjm);
  IntegerMatrix out(maximum, 2);
  
  for(int i=0;i<maximum;i++){
    out(i, 0) = i+1;
    out(i, 1) = sum(replace_values(clone(adjm), i+1))/2;
  }
  
  return(out);
}

// Parallel Worker
struct ReplaceWorker : public Worker
{
  // source matrix
  const RMatrix<double> input;
  
  // destination matrix
  RMatrix<double> output;
  
  double th;
  
  // initialize with source and destination
  ReplaceWorker(const NumericMatrix input, NumericMatrix output, 
                double threshold) 
    : input(input), output(output), th(threshold) {}
  
  
  // take the square root of the range of elements requested
  void operator()(std::size_t begin, std::size_t end) {
    std::transform(input.begin() + begin, 
                   input.begin() + end, 
                   output.begin() + begin, 
                   [this] (const auto& x) { return x < th ? 0 : 1; });
  }
};

//' Count edges in an adjacency matrix using different cut-off 
//' thresholds.
//'
//' Same as \code{\link{network_edge_strength}}, but for (positive) 
//' non-integer matrices.
//' 
//' Computes the how many edges remain in a network if edges with a 
//' weight lower than a certain threshold are removed.
//' The number of remaining edges between 1 and max(adjm) are calculated.
//' It is assumend that the matrix is symmetric and therefore 
//' the number of edges divided by two.
//' Uses the function \code{\link{replace_values_float}}.
//' 
//' @param adjm A symmetrix numeric matrix.
//' @param steps Number of steps for which the edge count is evaluated.
//' @param maximum Highest value until which the edge weight is evaluated. 
//' If maximum=0, the max value of \code{adjm} is used.
//' @return A numeirc matrix of \code{dim(max(adjm), 2)}. 
//' The first column indicated the applied threshold, the second 
//' column the remaining edges.
//' 
//' @examples
//' m <- matrix(rnorm(10000), nrow=100)
//' bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
//' bn <- bicluster_network(bics, m)
//' network_edge_strength_float(apply_threshold(bn))
//'
//' @export
// [[Rcpp::export]]
NumericMatrix network_edge_strength_float(NumericMatrix adjm, 
                                          int steps=100, double maximum=0){
  
  NumericMatrix out(steps,2);
  if(maximum==0){
    maximum = max(adjm);
  }
  float step = maximum/(steps-1);
  
  for(int i=0;i<steps;i++){
    out(i, 0) = step*i;
    NumericMatrix tmp(adjm.nrow(), adjm.ncol());
    ReplaceWorker rw(adjm, tmp, step*i);
    parallelFor(0, adjm.length(), rw);
    out(i, 1) = sum(tmp)/2;
  }
  return(out);
}

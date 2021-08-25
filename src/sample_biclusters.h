#include <Rcpp.h>
using namespace Rcpp;

#ifndef __SAMPLEBICS__
#define __SAMPLEBICS__

//' Get the rowlengths for a list of bicluster objects.
//' 
//' Can be used for e.g. histograms.
//' @param bic A list  of bicluster objects.
//' @return A vector with the lenghts of the rows in every bicluster object.
//' 
//' @examples
//' m <- matrix(rnorm(10000), nrow=100)
//' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
//' # rowhistogram(bics)
//' 
//' @export
// [[Rcpp::export]]
NumericVector rowhistogram(List bic);

//' Get the columnlengths for a list of bicluster objects.
//' 
//' Can be used for e.g. histograms.
//' @param bic A list  of bicluster objects.
//' @return A vector with the lenghts of the columns in every bicluster object.
//' 
//' @examples
//' m <- matrix(rnorm(10000), nrow=100)
//' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
//' # colhistogram(bics)
//' 
//' @export
// [[Rcpp::export]]
NumericVector colhistogram(List bic);

//' Get list the list of algorithms from a list of bicluster objects.
//' 
//' Can be used for .g. histograms.
//' @param bic A list  of bicluster objects.
//' @return A character vector with the extracted biclustering algorithms 
//' used for each bicluster of the input list.
//' 
//' @examples
//' m <- matrix(rnorm(10000), nrow=100)
//' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
//' # algohistogram(bics)
//' 
//' @export
// [[Rcpp::export]]
CharacterVector algohistogram(List bic);

//' Sample a list of biclusters.
//' 
//' The function generates a list of biclusters given an input list of 
//' biclusters, where each bicluster has the same number or rows and columns, 
//' but with sampled entries from a uniform distribution of all rows and 
//' columns is the matrix.
//' @param bics A list of validated bicluster objects.
//' @param mat The numeric matrix, that was used to generate the biclusters.
//' @return A list of \link{bicluster} objects.
//' 
//' @examples
//' m <- matrix(rnorm(10000), nrow=100)
//' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
//' # sample_biclusters(bics, m)
//' 
//' @export
// [[Rcpp::export]]
List sample_biclusters(List bics, NumericMatrix mat);
  
#endif

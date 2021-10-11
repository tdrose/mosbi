#include <Rcpp.h>
using namespace Rcpp;

//' Check, whether a matrix has row- and colnames.
//' @param m A matrix
//' @return Logical indicating existence of row- and colnames.
//' 
//' @examples
//' has_names(matrix(c(1,2,3,4), nrow=2))
//' 
//' m <- matrix(c(1,2,3,4), nrow=2)
//' rownames(m) <- c("r1", "r2")
//' rownames(m) <- c("c1", "c2")
//' has_names(m)
//' 
//' @export
// [[Rcpp::export]]
bool has_names(NumericMatrix m ){
  return(!((colnames(m)==R_NilValue)|(rownames(m)==R_NilValue)));
}

//' Throw an error, if a matrix has not both row- and colnames.
//' @param m A matrix.
//' @return Throws error, if matrix has no row- and column names.
//' 
//' @examples 
//' m <- matrix(c(1,2,3,4), nrow=2)
//' rownames(m) <- c("r1", "r2")
//' colnames(m) <- c("c1", "c2")
//' check_names(m)
//' 
//' @export
// [[Rcpp::export]]
void check_names (NumericMatrix m){
  if (!has_names(m)){
    stop("Matrix m requires row- and colnames.");
  }
}

#include <Rcpp.h>
using namespace Rcpp;


//' Extract the class-wise degree of an adjacency matrix.
//' 
//' For a adjacency matrix as computed by \code{\link{full_graph}},
//' the function computes how many row-column interactions connect 
//' rows (columns) to columns (rows) of a specific class/category.
//' 
//' @param mat A adjacency matrix with bipartite interactions as 
//' computed by \code{\link{full_graph}} or \code{\link{attribute_graph}} 
//' (with parameter \code{bipartite=TRUE}).
//' @param otherclasses A logical vector indicating two classes 
//' of elements in rows (columns).
//' @param useOther Logical indicating if the attributes, that 
//' are classified appear first in the matrix (\code{True}) or 
//' the attributes that connect classified attributes (\code{False}).
//' @return A DataFrame that holds the total degree of every 
//' attribute (row/column) and the fraction of the degree that 
//' connects only to elements of class \code{True} (from 
//' parameter \code{otherclasses}).
//' 
//' @examples
//' m <- matrix(seq(1:16), nrow=4)
//' # m <- matrix(rnorm(10000), nrow=100)
//' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
//' # fn <- feature_network(bics, m)
//' # attributeConnector(apply_threshold(fn), 
//' #     otherclasses=c(rep(FALSE, 100), rep(TRUE, 100)))
//' 
//' @export
// [[Rcpp::export]]
DataFrame attributeConnector(
    IntegerMatrix mat,
    LogicalVector otherclasses,
    bool useOther=false){
  
  if(mat.nrow()!=mat.ncol()){
    stop("Matrix mat must be quadratic. (Same number of rows and columns)");
  }
  
  
  int row_begin, row_stop, col_begin, col_stop, c1, c2;
  //useOther=F -> Lipide kommen zuerst, dann patienten
  if(!useOther){
    row_begin = 0; // Loop over lipids
    row_stop = mat.nrow() - otherclasses.length();
    col_begin = mat.nrow() - otherclasses.length();
    col_stop = mat.nrow();
  } else {
    row_begin = mat.nrow() - otherclasses.length();
    row_stop = mat.nrow();
    col_begin = 0;
    col_stop = mat.nrow() - otherclasses.length();
  }
  
  String tmp_s("");
  CharacterVector mat_nam;
  bool checker = false;
  if(colnames(mat)!=R_NilValue){
    mat_nam = colnames(mat);
    checker= true;
  }
  
  CharacterVector nam(mat.nrow() - otherclasses.length());
  IntegerVector ids(mat.nrow() - otherclasses.length());
  IntegerVector total_degree(mat.nrow() - otherclasses.length());
  IntegerVector class1_degree(mat.nrow() - otherclasses.length());
  
  c1=0;
  for(int i=row_begin;i<row_stop;i++){ // loop over lipids
    IntegerMatrix::Row tmp = mat(i, _);
    c2=0;
    ids[c1]=c1; // dummy for patients
    if(checker) {
      tmp_s = mat_nam[i];
      nam[c1] = tmp_s;
    }
    for(int j=col_begin;j<col_stop;j++){ // loop over patients
      total_degree[c1] += tmp[j];
      if(otherclasses[c2]){
        class1_degree[c1] += tmp[j];
      }
      ++c2;
    }
    ++c1;
  }
  
  if(checker){
    return(DataFrame::create(Named("ids")=nam, 
                             Named("total_degree")=total_degree,
                             Named("class1_degree")=class1_degree));
  } else{
    return(DataFrame::create(Named("ids")=ids, 
                             Named("total_degree")=total_degree,
                             Named("class1_degree")=class1_degree));
  }
}
  

#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

using Row = std::vector<float>; 
using matrix = std::vector<Row>;
using cube = std::vector<matrix>;

//' Replace elements of an integer matrix.
//' 
//' This function replaces all elements of an integer matrix, which are under a 
//' certain threshold (<) with zero.
//' 
//' @param m A numeric matrix.
//' @param threshold A numeric threshold under which all elements in the 
//' matrix are replaced by zero.
//' @return An integer matrix.
//' 
//' @examples
//' m <- matrix(rnorm(10000), nrow=100)
//' replace_threshold(m, 1)
//' 
//' @export
// [[Rcpp::export]]
IntegerMatrix replace_threshold(IntegerMatrix m, int threshold){
  IntegerMatrix tmp = m;
  int nr = tmp.nrow();
  int nc = tmp.ncol();
  for (int i=0; i<nc; i++){
    IntegerMatrix::Column tmp_c = tmp(_, i);
    for(int j=0; j<nr;j++){
      if(tmp_c[j] < threshold){
        tmp_c[j] = 0;
      }
    }
  }
  return(tmp);
}

//' Generate attribute specific co-occurance networks.
//'
//' The function generates co-occurance networks for all the attributes.
//' E.g. if \code{MARGIN="column"}, for each column, a oc-occurance matrix 
//' of rows is generated, which includes all biclusters, where the 
//' column element is present. 
//' 
//' @param bics A list of \code{\link{bicluster}}s.
//' @param m The matrix used for biclustering.
//' @param MARGIN \code{"row"} or \code{"row"}, Indicating if a list of 
//' row- or column-specific networks is generated
//' @return A list of numeric matrices.
//' If \code{MARGIN="column"} (\code{"row"}), the list has a 
//' length of \code{ncol(m)} (\code{nrow(m)}) 
//' and each matrix the dimensions of \code{c(nrow(m), 
//' nrow(m))} (\code{c(ncol(m), ncol(m))})
//' 
//' @examples
//' m <- matrix(rnorm(10000), nrow=100)
//' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
//' # attribute_graph(bics, m)
//'
//' @export
// [[Rcpp::export]]
List attribute_graph(const List bics, 
                     const NumericMatrix m, 
                     String MARGIN="column"){
  
  List out_l = List::create();
  int n_bics = bics.size();
  IntegerVector other_copy, attr_copy;
  CharacterVector tmp_c, tmp_r;
  
  // attr is MARGIN, other is the other dimension
  int attr_size, other_size;
  String attr, other;
  
  if(MARGIN=="column"){
    attr_size = m.ncol();
    other_size = m.nrow();
    attr = "column";
    other = "row";
  } else {
    attr_size = m.nrow();
    other_size = m.ncol();
    attr = "row";
    other = "column";
  }
  
  for(int i=0;i<attr_size;i++){
    out_l.push_back(NumericMatrix(other_size,other_size));
  }
  
  
  for(int j=0;j<n_bics;j++){
    
    NumericMatrix m2(other_size,other_size);
    S4 current_bicluster = bics[j];
    
    IntegerVector bic_attr = current_bicluster.slot(attr);
    IntegerVector bic_other = current_bicluster.slot(other);
    
    other_copy = bic_other - 1;
    attr_copy = bic_attr - 1;
    
    // Generate adjacency matrix for the other attribute in this bicluster
    for(auto o2: other_copy){
      for (auto o3: other_copy){
        if(o2!=o3){
          m2(o2, o3) += 1.;
        }
      }
    }
    // Add values to all matrices which are part of bic_attr
    for(auto a: attr_copy){
      NumericMatrix tmp_m = out_l[a];
      tmp_m += m2;
    }
  }
  
  // Add row & colnames
  if((rownames(m)!=R_NilValue)&(colnames(m)!=R_NilValue)){
    
    tmp_r = rownames(m);
    tmp_c = colnames(m);
    
    if(attr=="row"){
      out_l.names() = tmp_r;
      for(int i=0;i<attr_size;i++){
        NumericMatrix m3 = out_l[i];
        colnames(m3) = tmp_c;
        rownames(m3) = tmp_c;
      }
    } else{
      out_l.names() = tmp_c;
      for(int i=0;i<attr_size;i++){
        NumericMatrix m3 = out_l[i];
        colnames(m3) = tmp_r;
        rownames(m3) = tmp_r;
      }
    }
  }
  
  return(out_l);
}
 
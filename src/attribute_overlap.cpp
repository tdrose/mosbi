#include <Rcpp.h>
#include <map>
#include <string>
using namespace Rcpp;

//' Count how often row/column elements occur in biclusters.
//' 
//' Given a list of bicluster objects (\code{\link{bicluster}}), 
//' the function counts the occurance of all elements in the biclusters.
//' 
//' @param bics A list of \code{\link{bicluster}} objects.
//' @param named Locigal, indicating, if all bicluster objects have names.
//' @return A Data Frame with the counts oof all elements.
//' 
//' @examples
//' # m <- matrix(rnorm(10000), nrow=100)
//' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
//' # attr_overlap(bics, named=FALSE)
//'
//' @export
// [[Rcpp::export]]
DataFrame attr_overlap(List bics, bool named){
  
  S4 current_bic;
  IntegerVector row, col, res_1;
  NumericVector res_2;
  CharacterVector row_c, col_c, res_c, type;
  std::map<int,int> rows, cols;
  std::map<int, std::string> rownames, colnames;
  
  for(int i=0;i<bics.length();i++){
    
    S4 current_bic = bics[i];
    IntegerVector row = current_bic.slot("row");
    if(named){
      row_c = current_bic.slot("rowname");
    }
    // loop over rows
    for(int r=0;r<row.length();r++){
      if(rows.find(row[r]) == rows.end()){
        rows[row[r]]=1;
        if(named){
          rownames[row[r]]=row_c[r];
        }
      } else {
        rows[row[r]]+=1;
      }
    }
    
    IntegerVector col = current_bic.slot("column");
    if(named){
      col_c = current_bic.slot("colname");
    }
    // loop over columns
    for(int c=0;c<col.length();c++){
      if(cols.find(col[c]) == cols.end()){
        cols[col[c]]=1;
        if(named){
          colnames[col[c]]= col_c[c];
        }
      } else {
        cols[col[c]]+=1;
      }
    }
  }
  
  // Calculate fractions
  
  // Loop over rows
  for (std::map<int,int>::iterator it=rows.begin(); it!=rows.end(); ++it){
    res_1.push_back(it->first);
    res_2.push_back((double)(it->second)/(bics.length()));
    type.push_back("row");
    if(named){
      res_c.push_back(rownames[it->first]);
    }
  }
  
  //Loop over columns
  for (std::map<int,int>::iterator it=cols.begin(); it!=cols.end(); ++it){
    res_1.push_back(it->first);
    res_2.push_back((double)(it->second)/(bics.length()));
    type.push_back("column");
    if(named){
      res_c.push_back(colnames[it->first]);
    }
  }
  
  if(named){
    return(DataFrame::create(Named("type")=type, Named("ID")=res_1, 
                             Named("Fraction")=res_2, Named("names")=res_c, 
                             Named("stringsAsFactors") = false));
  } else {
    return(DataFrame::create(Named("type")=type, Named("ID")=res_1, 
                             Named("Fraction")=res_2, 
                             Named("stringsAsFactors") = false));
  }
}


//' Occurance matrix of data points in a list of biclusters
//' 
//' The function computes a matrix with the same dimensions as the input 
//' matrix and fills the matrix elements with the frequence of occurance 
//' of the data points in the input list of biclusters.
//'  
//' @param bics A list of \code{\link{bicluster}} objects.
//' @param mat The data matrix used for biclustering.
//' @return A numeric matrix with the dimensions of the input matrix. 
//' The values represent the frequency of occurance of this point in 
//' the list of biclusters.
//' 
//' @examples
//' # m <- matrix(rnorm(10000), nrow=100)
//' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
//' # occurance_matrix(bics, m)
//' 
//' @export
// [[Rcpp::export]]
NumericMatrix occurance_matrix(const List bics, const NumericMatrix mat){
  NumericMatrix m1(mat.nrow(), mat.ncol());
  IntegerVector row, column;
  
  for (int i=0; i<bics.length(); i++){
    
    S4 current_bic = bics[i];
    row = current_bic.slot("row");
    column = current_bic.slot("column");
    
    for(auto r: row){
      for(auto c: column){
        m1(r-1, c-1) += 1;
      }
    }
  }
  
  m1 = m1 / bics.length();
  
  rownames(m1) = rownames(mat);
  colnames(m1) = colnames(mat);
  return(m1);
}

//' Occurance table of data points in a list of biclusters
//' 
//' The function uses the \code{\link{occurance_matrix}} function and 
//' returns all values higher than the \code{threshold} as a DataFrame.
//' 
//' @param bics A list of \code{\link{bicluster}} objects.
//' @param mat The data matrix used for biclustering.
//' @param threshold Only data points higher than this threshold are returned.
//' @return A DataFrame with the frequencies of occurance for values higher 
//' than a \code{threshold}.
//' 
//' @examples
//' # m <- matrix(rnorm(10000), nrow=100)
//' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
//' # occurance_table(bics, m, threshold=.1)
//' 
//' @export
// [[Rcpp::export]]
DataFrame occurance_table(const List bics, 
                          const NumericMatrix mat, 
                          double threshold=0.){
  
  
  CharacterVector rn, cn, rN, cN;
  NumericMatrix occurance_m = occurance_matrix(bics, mat);
  IntegerVector row, column;
  NumericVector occurance_v;
  
  if((rownames(occurance_m)!=R_NilValue)&(colnames(occurance_m)!=R_NilValue)){
    
    rN = rownames(occurance_m);
    cN = colnames(occurance_m);
    
    for(int r=0; r<occurance_m.nrow(); r++){
      NumericMatrix::Row curr_row = occurance_m(r, _);
      
      for(int c=0; c<curr_row.size(); c++){
        if (curr_row[c] > threshold){
          row.push_back(r+1); // write rowID
          column.push_back(c+1); // write colID
          occurance_v.push_back(curr_row[c]); // write value
          rn.push_back(rN[r]); // write rowName
          cn.push_back(cN[c]); // write colName
        }
      }
    }
    
    return(DataFrame::create(Named("rowID")=row, Named("rowName")= rn, 
                             Named("colID")=column, Named("colName")= cn, 
                             Named("Fraction")=occurance_v, 
                             Named("stringsAsFactors") = false));
    
  } else {
    
    for(int r=0; r<occurance_m.nrow(); r++){
      NumericMatrix::Row curr_row = occurance_m(r, _);
      
      for(int c=0; c<curr_row.size(); c++){
        if (curr_row[c] > threshold){
          row.push_back(r+1); // write rowID
          column.push_back(c+1); // write colID
          occurance_v.push_back(curr_row[c]); // write value 
        }
      }
    }
    
    return(DataFrame::create(Named("rowID")=row, Named("colID")=column, 
                             Named("Fraction")=occurance_v, 
                             Named("stringsAsFactors") = false));
  }
  
}

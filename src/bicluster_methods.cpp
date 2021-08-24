//#include <Rcpp.h>
#include <algorithm>
#include <Rcpp.h>

using namespace Rcpp;

//' Indicates, whether a bicluster is valid.
//' That means it needs at least one row and one column.
//' 
//' @param bic A bicluster object
//' @param minRow Minimum number of required rows (Min=1).
//' @param minCol Minimum number of required columns (Min=1).
//' @return Logical indicating a valid bicluster object.
//' 
//' @examples
//' validate_bicluster(bicluster(row=c(3,4,5,6), column=c(3,4,5,6)))
//' 
//' @export
// [[Rcpp::export]]
bool validate_bicluster(S4 bic, unsigned int minRow = 1, 
                        unsigned int minCol = 1){
  if (!bic.is("bicluster")){
    warning("Object is not a bicluster object.");
    return(false);
  }
  
  if (minRow<1){
    minRow=1;
  }
  if (minCol<1){
    minCol=1;
  }
  if(bic.slot("row")==R_NilValue){
    return(false);
  }
  if(bic.slot("column")==R_NilValue){
    return(false);
  }
  NumericVector r = bic.slot("row");
  if(r.length()<minRow){
    return(false);
  }
  NumericVector c = bic.slot("column");
  if(c.length()<minCol){
    return(false);
  }
  return(true);
}

//' Transpose a bicluster.
//' Row and column slots will be changed.
//' 
//' @param bic A bicluster object.
//' @return A transposed bicluster object,
//' 
//' @examples
//' transpose_bicluster(bicluster(row=c(3,4,5,6), column=c(3,4,5,6)))
//' 
//' @export
// [[Rcpp::export]]
S4 transpose_bicluster(S4 bic){
  //if (!validate_bicluster(bic)){
  //  stop("Not a valid bicluster.");
  //}
  
  S4 res("bicluster");
  res.slot("algorithm") = bic.slot("algorithm");
  res.slot("row") = bic.slot("column");
  res.slot("column") = bic.slot("row");
  res.slot("rowname") = bic.slot("rowname");
  res.slot("colname") = bic.slot("colname");
  return(res);
}

//' Clean a list of biclusters, by returning only the valid ones,
//' @param bics A list of bicluster objects.
//' @return A lis tof bicluster objects
//' 
//' @examples
//' b <- list(bicluster(row=c(1,2,3,4), column=c(1,2,3,4)),
//'         bicluster(row=c(3,4,5,6), column=c(3,4,5,6)))
//' clean_bicluster_list(b)
//' 
//' @export
// [[Rcpp::export]]
List clean_bicluster_list(List bics){
  List newl = List();
  for (int i = 0;i<bics.length();++i){
    S4 bic = bics[i];
    if(validate_bicluster(bic))
      newl.push_back(bic);
  }
  return(newl);
}

bool iinv(int &i, NumericVector vec){
  return(std::find(vec.begin(), vec.end(), i)!=vec.end());
}

//' Check if a bicluster is a subset (in rows AND columns) 
//' of identical to another bicluster. 
//' 
//' @param bic1 A bicluster.
//' @param bic2 A bicluster.
//' @return 1 if bic1 is a subset of bic2, 2 if bic 1 is 
//' identical to bic2, 0 else.
//' 
//' @examples
//' is_subset_or_identical(bicluster(row=c(1,2,3,4), column=c(1,2,3,4)),
//'     bicluster(row=c(1,2,3,4), column=c(1,2,3,4)))
//'     
//' @export
// [[Rcpp::export]]
int is_subset_or_identical(S4 &bic1, S4 &bic2){
  
  //Counter for identical rows
  int irow = 0;
  int icol = 0;
  
  NumericVector row1 = bic1.slot("row");
  NumericVector row2 = bic2.slot("row");
  NumericVector col1 = bic1.slot("column");
  NumericVector col2 = bic2.slot("column");
  
  if((row1.length()>row2.length())|(col1.length()>col2.length())){
    return (0);
  }
  
  // loop over rows
  for(const int& i : row1){
    if(std::find(row2.begin(), row2.end(), i)!=row2.end()){
      ++irow;
    }
  }
  
  // loop over columns
  for(const int& j : col1){
    if(std::find(col2.begin(), col2.end(), j)!=col2.end()){
      ++icol;
    }
  }
  
  // Check if identical
  if((irow==row1.length())&(irow==row2.length())&
     (icol==col1.length())&(icol==col2.length())){
    return (2);
  }
  
  // Check if subset
  if((irow==row1.length())&(icol==col1.length())){
    return (1);
  }
  
  return (0);
}

//' Remove all biclusters from a list, that are identical
//' or perfect subsets from each other.
//' Additionally all invalid biclusters are 
//' removed (See \code{\link{validate_bicluster}}).
//' 
//' @param bics A list of bicluster objects
//' @return A list of bicluster objects, where 
//' perfects subsets or identical biclusters are deleted.
//' 
//' @examples
//' filter_subsets(list(bicluster(row=c(1,2,3,4), column=c(1,2,3,4)),
//'     bicluster(row=c(1,2,3,4), column=c(1,2,3,4))))
//'     
//' @export
// [[Rcpp::export]]
List filter_subsets(List bics){
  
  NumericVector ereaseV;
  NumericVector identicalV;
  S4 b1;
  S4 b2;
  int tmp = 0;
  
  // validate all biclusters
  List::iterator it = bics.begin();
  while(it != bics.end()){
    if(validate_bicluster(*it)){
      ++it;
    } else{
      it = bics.erase(it);
    }
  }
  
  // Loop over all biclusters
  for(int first = 0;first<bics.length();++first){
    S4 b1 = bics[first];
    for(int second = 0;second<bics.length();++second){
      if(first!=second){
        S4 b2 = bics[second];
        tmp = is_subset_or_identical(b1, b2);
        
        switch(tmp){
         case 0: break;
         case 1: ereaseV.push_back(first); // subset
                 break;
         case 2: if(!iinv(second, identicalV)) identicalV.push_back(first); 
         // identical
                 break;
         default: break;
        }
      }
    }
  }

  // Generate new list with all independent biclusters
  List newl = List();
  for(int b=0;b<bics.length();++b){
    if((!iinv(b, ereaseV))&(!iinv(b, identicalV))){
      S4 b1 = bics[b];
      newl.push_back(b1);
    }
  }
  
  return (newl);
}

//' Make a vector of R indices compatible with c++ by 
//' substracting every element by one.
//' @param v A numeric vector.
//' @return A numeric vector with every element decremented by one.
//' 
//' @examples
//' zero_subsetting(c(1,2,3,4,5))
//' 
//' @export
// [[Rcpp::export]]
NumericVector zero_subsetting(NumericVector v){
  return(v-1);
}


//' Subsetting of R matrices within c++.
//'
//' @param m A numeric matrix
//' @param bic A bicluster object.
//' @return Matrix subset.
//' 
//' @examples
//' cpp_matrix_subsetting(matrix(seq(1:16), nrow=4), 
//'     bicluster(row=c(1,2), column=c(1,2)))
//'     
//' @export
// [[Rcpp::export]]
NumericMatrix cpp_matrix_subsetting(NumericMatrix m, S4 bic){
  
  IntegerVector r = IntegerVector(bic.slot("row")) -1;
  IntegerVector c = IntegerVector(bic.slot("column")) -1;
  int rl = r.length();
  int cl = c.length();
  NumericMatrix out(rl, cl);
  
  for (int i=0; i<cl; i++){
    NumericMatrix::Column org_c = m(_, c[i]-1);
    NumericMatrix::Column new_c = out(_, i);
    for (int j=0; j<rl; j++){
      new_c[j] = org_c[r[j]-1];
    }
  }
  return(out);
}


//' Filter a list of bicluster objects, by erasing all biclusters, 
//' that do not fulfill the minimum number of rows and columns.
//' Utilizes the function \code{\link{validate_bicluster}}.
//' 
//' @param bics List of bicluster objects.
//' @param minRow Minimum number of rows.
//' @param minCol Minimum number of columns.
//' @return A filtered list of bicluster objects.
//' 
//' @examples
//' b <- list(bicluster(row=c(1,2), column=c(1,2,3,4)),
//'         bicluster(row=c(3,4,5,6), column=c(3,4,5,6)))
//' filter_bicluster_size(b, 3, 3)
//' 
//' @export
// [[Rcpp::export]]
List filter_bicluster_size(List bics, unsigned int minRow,
                           unsigned int minCol){
  List tmp = List();
  
  for(auto b: bics){
    if(validate_bicluster(b, minRow=minRow, minCol=minCol))
      tmp.push_back(b);
  }
  return(tmp);
}
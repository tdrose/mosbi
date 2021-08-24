// [[Rcpp::depends(RcppParallel)]]
#include <RcppParallel.h>
#include <Rcpp.h>
#include <algorithm>
#include "Rmath.h"
#include "sample_biclusters.h"
#include "distance_matrix.h"
using namespace Rcpp;
using namespace RcppParallel;

// Search for number in vector
bool iinv(int &i, IntegerVector vec){
  return(std::find(vec.begin(), vec.end(), i)!=vec.end());
}

bool iinv(const int &i, const RVector<int> vec){
  return(std::find(vec.begin(), vec.end(), i)!=vec.end());
}


int detect_elements(List bics, String MARGIN){
  int max_elem = 0, tmp=0;
  for(int i=0;i<bics.length();i++){
    S4 bic = bics[i];
    IntegerVector attr = bic.slot(MARGIN);
    tmp = max(attr);
    if(tmp>max_elem){
      max_elem = tmp;
    }
  }
  return(max_elem);
}


double p_overlap(const int& x, const int& y, const int& k, const int& n){
  return(((double) R::choose(y,x) * R::choose(n-y, k-x)) / R::choose(n,k));
}

double p_overlap_higher(const int& x, const int& y, const int& k, 
                        const int& n){
  double p_cumm = 0;
  int min_x = y <= k ? y : k;
  
  for(int x_ov=x;x_ov<=min_x;x_ov++){
      p_cumm += p_overlap(x_ov, y, k, n);
  }
  return(p_cumm);
}


double p_overlap_2d(const int& ov_x, const int& ov_y, const int& s1x, 
                    const int& s1y, const int& s2x, const int& s2y, 
                    const int& mat_x, const int& mat_y){
  return(p_overlap(ov_x, s1x, s2x, mat_x)*p_overlap(ov_y, s1y, s2y, mat_y));
}


double p_overlap_2d_higher(const int& ov_x, const int& ov_y, 
                           const int& s1x, const int& s1y, const int& s2x, 
                           const int& s2y, const int& mat_x, const int& mat_y){
  
  double p_cumm = 0;
  int min_x = s1x <= s2x ? s1x : s2x;
  int min_y = s1y <= s2y ? s1y : s2y;
  
  for(int r_ov=ov_x;r_ov<=min_x;r_ov++){
    for(int c_ov=ov_y;c_ov<=min_y;c_ov++){
      p_cumm += p_overlap_2d(r_ov, c_ov, s1x, s1y, s2x, s2y, mat_x, mat_y);
    }
  }
  
  return(p_cumm);
}


double scaling_function(const double& p, const double& base){
  return(1./(1. - (log(p)/log(base))));
}

void evaluate_metric(const int& metric, NumericMatrix& out, int& counter, 
                     int& elements1, int& elements2, int& r, int& c){
  
  double tp, fp, fn; // For Fowlkes–Mallows index
  
  switch(metric){
  case 1: // Bray-Curtis
    out(r,c) = ((double)(2*counter)/(elements1+elements2));
    out(c,r) = ((double)(2*counter)/(elements1+elements2));
    break;
  case 2: // Jaccard
    out(r,c) = ((double)(counter)/(elements1+elements2-counter));
    out(c,r) = ((double)(counter)/(elements1+elements2-counter));
    break;
  case 3: // 1 - overlap coefficient
    if(elements1<=elements2){
      out(r,c) = ((double)(counter)/(elements1));
      out(c,r) = ((double)(counter)/(elements1));
    } else{
      out(r,c) = ((double)(counter)/(elements2));
      out(c,r) = ((double)(counter)/(elements2));
    }
    break;
  case 4:
    tp = R::choose(counter, 2);
    fp = R::choose(elements1-counter, 2);
    fn = R::choose(elements2-counter, 2);
    //tn = (elements1-counter)*(elements1-counter);
    if(tp==0){
      out(r,c) = 0;
      out(c,r) = 0;
      break;
    }
    out(r,c) = sqrt((tp/(tp+fp))*(tp/(tp+fn)));
    out(c,r) = sqrt((tp/(tp+fp))*(tp/(tp+fn)));
    break;
    
  default:
    stop("metric must be one of c(1,2,3,4).");
  
  }
}


NumericMatrix rowcol_similarity(List bics, String MARGIN, const int metric, 
                                bool prob_scale, const int mat_row, 
                                const int mat_col){
  int number = bics.length(), counter;
  NumericMatrix out(number, number);
  CharacterVector dimnames(number, "bicluster");
  NumericVector v1, v2;
  
  out.fill_diag(1.);
  
  for(int r=0; r<number;r++){ // Bicluster1
    S4 bic1 = bics[r];
    IntegerVector attr1 = bic1.slot(MARGIN);
    int l1 = attr1.length();
    
    for(int c=r+1;c<number;c++){ // Bicluster2
      S4 bic2 = bics[c];
      IntegerVector attr2 = bic2.slot(MARGIN);
      int l2 = attr2.length();
      
      counter=0; // shared elements between two biclusters
      for(int i=0;i<l1;i++){ // Loop over elements in vector
        if(iinv(attr1[i], attr2)){
          counter+=1;
        }
      }
      evaluate_metric(metric, out, counter, l1, l2, r, c);
      
      // Scale by overlap probability
      if(prob_scale){
        if(MARGIN=="row"){
          out(r,c) *= (1. - scaling_function(p_overlap_higher(counter, l1, l2, 
                                             mat_row)));
          out(c,r) *= (1. - scaling_function(p_overlap_higher(counter, l1, l2, 
                                             mat_row)));
        } else {
          out(r,c) *= (1. - scaling_function(p_overlap_higher(counter, l1, l2, 
                                             mat_col)));
          out(c,r) *= (1. - scaling_function(p_overlap_higher(counter, l1, l2, 
                                             mat_col)));
        }
      }
    }
  }
  // Add row- & colnames
  for(int j=0;j<number;j++){
    dimnames[j] += String(j+1);
  }
  rownames(out) = dimnames;
  colnames(out) = dimnames;
  
  return(out);
}

NumericMatrix both_similarity(List bics, const int metric, bool prob_scale, 
                              const int& mat_row, const int& mat_col){
  int number = bics.length(), counter, c1, c2, l1, l2;
  NumericMatrix out(number, number);
  CharacterVector dimnames(number, "bicluster");
  
  out.fill_diag(1.);
  
  for(int r=0; r<number;r++){ // Bicluster1
    S4 bic1 = bics[r];
    IntegerVector row1 = bic1.slot("row");
    IntegerVector col1 = bic1.slot("column");
  
    for(int c=r+1;c<number;c++){ // Bicluster2
      S4 bic2 = bics[c];
      IntegerVector row2 = bic2.slot("row");
      IntegerVector col2 = bic2.slot("column");
      
      c1=0;
      for(int i=0;i<row1.length();i++){
        if(iinv(row1[i], row2)){
          c1+=1;
        }
      }
      
      c2=0;
      for(int j=0;j<col1.length();j++){
        if(iinv(col1[j], col2)){
          c2+=1;
        } 
      }
      
      counter=c1*c2; // shared elements between two biclusters
      l1 = row1.length()*col1.length(); // Total elements from bic1
      l2 = row2.length()*col2.length(); // Total elements from bic2
      
      evaluate_metric(metric, out, counter, l1, l2, r, c);
      
      // Scale by overlap probability
      if(prob_scale){
        out(r,c) *= (1. - scaling_function(p_overlap_2d_higher(c1, c2, 
                                           row1.length(), col1.length(), 
                                           row2.length(), col2.length(), 
                                           mat_row, mat_col)));
        out(c,r) = out(r,c);
      }
    }
  }
  
  // Add row- & colnames
  for(int j=0;j<number;j++){
    dimnames[j] += String(j+1);
  }
  rownames(out) = dimnames;
  colnames(out) = dimnames;
  
  return(out);
}

// Parallel Worker
SimWorker::SimWorker(const IntegerVector row1, const IntegerVector row2)
    : row1(row1), row2(row2), value(0) {
}

  
void SimWorker::operator()(std::size_t begin, std::size_t end) {
  for(int i=begin;i<end;i++){
    if(iinv(row1[i], row2)){
      value+=1;
    }
  }
}

void SimWorker::join(const SimWorker& osw){
  value += osw.value;
}



NumericMatrix both_similarity_prl(const List bics, const int metric, 
                                  bool prob_scale, const int& mat_row, 
                                  const int& mat_col){
  int number = bics.length(), counter, c1, c2, l1, l2;
  NumericMatrix out(number, number);
  CharacterVector dimnames(number, "bicluster");
  
  out.fill_diag(1.);
  
  for(int r=0; r<number;r++){ // Bicluster1
    S4 bic1 = bics[r];
    IntegerVector row1 = bic1.slot("row");
    IntegerVector col1 = bic1.slot("column");
    
    for(int c=r+1;c<number;c++){ // Bicluster2
      S4 bic2 = bics[c];
      IntegerVector row2 = bic2.slot("row");
      IntegerVector col2 = bic2.slot("column");
      
      
      SimWorker swr(row1, row2);
      parallelReduce(0, row1.length(), swr);
      c1=swr.value;
      
      SimWorker swc(col1, col2);
      parallelReduce(0, col1.length(), swc);
      c2=swc.value;
      
      
      counter=c1*c2; // shared elements between two biclusters
      l1 = row1.length()*col1.length(); // Total elements from bic1
      l2 = row2.length()*col2.length(); // Total elements from bic2
      
      evaluate_metric(metric, out, counter, l1, l2, r, c);
      
      // Scale by overlap probability
      if(prob_scale){
        out(r,c) *= (1. - scaling_function(p_overlap_2d_higher(c1, c2, 
                                           row1.length(), col1.length(), 
                                           row2.length(), col2.length(), 
                                           mat_row, mat_col)));
        out(c,r) = out(r,c);
      }
    }
  }
  
  // Add row- & colnames
  for(int j=0;j<number;j++){
    dimnames[j] += String(j+1);
  }
  rownames(out) = dimnames;
  colnames(out) = dimnames;
  
  return(out);
}



NumericMatrix similarity_matrix(List bics, String MARGIN, const int metric, 
                                bool prob_scale, const int mat_row, 
                                const int mat_col, bool prl){
  
  if((MARGIN=="row")||(MARGIN=="column")){
    return(rowcol_similarity(bics, MARGIN, metric, prob_scale, 
                             mat_row, mat_col));
  } else if(MARGIN=="both"){
    if (prl){
      return(both_similarity_prl(bics, metric, prob_scale, 
                                 mat_row, mat_col));
    } else{
      return(both_similarity(bics, metric, prob_scale, 
                             mat_row, mat_col));
    }
  } else{
    stop("Invalid MARGIN. Must be 'row', 'column' or 'both'");
  }
}

NumericMatrix distance_matrix(List bics, String MARGIN, const int metric){
  return(1. - similarity_matrix(bics, MARGIN, metric));
}
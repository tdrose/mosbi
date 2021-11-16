#include <Rcpp.h>
#include <algorithm>
#include <vector>
#include <random>
#include <chrono>
#include "sample_biclusters.h"

using namespace Rcpp;


NumericVector rowhistogram(List bic){
  unsigned int l = bic.length();
  NumericVector out(l);
  for(unsigned int i=0;i<l;i++){
    S4 bicluster = bic[i];
    NumericVector tmp = bicluster.slot("row");
    out[i] = tmp.length();
  }
  return(out);
}

NumericVector colhistogram(List bic){
  unsigned int l = bic.length();
  NumericVector out(l);
  for(unsigned int i=0;i<l;i++){
    S4 bicluster = bic[i];
    NumericVector tmp = bicluster.slot("column");
    out[i] = tmp.length();
  }
  return(out);
}

CharacterVector algohistogram(List bic){
  CharacterVector out(bic.length());
  for(int i=0; i<bic.length(); i++){
    S4 bicluster = bic[i];
    String tmp = bicluster.slot("algorithm");
    out[i] = tmp;
  }
  
  return(out);
}

List sample_biclusters(List bics, NumericMatrix mat){
  
  List out;
  
  std::vector<int> rows(mat.nrow());
  std::vector<int> cols(mat.ncol());
  
  std::iota(rows.begin(), rows.end(), 1);
  std::iota(cols.begin(), cols.end(), 1);
  
  NumericVector rowh = rowhistogram(bics);
  NumericVector colh = colhistogram(bics);
  
  //std::random_device rd;
  //std::mt19937 mymt{rd()};
  unsigned seed1 = std::chrono::system_clock::now().time_since_epoch().count();
  std::mt19937 mymt{seed1};
  
  for (int i=0;i<bics.length();i++){
    
    std::vector<int> sampled_rows((int)rowh[i]);
    std::vector<int> sampled_cols((int)colh[i]);
    
    
    std::sample(rows.begin(), rows.end(), sampled_rows.begin(), 
                (int) rowh[i], mymt);
    std::sample(cols.begin(), cols.end(), sampled_cols.begin(), 
                (int) colh[i], mymt);
    
    S4 bic("bicluster");
    bic.slot("row")=sampled_rows;
    bic.slot("column")=sampled_cols;
    bic.slot("algorithm") = "sampled";
    
    out.push_back(bic);
  }
  
  return(out);
}
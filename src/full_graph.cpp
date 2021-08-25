#include <Rcpp.h>
using namespace Rcpp;

//' Generate a similarity network for a list of biclusters
//' 
//' The function computes a adjacency 
//' matrix for rows and columns of biclusters.
//' The matrix values show, how often two rows or two columns or a row and a 
//' column occur together in biclusters.
//' In the resulting adjacency matrix, rows are listed first, followed 
//' by columns. 
//' They have the same order as the the rows and columns of the input matrix.
//' 
//' In case the given biclusters have overall more or less columns than rows, 
//' the interactions can be weighted to visualize the result properly.
//' @param bics A list of biclusters.
//' @param m The matrix, that was used to calculated the biclusters.
//' @param rr_weight Weight row-row interactions.
//' @param rc_weight Weight row-col interactions.
//' @param cc_weight Weight col-col interactions.
//' @param weighting Weight interactions by bicluster size. 0 - no weighting, 
//' 1 - multiply by bicluster size, 2 - divide by bicluster size.
//' @return An adjacency matrix.
//' 
//' @examples
//' m <- matrix(seq(1:16), nrow=4)
//' b <- list(bicluster(row=c(1,2,3,4), column=c(1,2,3,4)),
//'         bicluster(row=c(3,4,5,6), column=c(3,4,5,6)),
//'         bicluster(row=c(3,4,5,6), column=c(3,4,5,6)))
//' # full_graph(b, m)
//' 
//' @export
// [[Rcpp::export]]
NumericMatrix full_graph(const List bics, const NumericMatrix m, 
                         double const rr_weight=1, double const rc_weight=1, 
                         double const cc_weight=1, const int weighting=0) {
  
  NumericMatrix m2(m.ncol()+m.nrow(),m.ncol()+m.nrow());
  int bipartite_offset = m.nrow();
  double weigh=1;
  
  Rcout << m2.nrow() << " " << m2.ncol() << "\n";
  
  IntegerVector bic_attr, attr_copy, bic_other, other_copy;
  S4 current_bicluster;
  CharacterVector tmp_c, tmp_r, names;
  
  
  // Calculation
  for(unsigned int i=0;i<bics.length();++i){ // Loop over biclusters
    
    S4 current_bicluster = bics[i];
    
    IntegerVector bic_attr = current_bicluster.slot("row");
    IntegerVector bic_other = current_bicluster.slot("column");
    
    attr_copy = bic_attr - 1; // Zero indexing
    other_copy = bic_other - 1;
    
    if (weighting==1){
      weigh = bic_attr.size() * bic_other.size();
    } else if (weighting==2){
      weigh = 1. / log((bic_attr.size() * bic_other.size()));
    }
    
    if(weighting==3){ // New code for weighting different factors 
      // independently

      for(auto r: attr_copy){
        for(auto c: attr_copy){ // Get row-row interactions
          m2(r,c)+=rr_weight / bic_attr.size();
        }
        for(auto o: other_copy){ // Get row-col interactions
          m2(r, bipartite_offset + o) += (rc_weight * 2.) / 
            (bic_attr.size() + bic_other.size());
          m2(bipartite_offset + o, r) += (rc_weight * 2.) / 
            (bic_attr.size() + bic_other.size());
        }
      }
      for(auto o2: other_copy){ // Get col-col interactions
        for (auto o3: other_copy){
          m2(bipartite_offset + o2, bipartite_offset + o3) += 
            cc_weight /bic_other.size();
        }
      }
      
    } else {
      
      for(auto r: attr_copy){
        for(auto c: attr_copy){ // Get row-row interactions
          m2(r,c)+=rr_weight * weigh;
        }
        for(auto o: other_copy){ // Get row-col interactions
          m2(r, bipartite_offset + o) += rc_weight * weigh;
          m2(bipartite_offset + o, r) += rc_weight * weigh;
        }
      }
      for(auto o2: other_copy){ // Get col-col interactions
        for (auto o3: other_copy){
          m2(bipartite_offset + o2, 
             bipartite_offset + o3) += cc_weight * weigh;
        }
      }
    
    }
  }
  
  m2.fill_diag(0);

  // Add row/colnames to matrix
  if((rownames(m)!=R_NilValue)&(colnames(m)!=R_NilValue)){
    
    CharacterVector names(m.nrow()+m.ncol());
    tmp_r = rownames(m);
    std::copy(tmp_r.begin(), tmp_r.end(), names.begin());
    tmp_c = colnames(m);
    std::copy(tmp_c.begin(), tmp_c.end(), names.begin()+bipartite_offset);
      
    colnames(m2) = names;
    rownames(m2) = names;
  }
  return(m2);
}

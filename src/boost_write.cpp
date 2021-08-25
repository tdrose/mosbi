// [[Rcpp::depends(BH)]]
#include "boost/graph/adjacency_list.hpp"
#include "boost/graph/graphml.hpp"
#include <string>
#include <vector>
#include <fstream>
#include <Rcpp.h>
#include "boost_graph.h"
#include "boost_types.h"

//' Save adjacency matrix as GraphML file
//' 
//' Save and adjacency matrix as returned by \code{\link{full_graph}} or 
//' 1 - \code{\link{distance_matrix}} as a GraphML file.
//' 
//' @param m A symmetric numeric matrix (Adjacency matrix). Rownames 
//' are considered as node names.
//' @param filename Name of the resulting GraphML 
//' file (should end with ".gml").
//' @param cols Node colors.
//' @return 0 if successful.
//' @import BH
//' 
//' @examples
//' # m <- matrix(rnorm(10000), nrow=100)
//' # bics <- c(run_fabia(m), run_isa(m), run_plaid(m))
//' # bn <- bicluster_network(bics, m)
//' # write_graphml(apply_threshold(bn), "testfile.txt")
//' 
//' @export
// [[Rcpp::export]]
int write_graphml(NumericMatrix m, String filename, 
                  CharacterVector cols){
  mygraph out_graph;
  generate_boost_graph(out_graph, m, cols);
  
  boost::dynamic_properties dp;
  
  dp.property("vertexID", get(&myvertex::vertexID, out_graph));
  dp.property("name", get(&myvertex::name, out_graph));
  dp.property("color", get(&myvertex::color, out_graph));
  
  dp.property("weight", get(&myedge::weight, out_graph));
  
  std::ofstream myfile;
  myfile.open(filename);
  
  boost::write_graphml(myfile, out_graph, dp, true);
  
  myfile.close();
  
  return(0);
}

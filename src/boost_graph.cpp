// [[Rcpp::depends(BH)]]
#include "boost/graph/adjacency_list.hpp"
#include "boost/graph/graphml.hpp"
#include <string>
#include <vector>
#include <fstream>
#include <Rcpp.h>
#include "boost_graph.h"
#include "boost_types.h"

using namespace Rcpp;


void generate_boost_graph(mygraph &g, 
                          NumericMatrix m, 
                          CharacterVector cols){
  
  // Matrix must be symmetric
  if(m.nrow()!=m.ncol()){
    stop("matrix m must be symmetric");
  }
  
  // col vector must fit to number of nodes
  bool has_col = false;
  if(cols.size()!=0){
    if (cols.size()==m.nrow()){
      has_col=true;
    } else{
      stop("cols must have the same length as number of rows of the matrix");
    }
  }
  
  // Check if matrix nas rownames
  bool has_names = false;
  CharacterVector name;
  if(rownames(m)!=R_NilValue){
    has_names = true;
    name = rownames(m);
  }
  
  // Add vertices
  std::vector<vertex_t> vertices(m.nrow());
  for(int v=0; v<m.nrow(); v++){
    vertices[v] = boost::add_vertex(g);
    
    // Add features
    g[vertices[v]].vertexID = v+1;
    if(has_names){
      g[vertices[v]].name = name[v];
    }
    if(has_col){
      g[vertices[v]].color = cols[v];
    }
    
  }
  
  // Add edges
  for(int i=0; i<m.nrow(); i++){
    NumericMatrix::Row tmp_row = m(i, _);
    for(int j=i+1; j<m.ncol(); j++){
      if (tmp_row[j]!=0){
        auto tmp_edge = boost::add_edge(vertices[i], vertices[j], g);
        g[tmp_edge.first].weight = tmp_row[j];
      }
    }
  }
}
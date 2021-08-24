#include <Rcpp.h>
using namespace Rcpp;
#include "boost/graph/adjacency_list.hpp"
#include "boost/graph/graphml.hpp"
using namespace Rcpp;

#ifndef __BOOST_TYPES__
#define __BOOST_TYPES__

struct myedge{
  double weight = .0;
};

struct myvertex{
  int vertexID = 0;
  std::string name = "";
  std::string color = "";
};

using mygraph = boost::adjacency_list<boost::vecS, 
                                      boost::vecS, 
                                      boost::undirectedS, 
                                      myvertex,
                                      myedge>;

using vertex_t =  boost::graph_traits<mygraph>::vertex_descriptor;
using edge_t = boost::graph_traits<mygraph>::edge_descriptor;

#endif

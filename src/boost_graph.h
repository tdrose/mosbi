#include <Rcpp.h>
#include "boost/graph/adjacency_list.hpp"
#include "boost/graph/graphml.hpp"
#include "boost_types.h"
using namespace Rcpp;

#ifndef __BOOST_GRAPH__
#define __BOOST_GRAPH__


void generate_boost_graph(mygraph &g, 
                          NumericMatrix m, 
                          CharacterVector cols = CharacterVector());

#endif
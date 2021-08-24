#include <Rcpp.h>
#include "boost/graph/adjacency_list.hpp"
#include "boost/graph/graphml.hpp"
#include "boost_types.h"
using namespace Rcpp;

#ifndef __BOOST_WRITE__
#define __BOOST_WRITE__

int write_graphml(NumericMatrix m, String filename, 
                  CharacterVector cols = CharacterVector());

#endif
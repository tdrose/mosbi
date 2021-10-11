// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
#include <string>
#include <fstream>
#include <regex>
#include <vector>
#include <boost/algorithm/string.hpp>


using namespace Rcpp;

std::regex G_MATCH("(?:f)([0-9]+)", std::regex_constants::ECMAScript);
std::regex C_MATCH("(?:c)([0-9]+)", std::regex_constants::ECMAScript);

std::regex BLOCK_MATCH("^BC[0-9]+\t", std::regex_constants::ECMAScript); 
// Find the beginning of a new bicluster block


IntegerVector extract_gene(std::string &line, int &mode){
  
  std::vector<std::string> search_results;
  IntegerVector attr_out;
  std::size_t found = line.find(" ");
  std::smatch tmp_match;
  int gene_element=0;
  
  
  if (found!=std::string::npos){
    boost::split(search_results, line, [](char c){return c == ' ';});
    for(unsigned int i=3;i<(search_results.size()-1);i++){
      std::regex_search(search_results[i], tmp_match, G_MATCH);
      try{
        gene_element = stoi(tmp_match[1]);
        attr_out.push_back(gene_element);
      } catch(...){
        warning("Faulty gene in bicluster detected. Element will be skipped");
      }
    }
  } else{
    mode=0;
  }
  return(attr_out);
}

IntegerVector extract_cond(std::string &line, int &mode){
  
  std::vector<std::string> search_results;
  IntegerVector attr_out;
  std::size_t found = line.find(" ");
  std::smatch tmp_match;
  int cond_element=0;
  
  
  if (found!=std::string::npos){
    boost::split(search_results, line, [](char c){return c == ' ';});
    for(unsigned int i=3;i<(search_results.size()-1);i++){
      std::regex_search(search_results[i], tmp_match, C_MATCH);
      try{
        cond_element = stoi(tmp_match[1]);
        attr_out.push_back(cond_element);
      } catch(...){
        warning(
          "Faulty Condition in bicluster detected. Element will be skipped");
      }
    }
  } else{
    mode=0;
  }
  return(attr_out);
}

//' Extract QUBIC2 biclusters
//' 
//' Extract biclusters from a QUBIC2 "*.blocks" file. 
//' Row and column names are not added to the bicluster objects.
//' @param filename Path to the QUBIC2 results file.
//' @param transposed Set to TRUE, if the
//' biclustering was performed on a tranposed matrix.
//' @return A list of validated bicluster 
//' objects (See \code{\link{validate_bicluster}}).
//' 
//' @examples
//' a <- "PathToQUBIC2output.txt"
//' # Not run: getQUBIC2biclusters(a)
//' 
//' @export
// [[Rcpp::export]]
List getQUBIC2biclusters(String filename, bool transposed=false){
  
  List out;
  
  std::string tmp_line;
  IntegerVector genes, conds;
  int mode = 0; // 0 - waiting for next block, 1 - read genes, 2 - read Conds;
  std::ifstream infile;
  
  
  infile.open(filename);
  if(!infile.is_open()){
    stop("File could not be opened. Check that path is correct.");
  }
  
  while(getline(infile, tmp_line)){
    
    switch(mode){
    case 2:
      conds = extract_cond(tmp_line, mode);
      if(mode!=0){
        S4 bic("bicluster");
        bic.slot("algorithm") = "QUBIC2";
        if(!transposed){
          bic.slot("row") = genes;
          bic.slot("column") = conds;
        } else{
          bic.slot("row") = conds;
          bic.slot("column") = genes;
        }
        out.push_back(bic);
        mode=0;
      }
      
      break;
    case 1:
      mode=2;
      genes = extract_gene(tmp_line, mode);
      break;
    case 0:
      if(std::regex_search(tmp_line, BLOCK_MATCH)) mode=1;
      break;
    }
    
  }
  infile.close();
  
  return(out);
}

#include <Rcpp.h>
#include <iostream>
#include <fstream>
#include <iomanip>
using namespace std;
using namespace Rcpp;

//' Write an R matrix to a file (In a Bi-Force or QUBIC2 readable format).
//' @param m A Numeric matrix.
//' @param filename Name of the output file.
//' @param qubic2_format Write the matrix in a format QUBIC2 is able to read. 
//' This means adding a row- and column names to the file.
//' @return 0 if file was written successfully.
//' 
//' @examples
//' write_matrix(matrix(c(1,2,3,4), nrow=2), "testfile.txt")
//' 
//' @export
// [[Rcpp::export]]
int write_matrix(NumericMatrix m, String filename, bool qubic2_format=false) {
  ofstream myfile;
  myfile.open (filename);
  NumericVector v = m.attr("dim");
  
  if(qubic2_format){
    myfile << "o";
    for(int rn=1; rn<=v[1]; rn++){
      myfile << "\tc" << rn;
    }
    myfile << "\n";
  }
  
  for(int i=0; i<v[0];i++){
    
    if(qubic2_format) myfile << "f" << i+1 << "\t";
      
    for(int j=0; j<(v[1]-1);j++){
      myfile << std::setprecision(10) << m(i,j) << "\t";
    }
    myfile << std::setprecision(10) << m(i, (v[1]-1)) << "\n";
  }
  return(0);
}
#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
int first(NumericMatrix uscases, int row){
  int n = uscases.ncol();
  NumericVector temp(n);
  temp = uscases(row,_);
  NumericVector date(n);
  date[0] = 1;
  for (int i=0; i<n; ++i){
    date[i] += i;
  }
  int first = 0;
  for (int i =0;i<n;++i){
    if (temp[i] != 0){
      first = date(i);
     // return first;
    }
    if (temp[i] != 0){break;}
  }
  //int len = first.length();
  return first;
}


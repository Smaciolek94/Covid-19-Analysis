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
NumericVector first(NumericMatrix uscases, int row){
  int n = uscases.ncol();
  NumericVector temp(n);
  //for (int i=0; i < n; ++i) {
  //  NumericVector temp(n);
    //temp = uscases[Range(0,n),row];
  //  temp[i]= uscases(i,row);
 // }
 temp = uscases(row,_);
 NumericVector date(n);
 date[0] = 1;
 for (int i=0; i<n; ++i){
   date[i] += i;
 }
 //NumericMatrix tempcases(2,n);
 //tempcases(1,_) = date;
 //tempcases(2,_) = temp;
  return temp;
}


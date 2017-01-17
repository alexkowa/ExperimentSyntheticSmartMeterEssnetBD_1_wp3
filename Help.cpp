#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double sumC(NumericVector x) {
  double out = 0;
  
  for(int i=0;i<x.size();i++){
    out = out+x[i];
  }
  
  return(out);
}


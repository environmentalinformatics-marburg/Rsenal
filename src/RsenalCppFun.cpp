#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector findudC(NumericVector v) {
  
  // initialize variables
  int nSize = v.size() - 1;
  NumericVector vud(nSize);
  NumericVector out(nSize);
  
  // calculate differences
  vud = diff(v);
  
  // diff > 0 -> 1; diff <= 0 -> -1
  for (int i = 0; i < nSize; i++) {
    if (vud[i] > 0) {
      out[i] = 1;
    } else {
      out[i] = -1;
    }
  }
  
  return out;
}

// [[Rcpp::export]]
double ioaC(NumericVector x, NumericVector y) {
  
  // initialize variables
  int nSizeX = x.size() - 1;
  NumericVector udx(nSizeX);
  
  int nSizeY = y.size() - 1;
  NumericVector udy(nSizeY);
  
  // calculate differences
  udx = findudC(x);
  udy = findudC(y);
  
  IntegerVector equalTrend(nSizeX);
  for (int i = 0; i < nSizeX; i++) {
    if (udx[i] == udy[i]) {
      equalTrend[i] = 1;
    } else {
      equalTrend[i] = 0;
    };
  }
  
  double nOut = mean(equalTrend);
  return nOut;
}

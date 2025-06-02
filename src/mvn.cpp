// [[Rcpp::depends(RcppArmadillo)]]
// we only include RcppArmadillo.h which pulls Rcpp.h in for us
#include <RcppArmadillo.h>
#include <Rmath.h>
#include <cmath>

// [[Rcpp::export(name = ".rmvn")]]
arma::mat rmvn(unsigned n, arma::mat mu, arma::mat rho) {
  unsigned p = mu.n_cols; // Number of stochastic variables
  arma::mat res(n, p);
  std::generate( res.begin(), res.end(), ::norm_rand ) ;
  arma::mat R = arma::eye(p, p);
  arma::mat U, V;
  arma::vec s;
  arma::rowvec mu_i(p); // mean vector
  for (unsigned i=0; i<n; i++) {
    if (i % 10000 == 0) Rcpp::checkUserInterrupt();
    if (i<rho.n_rows) { // setup correlation matrix
      if (p == 0) {
        double q1 = std::sqrt((1+rho(i, 0))/2.0);
        double q2 = std::sqrt((1-rho(i, 0))/2.0);
        arma::mat Q = { {q1, q2},
                        {q1, -q2} };
        V = Q;
      } else {
        unsigned pos=0;
        for (unsigned r=0; r<p; r++) {
          for (unsigned c=r+1; c<p; c++) {
            R(r,c) = rho(i,pos);
            R(c,r) = rho(i,pos);
            pos++;
          }
        }
        arma::svd(U, s, V, R);
        V = U * diagmat(arma::sqrt(s)); // sqrt(R)
      }
    }
    if (i<mu.n_rows) {
      mu_i = mu.row(i);
    }
    res.row(i) =  res.row(i)*V.t();
    res.row(i) += mu_i;
  }
  return res;
}

#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector kslambda(NumericVector input) {
  int n = input.size();
  NumericVector output(n);

  for (int i = 0; i < n; i++) {
    double r = input[i];

    bool ok = false;
    double lambda = 0.0;

    while (!ok) {
      double Y = R::rnorm(0, 1);
      Y = Y * Y;
      Y= (2* r) / (Y + 2 * r + std::sqrt(Y * Y + 4 * Y * r));
      double U = R::runif(0, 1);
      lambda = (U < 1 / (1 + Y)) ? r / Y : r * Y;

      U = R::runif(0, 1);
      
      if (lambda > 4 / 3) {
        double Z = 1;
        double X = std::exp(-0.5 * lambda);
        int j = 0;

        while (true) {
          j += 1;
          Z -= std::pow(j + 1, 2) * std::pow(X, std::pow(j + 1, 2) - 1);
          if (Z > U) {
            ok = true;
            break;
          }
          j += 1;
          Z += std::pow(j + 1, 2) * std::pow(X, std::pow(j + 1, 2) - 1);
          if (Z < U) {
            ok = false;
            break;
          }
        }
      } else {
        double H = 0.5 * std::log(2) + 2.5 * std::log(M_PI) - 2.5 * std::log(lambda) -
                   (M_PI * M_PI) / (2 * lambda) + 0.5 * lambda;
        double lU = std::log(U);
        double Z = 1;
        double X = std::exp(-(M_PI * M_PI) / (2 * lambda));
        double K = lambda / (M_PI * M_PI);
        int j = 0;

        while (true) {
          j += 1;
          Z -= K * std::pow(X, (j * j) - 1);
          if ((H + std::log(Z)) > lU) {
            ok = true;
            break;
          }
          j += 1;
          Z += std::pow(j + 1, 2) * std::pow(X, std::pow(j + 1, 2) - 1);
          if ((H + std::log(Z)) < lU) {
            ok = false;
            break;
          }
        }
      }
    }

    output[i] = lambda;
  }

  return output;
}


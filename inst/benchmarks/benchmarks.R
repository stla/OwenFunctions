library(microbenchmark)
library(Owen)

#### Owen T ####
microbenchmark(
  Rcpp = OwenTRcpp::OwenT(h=rgamma(1,100,1), a=runif(1,0,1000)),
  haskell = OwenT(h=rgamma(1,100,1), a=runif(1,0,1000)),
  PowerTOST = PowerTOST::OwensT(h=rgamma(1,100,1), a=runif(1,0,1000)),
  times = 1000
)

#### OwenQ1_R ####
microbenchmark(
  haskell_and_R = OwenQ1_R(nu=1L+rpois(1,100), t=runif(1, 1, 10), delta=runif(1, -100, 100), R=rgamma(1,100,1)),
  PowerTOST = PowerTOST::OwensQOwen(nu=1L+rpois(1,100), t=runif(1, 1, 10), delta=runif(1, -100, 100), b=rgamma(1,100,1)),
  times = 1000
)

# for even values (no OwenT)
microbenchmark(
  R = OwenQ1_R2(nu=100L, t=runif(1, 1, 10), delta=runif(1, -100, 100), R=rgamma(1,100,1)),
  PowerTOST = PowerTOST::OwensQOwen(nu=100L, t=runif(1, 1, 10), delta=runif(1, -100, 100), b=rgamma(1,100,1)),
  times = 500
)

microbenchmark(
  vector = vector(mode="numeric", length=100000L),
  numeric = numeric(100000L),
  times=10000
)

M <- matrix(numeric(100*100), ncol=100)
microbenchmark(
  i = M[40L,40L]+exp(2)^3,
  j = M[40,40]+exp(2)^3,
  times=10000
)

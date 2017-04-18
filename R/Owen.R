.pStudent <- function(q, nu, delta){
  .C("pStudentExport", q=as.double(q), nu=as.integer(nu),
     delta=as.double(delta), result=numeric(1L))$result
}

.OwenQ1 <- function(nu, t, delta, R){
  .C("owenQ1export", nu=as.integer(nu), t=as.double(t),
     delta=as.double(delta), r=as.double(R), result=numeric(1))$result
}

.OwenQ2 <- function(nu, t, delta, r){
  .C("owenQ2export", nu=as.integer(nu), t=as.double(t),
     delta=as.double(delta), r=as.double(r), result=numeric(1))$result
}

.OwenQ <- function(nu, t, delta, a, b){
  .C("owenQexport", nu=as.integer(nu), t=as.double(t),
     delta=as.double(delta), a=as.double(a), b=as.double(b),
     result=numeric(1))$result
}

.OwenT <- function(h, a){
  .C("owenTexport", h=as.double(h), a=as.double(a), result=numeric(1))$result
}

#' @title Student CDF with integer number of degrees of freedom
#' @description Cumulative distribution function of the noncentrel Student
#' distribution with an integer number of degrees of freedom.
#' @param q quantile
#' @param nu integer greater than 1, the number of degrees of freedom
#' @param delta noncentrality parameter
#' @return Numeric value, the CDF evaluated at \code{q}.
#' @export
#' @useDynLib Owen
#' @examples
#' pStudent(2, 3) - pt(2, 3)
#' pStudent(2, 3, delta=1) - pt(2, 3, ncp=1)
pStudent <- function(q, nu, delta=0){
  if(!isPositiveInteger(nu)){
    stop("`nu` must be an integer >=1.")
  }
  .pStudent(q=q, nu=nu, delta=delta)
}

#' @title Owen T-function
#' @description Evaluates the Owen T-function by numerical integration.
#' @param h numeric scalar
#' @param a numeric scalar
#' @return A number between 0 and 1.
#' @export
#' @useDynLib Owen
#' @examples
#' # theoretically 0:
#' a <- runif(1, -1000, 1000)
#' OwenT(0,a) - atan(a)/(2*pi)
#' h <- runif(1, -3, 3)
#' OwenT(h,1) - pnorm(h)*(1-pnorm(h))/2
#' a <- 1000 # a -> Inf
#' OwenT(h,a) - (1-pnorm(abs(h)))/2
OwenT <- function(h, a){
  .OwenT(h, a)
}


#' @title First Owen Q-function
#' @description Evaluates the first Owen Q-function (integral from \eqn{0} to \eqn{R})
#' for an integer value of the degrees of freedom.
#' @param nu integer greater than \eqn{1}, the number degrees of freedom
#' @param t finite number, positive or negative
#' @param delta finite number, positive or negative
#' @param R finite positive number, the upper bound of the integral
#' @useDynLib Owen
#' @return A number between 0 and 1, the value of the integral from \eqn{0} to \eqn{R}.
#' @export
#' @examples
#' OwenQ1(4, 3, 2, 1)
#' # OwenQ(nu, t, delta, Inf) = pStudent(t, nu, delta)
#' OwenQ1(4, 3, 2, 100)
#' pStudent(3, 4, 2)
OwenQ1 <- function(nu, t, delta, R){
  if(R<0){
    stop("R must be positive.")
  }
  if(!isPositiveInteger(nu)){
    stop("`nu` must be an integer >=1.")
  }
  if(is.infinite(t) || is.infinite(delta) || is.infinite(R)){
    stop("Parameters must be finite.")
  }
  .OwenQ1(nu=nu, t=t, delta=delta, R=R)
}

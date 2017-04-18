OwenSequences <- function(n, a=1, b=1, d=1, r=1){
  A <- L <- H <- M <- numeric(n+2L)
  A[1L:2L] <- 1
  A[3L] <- 0.5
  H[3L] <- -dnorm(r) * pnorm (a*r-d)
  sB <- sqrt(b)
  M[3L] <- a*sB*dnorm(d*sB)*(pnorm(d*a*sB)-pnorm((d*a*b-r)/sB))
  A[4L] <- 2/3
  L[4L] <- a * b * r * dnorm(r) * dnorm(a*r-d) / 2
  H[4L] <- r * H[3L]
  M[4L] <- b*(d*a*M[3L] + a*dnorm(d*sB)*(dnorm(d*a*sB)-dnorm((d*a*b-r)/sB)))
  for(k in 5L:(n+2L)){
    A[k] <- 1 / (k-1L) / A[k-1L]
    L[k] <- A[k-1L] * r * L[k-1L]
    H[k] <- A[k-2L] * r * H[k-1L]
    M[k] <- (1-1/(k-3L)) * b * (A[k-4L] * d * a * M[k-1L] + M[k-2L]) - L[k-1L]
  }
  return((H+M)[-c(1L,2L)])
}

#' @title First Owen Q-function
#' @description Evaluates the first Owen Q-function (integral from \eqn{0} to \eqn{R})
#' for an integer value of the degrees of freedom.
#' @param nu integer greater than \eqn{1}, the number of degrees of freedom
#' @param t finite number, positive or negative
#' @param delta finite number, positive or negative
#' @param R finite positive number, the upper bound of the integral
#' @return A number between \eqn{0} and \eqn{1}, the value of the integral from \eqn{0} to \eqn{R}.
#' @export
#' @examples
#' # OwenQ1(nu, t, delta, Inf) = pStudent(t, nu, delta)
#' OwenQ1_R(4, 3, 2, 100)
#' pStudent(3, 4, 2)
OwenQ1_R <- function(nu, t, delta, R){
  if(R<0){
    stop("R must be positive.")
  }
  if(!isPositiveInteger(nu)){
    stop("`nu` must be an integer >=1.")
  }
  if(is.infinite(t) || is.infinite(delta) || is.infinite(R)){
    stop("Parameters must be finite.")
  }
  a <- sqrt(t*t/nu)
  b <- nu/(nu+t*t)
  sB <- sqrt(b)
  if(nu==2L){
    return(pnorm(-delta) + sqrt(2*pi) *
             (-dnorm(R) * pnorm (a*R-delta) +
                a*sB*dnorm(delta*sB)*(pnorm(delta*a*sB)-pnorm((delta*a*b-R)/sB))))
  }
  if(nu%%2L==1){
    C <- pnorm(R) - 2*OwenT(R, (a*R-delta)/R) -
      2*OwenT(delta*sB, (delta*a*b-R)/b/delta) + 2*OwenT(delta*sB, a) - (delta>=0)
    if(nu==1L){
      return(C)
    }else if (nu==3L){
      C + 2*(R*(-dnorm(R) * pnorm (a*R-delta)) +
               b*(delta*a*a*sB*dnorm(delta*sB) *
                    (pnorm(delta*a*sB)-pnorm((delta*a*b-R)/sB)) +
                    a*dnorm(delta*sB)*(dnorm(delta*a*sB)-dnorm((delta*a*b-R)/sB))))
    }else{
      return(C +
               2*sum(OwenSequences(nu-1L, a, b, delta, R)[seq(2L, nu-1L, by=2L)]))
    }
  }else{
    return(pnorm(-delta) + sqrt(2*pi) *
             sum(OwenSequences(nu-1L, a, b, delta, R)[seq(1L, nu-1L, by=2L)]))
  }
}


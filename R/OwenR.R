# OwenSequences <- function(n, a=1, b=1, d=1, r=1){
#   # le dernier A ne sert à rien, ce serait mieux d'utiliser A[k] dans la boucle
#   # le dernier L non plus
#   A <- L <- H <- M <- numeric(n+2L)
#   A[1L:2L] <- 1
#   A[3L] <- 0.5
#   H[3L] <- -dnorm(r) * pnorm (a*r-d)
#   sB <- sqrt(b)
#   M[3L] <- a*sB*dnorm(d*sB)*(pnorm(d*a*sB)-pnorm((d*a*b-r)/sB))
#   A[4L] <- 2/3
#   L[4L] <- a * b * r * dnorm(r) * dnorm(a*r-d) / 2
#   H[4L] <- r * H[3L]
#   M[4L] <- b*(d*a*M[3L] + a*dnorm(d*sB)*(dnorm(d*a*sB)-dnorm((d*a*b-r)/sB)))
#   for(k in 5L:(n+2L)){
#     A[k] <- 1 / (k-1L) / A[k-1L]
#     L[k] <- A[k-1L] * r * L[k-1L]
#     H[k] <- A[k-2L] * r * H[k-1L]
#     M[k] <- (k-4L)/(k-3L) * b * (A[k-4L] * d * a * M[k-1L] + M[k-2L]) - L[k-1L]
#   }
#   return(cbind(H,M)[-c(1L,2L),])
# }
#
# OwenSequences <- function(n, a=1, b=1, d=1, r=1){
#   A <- L <- numeric(n+1L)
#   H <- M <- numeric(n+2L)
#   A[1L:2L] <- 1
#   A[3L] <- 0.5
#   H[3L] <- -dnorm(r) * pnorm (a*r-d)
#   sB <- sqrt(b)
#   M[3L] <- a*sB*dnorm(d*sB)*(pnorm(d*a*sB)-pnorm((d*a*b-r)/sB))
#   #A[4L] <- 2/3
#   L[3L] <- a * b * r * dnorm(r) * dnorm(a*r-d) / 2
#   H[4L] <- r * H[3L]
#   M[4L] <- b*(d*a*M[3L] + a*dnorm(d*sB)*(dnorm(d*a*sB)-dnorm((d*a*b-r)/sB)))
#   for(k in 5L:(n+1)){
#     aa <- A[k-1L] <- 1 / (k-2L) / A[k-2L]
#     L[k-1L] <- aa * r * L[k-2L]
#     H[k] <- A[k-2L] * r * H[k-1L]
#     M[k] <- (k-4L)/(k-3L) * b * (A[k-4L] * d * a * M[k-1L] + M[k-2L]) - L[k-2L]
#   }
#   H[n+2L] <- aa * r * H[n+1L]
#   M[n+2L] <- (n-2L)/(n-1L) * b * (A[n-2L] * d * a * M[n+1L] + M[n]) - L[n]
#   return(cbind(H,M)[-c(1L,2L),])
# } # peut-être mieux de faire deux boucles, une pour A et L, une pour H et M

OwenSequences <- function(n, a=1, b=1, d=1, r=1){
  H <- M <- numeric(n)
  H[1L] <- -dnorm(r) * pnorm (a*r-d)
  sB <- sqrt(b)
  M[1L] <- a*sB*dnorm(d*sB)*(pnorm(d*a*sB)-pnorm((d*a*b-r)/sB))
  # if n>1
  H[2L] <- r * H[1L]
  M[2L] <- b*(d*a*M[1L] + a*dnorm(d*sB)*(dnorm(d*a*sB)-dnorm((d*a*b-r)/sB)))
  if(n>2){
    A <- numeric(n)
    L <- numeric(n-2L)
    A[1L:2L] <- 1
    L[1L] <- a * b * r * dnorm(r) * dnorm(a*r-d) / 2
    for(k in 3L:n){
      A[k] <- 1/(k-1L)/A[k-1L]
    }
    if(n>3){
      for(k in 2L:(n-2L)){
        L[k] <- A[k+2L] * r * L[k-1L]
      }
    }
    for(k in 3L:n){
      H[k] <- A[k] * r * H[k-1L]
      M[k] <- (k-2L)/(k-1L) * b * (A[k-2L] * d * a * M[k-1L] + M[k-2L]) - L[k-2L]
    }
  }
  return(cbind(H,M))
}

# OwenSequences <- function(n, a=1, b=1, d=1, r=1){
#   ALHM <- matrix(numeric(4L*(n+2L)), ncol=4L)
#   ALHM[1L,1L] <- 1
#   ALHM[2L,1L] <- 1
#   sB <- sqrt(b)
#   ALHM[3L,] <- c(0.5, 0, -dnorm(r) * pnorm (a*r-d), a*sB*dnorm(d*sB)*(pnorm(d*a*sB)-pnorm((d*a*b-r)/sB)))
#   ALHM[4L,] <- c(2/3, a * b * r * dnorm(r) * dnorm(a*r-d) / 2, r * ALHM[3L,3L], b*(d*a*ALHM[3L,4L] + a*dnorm(d*sB)*(dnorm(d*a*sB)-dnorm((d*a*b-r)/sB))))
#   for(k in 5L:(n+2L)){
#     previous <- ALHM[k-1L,]
#     ALHM[k,] <- c(1 / (k-1L) / previous[1L], previous[1L]*r*previous[2L],
#                   ALHM[k-2L,1L]*r*previous[3L],
#                   (1-1/(k-3L)) * b * (ALHM[k-4L,1L] * d * a * previous[4L] + ALHM[k-2L,4L]) - previous[2L])
#   }
#   return(rowSums(ALHM[-c(1L,2L), c(3L,4L)])) # plus de sommes que nécessaire
# }

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
  if(isNotPositiveInteger(nu)){
    stop("`nu` must be an integer >=1.")
  }
  if(is.infinite(t) || is.infinite(delta) || is.infinite(R)){
    stop("Parameters must be finite.")
  }
  a <- sqrt(t*t/nu)
  b <- nu/(nu+t*t)
  sB <- sqrt(b)
  nu <- as.integer(nu)
  if(nu==2L){
    return(pnorm(-delta) + sqrt(2*pi) *
             (-dnorm(R) * pnorm (a*R-delta) +
                a*sB*dnorm(delta*sB)*(pnorm(delta*a*sB)-pnorm((delta*a*b-R)/sB))))
  }
  if(nu%%2L==1L){
    C <- pnorm(R) - 2*OwenT(R, (a*R-delta)/R) -
      2*OwenT(delta*sB, (delta*a*b-R)/b/delta) + 2*OwenT(delta*sB, a) - (delta>=0)
    if(nu==1L){
      return(C)
    # }else if (nu==3L){
    #   C + 2*(R*(-dnorm(R) * pnorm (a*R-delta)) +
    #            b*(delta*a*a*sB*dnorm(delta*sB) *
    #                 (pnorm(delta*a*sB)-pnorm((delta*a*b-R)/sB)) +
    #                 a*dnorm(delta*sB)*(dnorm(delta*a*sB)-dnorm((delta*a*b-R)/sB))))
    }else{
      return(C +
               2*sum(OwenSequences(nu-1L, a, b, delta, R)[seq(2L, nu-1L, by=2L),]))
    }
  }else{
    return(pnorm(-delta) + sqrt(2*pi) *
             sum(OwenSequences(nu-1L, a, b, delta, R)[seq(1L, nu-1L, by=2L),]))
  }
}


.pStudent <- function(q, nu, delta){
  .C("pStudentExport", q=as.double(q), nu=as.integer(nu),
     delta=as.double(delta), result=numeric(1L))$result
}

.OwenQ1 <- function(nu, t, delta, r){
  .C("owenQ1export", nu=as.integer(nu), t=as.double(t),
     delta=as.double(delta), r=as.double(r), result=numeric(1))$result
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
#' Student CDF with integer number of degrees of freedom
#'
#' @param q quantile
#' @param nu integer greater than 1, the number of degrees of freedom
#' @param delta noncentrality parameter
#'
#' @return Numeric value, the CDF evaluated at \code{q}.
#' @export
#' @useDynLib Owen
#'
#' @examples
#' pStudent(2, 3)
#' pt(2,3)
pStudent <- function(q, nu, delta=0){
  if(!isPositiveInteger(nu)){
    stop("`nu` must be an integer >1")
  }
  .pStudent(q=q, nu=nu, delta=delta)
}

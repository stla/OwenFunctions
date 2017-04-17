isPositiveInteger <- function(x){
  is.numeric(x) && x>0 && (floor(x) == x)
}

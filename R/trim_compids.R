#' @export
trim_compids <- function(x){
  if (length(x) < 3) return(x)
  if (all(x[1:3] == x[4:6])){
    x <- x[-(1:3)]
  }
  x
}
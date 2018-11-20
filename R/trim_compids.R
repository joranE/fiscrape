trim_compids <- function(x){
  if (all(x[1:3] == x[4:6])){
    x <- x[-(1:3)]
  }
  x
}
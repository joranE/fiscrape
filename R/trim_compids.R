#' @export
trim_compids <- function(x){
  n <- min(length(x),10)
  x1 <- head(x,n)
  x2 <- tail(x,-n)
  x1 <- x1[!duplicated(x1)]
  x <- c(x1,x2)
  x
}
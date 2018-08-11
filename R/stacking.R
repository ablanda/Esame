#' Stacking
#'
#' @param X 
#' @param y 
#'
#' @return
#' @export
#'
#' @examples
stacking <- function(X,y){

  X<-X-y
  w<-rep(0,ncol(X))
  for(i in 1:ncol(X)){
    w[i]<-sum((colSums(X[,i]*X[,-i])/nrow(X))^-1)
  }
  w<-w/sum(w)
  return(w)
}

#'  cv regressione componenti principali
#'
#' fonisce le previsioni della cv, da utilizzare dentro sapply(1:k,...)
#'
#' @param i i-esimo fold
#' @param folds prodotto da kfold function
#' @param y  vettore. variabile risposta
#' @param x matrice variabili esplicative
#' @param family qualsiasi
#' @param cor logical. if correlation or covariance
#'
#' @export

cv.pc<-function(j,y,x,folds,family=binomial,cor=T){
  p<-sapply(1:length(folds),function(h) pc_crossvalidation(h,y=y,x=x,folds=folds,cor=cor,family=family,npc=j),simplify=F)
  length(p)
  p<-unlist(p)
  length(p)
  p<-p[order(unlist(folds))]
  n <- table(p>0.5, y)
  err.tot <- 1 - sum(diag(n))/sum(n)
  return(err.tot)
  
}

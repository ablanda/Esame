#' cross validation leave one out knn
#'
#' @param x 
#' @param y 
#' @param folds  
#' @param K 
#'
#' @return errore totale per un determinato k
#' @export

cv.knn<-function(K,x, y,folds=NULL)
{
  if(is.null(folds))
  {
  p<-FNN::knn.cv(as.matrix(x),cl=y,k=K)
  n <- table(p, y)
  }
  else {
    p<-sapply(1:length(folds),function(j) knn_crossvalidation(j,y=y,x=x,folds=folds,K=K),simplify=F)
    length(p)
    p<-unlist(p)
    length(p)
    p<-p[order(unlist(folds))]
    n <- table(p>0.5, y)
  }
  err.tot <- 1 - sum(diag(n))/sum(n)
  return(err.tot)
}


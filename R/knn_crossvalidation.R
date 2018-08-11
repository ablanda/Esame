#' knn cv
#'
#' fonisce le previsioni della cv, da utilizzare dentro sapply(1:k,...)
#'
#' @param i i-esimo fold
#' @param folds prodotto da kfold function
#' @param y  vettore. variabile risposta
#' @param x matrice variabili esplicative
#' @param K number of neighbours considered.
#'
#' @export

knn_crossvalidation <- function(i, K=1, y, x, folds,package='FNN') {
  if(package=='FNN')
  {
    
  if (class(x) != 'numeric')
  {
    x<-x[,!sapply(as.data.frame(x[-folds[[i]],]),var)%in%0]
    pred <-
      FNN::knn(
        train = x[-folds[[i]],],
        test = x[folds[[i]],],
        cl = y[-folds[[i]]],
        k = K,
        prob = T
      )
  }
  else
  {
    pred <-
      FNN::knn(
        train = as.matrix(x[-folds[[i]]]),
        test = as.matrix(x[folds[[i]]]),
        cl = y[-folds[[i]]],
        k = K,
        prob = T
      )
  }
  }
  else 
  {
    if (class(x) != 'numeric')
    {
      x<-x[,!sapply(as.data.frame(x[-folds[[i]],]),var)%in%0]
      pred <-
        class::knn(
          train = x[-folds[[i]],],
          test = x[folds[[i]],],
          cl = y[-folds[[i]]],
          k = K,
          prob = T,use.all=F
        )
    }
    else
    {
      pred <-
        class::knn(
          train = as.matrix(x[-folds[[i]]]),
          test = as.matrix(x[folds[[i]]]),
          cl = y[-folds[[i]]],
          k = K,
          prob = T,use.all=F
        )
    }
    
  }
  prob=attr(pred,'prob')
  prob[which(pred==0)]<-1-attr(pred,'prob')[which(pred==0)]
  return(prob)
  
}

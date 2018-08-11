#' random forest cv
#'
#' fonisce le previsioni della cv, da utilizzare dentro sapply(1:k,...)
#'
#' @param i i-esimo fold
#' @param formula formula del modello
#' @param folds prodotto da kfold function
#' @param y  vettore. variabile risposta
#' @param mtry 
#' @param x matrice variabili esplicative
#'
#' @export

randomforest_crossvalidation<-function(i,formula=NULL,y,x,folds,mtry=if (!is.null(y) && !is.factor(y))
  max(floor(ncol(x)/3), 1) else floor(sqrt(ncol(x)))){
  if(class(formula)=='NULL'){
    m<- randomForest(x= x[-folds[[i]],],y=y[-folds[[i]]], mtry=mtry)
  }
  else
    {
      formula<-as.formula(formula)
      m<- randomForest(formula,data=x[-folds[[i]],], mtry=mtry)
      }
  pred <- predict(m, newdata = x[folds[[i]],],type='prob')[,2]
  pred
}

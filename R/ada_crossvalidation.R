#' adaboost cv
#'
#' fonisce le previsioni della cv, da utilizzare dentro sapply(1:k,...)
#'
#' @param i i-esimo fold
#' @param formula formula del modello
#' @param folds prodotto da kfold function
#' @param y  vettore. variabile risposta
#' @param iter numero iterazioni
#' @param x matrice variabili esplicative
#'
#' @export

ada_crossvalidation<-function(i,formula=NULL,y,x,folds,iter=200)
{
  if(class(formula)=='NULL'){
   m<- ada(x= x[-folds[[i]],],y=y[-folds[[i]]], iter=iter,nu=1, control=rpart.control(maxdepth=1,cp=-1,minsplit=0,xval=0))
  }
  else
    {
      formula<-as.formula(formula)
      m<- ada(formula,data=x[-folds[[i]],], iter=iter,nu=1, control=rpart.control(maxdepth=1,cp=-1,minsplit=0,xval=0))
    }
  n.iter<-which.min(m$model$oob.str$oobm.err)
  pred <- predict(m, newdata = x[folds[[i]],],type='probs',n.iter = n.iter)[,2]
  pred
}
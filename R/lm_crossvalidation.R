#' LM cv
#'
#' fonisce le previsioni della cv, da utilizzare dentro sapply(1:k,...)
#'
#' @param i i-esimo fold
#' @param formula formula del modello
#' @param folds prodotto da kfold function
#' @param y  vettore. variabile risposta
#' @param x matrice variabili esplicative
#'
#' @export

lm_crossvalidation<-function(i,formula=NULL,y,x,folds){
  if(class(formula)=='NULL'){
    m<- lm(y[-folds[[i]]]~., data = x[-folds[[i]],])
  }
  else 
    {
      formula<-as.formula(formula)
      m<- lm(formula, data = x[-folds[[i]],])
    }
  pred <- predict(m, newdata = x[folds[[i]],],type='response')
  pred
}

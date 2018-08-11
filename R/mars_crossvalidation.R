#' GLM cv
#'
#' fonisce le previsioni della cv, da utilizzare dentro sapply(1:k,...)
#'
#' @param i i-esimo fold
#' @param formula formula del modello
#' @param folds prodotto da kfold function
#' @param y  vettore. variabile risposta
#' @param x matrice variabili esplicative
#' @param family qualsiasi
#'
#' @export

mars_crossvalidation<-function(i,formula=NULL,y,x,folds,family=binomial){
  x<-x[,search_constant(x[-folds[[i]],])]
  if(class(formula)=='NULL'){
    m<-earth(x=x[-folds[[i]],],y=y[-folds[[i]]],degree=2,glm=list(family=family))
  }
  else
    {
      formula<-as.formula(formula)
      m<-earth(formula,data=x[-folds[[i]],],degree=2,glm=list(family=family))
          }
  pred <- predict(m, newdata = x[folds[[i]],],type='response')
  pred
}

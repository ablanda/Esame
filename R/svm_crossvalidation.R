#' LM cv
#'
#' fonisce le previsioni della cv, da utilizzare dentro sapply(1:k,...)
#'
#' @param i i-esimo fold
#' @param formula formula del modello
#' @param folds prodotto da kfold function
#' @param y  vettore. variabile risposta
#' @param x matrice variabili esplicative
#' @param kernel 
#' @param cost 
#' @param gamma 
#' @param degree 
#'
#' @export

svm_crossvalidation<-function(i,formula=NULL,y,x,folds,kernel='linear',cost,gamma=1,degree=3){
  if(class(formula)=='NULL'){
    m<- svm(x=x[-folds[[i]],],y=y[-folds[[i]]],kernel=kernel,gamma=gamma ,degree=degree,cost=cost,scale=F,probability = T)
  }
  else 
    {
      formula<-as.formula(formula)
      m<- svm(formula, data = x[-folds[[i]],],kernel=kernel,gamma=gamma ,degree=degree,cost=cost,scale=F,probability =T)
    }
  pred <- predict(m, newdata = x[folds[[i]],],probability = T)
  return(attr(pred, "probabilities")[,1])
  }

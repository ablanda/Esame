#' LM cv
#'
#' fonisce le previsioni della cv, da utilizzare dentro sapply(1:k,...)
#'
#' @param i i-esimo fold
#' @param formula formula del modello
#' @param folds prodotto da kfold function
#' @param x matrice variabili esplicative
#' @param J 
#'
#' @export

tree_crossvalidation<-function(i,formula='as.factor(y)~.',x,folds,J){
      formula<-as.formula(formula)
      m<- tree(formula, data=x[-folds[[i]],], control=tree.control(nobs=nrow(x[-folds[[i]],]), minsize=2, mindev=0.001),split='gini')
      mb<-prune.misclass(m, best=J)
      p<- predict(mb, newdata=x[folds[[i]],], type="vector")[,2]
   return(p)   
        }

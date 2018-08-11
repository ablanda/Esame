#' Gam cv
#'
#' funzione da inserire in sapply per la cv
#'
#' @param i
#' @param x 
#' @param folds 
#' @param family 
#' @param formula
#'
#' @return predictions
#' @import gam
#' @export

gam_crossvalidation<-function(i,x,folds,family=binomial)
{
  x<-x[,search_constant(x[-folds[[i]],])]
  f<-formula_gam(data=x[-folds[[i]],],kdf ='df')
  m<- gam(f, data = x[-folds[[i]],],family = family)
pred <- predict.gam(m, newdata = x[folds[[i]],],type='response')
pred
  }

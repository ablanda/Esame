#' relaxlasso_crossvalidation
#'
#' @param i 
#' @param y response
#' @param x matrix or data.frame
#' @param folds 10
#' @param family binomial
#' @param s s=c("lambda.1se","lambda.min") default lambda.min
#'
#' @return
#' @export
#'
relaxlasso_crossvalidation<-
  function (i, y, x, folds,family='binomial',s='lambda.min') 
  {
    m <- cv.glmnet(y=y[-folds[[i]]],x = as.matrix(x[-folds[[i]], ]),family=family)
    var_lasso<-intersect(colnames(x),coef(m, s = s)@Dimnames[[1]])
      
    m1<-glm(y[-folds[[i]]]~.,data=x[-folds[[i]],var_lasso[coef(m, s = s)@i]],family=family)
    
    pred <- predict(m1, newdata = x[folds[[i]], ])
    pred
  }

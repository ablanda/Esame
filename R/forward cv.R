#' Forward cross validation
#'
#' @param i 
#' @param formula 
#' @param data 
#' @param folds 
#' @param family 
#'
#' @return
#' @export
#'
#' @examples
forward_crossvalidation<-function (i, formula = NULL, data, folds, family = binomial) 
{
  l<-search_constant(data=data[-folds[[i]], ])
  data<-data[,l]
  if(is.null(formula))
  {
  m0<-glm(y~1,data=data[-folds[[i]], ],family=family)
  m1<-glm(y~.,data=data[-folds[[i]], ],family = family,control=list(maxit=30))
  }
  else
  {
    formula<-as.formula(formula)
    m1<-glm(formula,data=data[-folds[[i]], ],family = family,control=list(maxit=30))
    m0<-glm(m1$model[[1]]~1,data=data[-folds[[i]], ],family=family)
  }
  
    m<-stepAIC(m0,scope=list(lower=m0,upper=m1),diretion='forward')
  pred <- predict(m, newdata = data[folds[[i]], ], type = "response")
  pred
}
#' selezione variabili
#'
#' selezione variabili di un modello (utilizzato quando si fa il lasso)
#'
#' @param object cv.glmnet
#' @param s lambda.min o lambda.1se
#' @param data data.frame o matrix
#'
#' @export

select_varlasso <- function(object, data, s = 'lambda.min') {
  data <- as.data.frame(data)
  if(length(object$glmnet.fit$classnames)<3)
  variables <-
    attr(coef(object), 'Dimnames')[[1]][as.matrix(coef(object, s = s)) != 0]
  else variables <-
      attr(coef(object)[[1]], 'Dimnames')[[1]][as.matrix(coef(object, s = s)[[1]]) != 0]
  id<-sapply(1:ncol(data), function(i){
    if(is.factor(data[,i]))
    any(grepl(
      colnames(data)[i], substr(variables, 1, nchar(colnames(data)[i]))))
    else any(colnames(data)[i] %in% variables)
  })
  return(colnames(data)[id])
}


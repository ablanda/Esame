#' label_fun
#'
#' @param fit glmnet
#' @param ... argomenti di \code{\link{text}}
#' @param variabili vettore di caratteri delle variabili da scrivere 
#'
#' @return
#' @export
label_fun <- function(fit, variabili=NULL,...) {
  L <- length(fit$lambda)
  x <- Matrix::norm(fit$beta)
  y <- fit$beta[, L]
  labs <- names(y)
  if(is.null(variabili)) variabili<-labs
  id<-labs%in%variabili
  text(x, y[id], labels=labs[id],...)
}

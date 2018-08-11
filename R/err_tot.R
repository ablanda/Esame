#' Misclassificaation error
#'
#' @param previsti
#' @param osservati
#'
#' @return
#' @export
#'
#' @examples
err_tot<-function(previsti, osservati){
  if(is.matrix(previsti))
  previsti<-sort(unique(osservati))[apply(previsti,1,which.max)]

  if(length(unique(osservati))>length(unique(previsti)))
    previsti = factor(previsti,levels=unique(osservati) %>% sort())
  n = table(previsti,osservati)
  err.tot <- 1-sum(diag(n))/sum(n)
  return(err.tot)
  }

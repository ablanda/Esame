#' search_path
#'
#' @param object glmnet
#'
#' @return i nomi delle variabili nell'ordine in cui sono entrate
#' @export

search_path<-function(object)
{
  out<-NULL
  nomecol<-object$beta@Dimnames[[1]]
  
  for(i in 2:length(object$lambda))
  {
    
    out<-c(out,nomecol[object$beta[nomecol,i]!=0])
    nomecol<-nomecol[!object$beta[nomecol,i]!=0]
  }
  return(out)
}


#'
#'
#' Trovare quelle variabili con una varianza maggiore della soglia e i fattori con un numero di modalità maggiore di 1 (mettere soglia uguale a 0)
#' @param data data.frame
#'
#' @return
#' @export

search_constant<-function(data,soglia=0)
{
  id<-sapply(1:ncol(data), function(i)
  {
    if(length(unique(data[!is.na(data[,i]),i]))<2) FALSE
    else var(as.numeric(data[!is.na(data[,i]),i])-1)>soglia
    })
  cat('drop variables: ',colnames(data)[!id])
  return(colnames(data)[id])
}

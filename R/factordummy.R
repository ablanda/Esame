#' fattorizza le dummy
#' 
#' cerca le variabili che hanno valori unici c(0,1) e mette quelle variabili a fattori
#'
#' @param data data.frame
#'
#' @export
factordummy<-function(data)
{
  for(i in 1:ncol(data))
  {
  if(length(unique(data[,i]))<3)
    {
    data[,i]<-as.factor(data[,i])
    cat(colnames(data)[i],', ',sep='')
  }
  }
  return(data)
}
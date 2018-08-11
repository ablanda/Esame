#' Multiple factor
#'
#'convertire più righe contemporaneamente in fattori
#'
#' @param data
#' @param subset
#'
#' @examples
#' @import data.table
#' @export
multifactor<-function(data,subset=NULL) UseMethod("multifactor")

#' @return \code{NULL}
#'
#' @rdname multifactor
#' @method multifactor data.frame
#' @S3method multifactor data.frame
multifactor.data.frame<-function(data,subset=NULL){
  if(is.null(subset)) subset<-1:ncol(data)
  last<-tail(subset,n=1)
  for(i in subset)
  {
    data[,i]<-as.factor(data[,i])
    if(i!=last)
    cat(colnames(data)[i],', ',sep='')
    else cat(colnames(data)[i],'.',sep='')
  }
  cat("\n")
  cat("\n")
  return(data)
}

#' @rdname multifactor
#' @method multifactor data.table
#' @S3method multifactor data.table
multifactor.data.table<-function(data,subset=NULL){
  if(is.null(subset)) subset<-colnames(data)
  if(is.numeric(subset)) subset<-colnames(data)[subset]

    data[,c(subset):=lapply(.SD, as.factor)]
  return(data)
}


# data=dati
# subset<-colnames(dati)
# sapply(dati,class)
# sapply(data,class)
# dati[,prova,with=F]
# prova<-c("Frequenza.Importo","GenereBambino")
# dati[,eval(prova):=.(lapply(.SD, as.factor))]
# class(dati$Frequenza.Importo)
# .SDcols = colonne_fattoriali]
# dati[,(prova):=lapply(.SD, as.factor),
#   .SDcols = (prova)]

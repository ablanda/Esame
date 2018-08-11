#' Imputazione
#'
#' Sostituisce gli NA con i valori previsti dalla regressione
#'
#' @param object list. I modelli concatenati in una lista. Devono avere lo stesso ordine di \code{unique(search_NA(data,exclude=exclude))}
#' @param data data.frame
#' @param exclude stesso di search_NA
#'
#' @return dataset riempito
#' @export

imputazione<-function(object,data,exclude=NULL)
{
  impu<-search_NA(data,exclude=exclude)
  for(i in 1:length(object))
  {
    p<-predict(object[[i]],newdata = data[impu$riga[[i]],],type='response')
    if(class(p)=='matrix'){
      for(j in 1:ncol(p))
      {
        if(class(data[,impu$col[[i]][j]])=='factor'){
          data[impu$riga[[i]],impu$col[[i]][j]]<-levels(data[,impu$col[[i]][j]])[1]
          data[impu$riga[[i]],impu$col[[i]][j]][p[,j]>0.5]<-levels(data[,impu$col[[i]][j]])[2]
        }
        else {
          if(all(na.omit(data[,impu$col[[i]][j]])%%1==0)) data[impu$riga[[i]],impu$col[[i]][j]]<-round(p[,j])
          else data[impu$riga[[i]],impu$col[[i]][j]]<-p[,j]
        }
      }
    }
    else   {
      if(class(data[,impu$col[[i]]])=='factor'){
        data[impu$riga[[i]],impu$col[[i]]]<-levels(data[,impu$col[[i]]])[1]
        data[impu$riga[[i]],impu$col[[i]]][p>0.5]<-levels(data[,impu$col[[i]]])[2]
      }
      else {
        if(all(na.omit(data[,impu$col[[i]]])%%1==0)) data[impu$riga[[i]],impu$col[[i]]]<-round(p)
        else data[impu$riga[[i]],impu$col[[i]]]<-p
      }
    }
  }
  return(data)
}

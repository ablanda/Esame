#' Bilanciare
#'
#' @param data
#' @import data.table
#' @export
balance<-function(data,y='y') UseMethod("balance")

#' @rdname balance
#' @method balance data.table
#' @S3method balance data.table
# data = dati
balance.data.table<-function(data,y='y'){
    minimo = data[, .(.N), by = .(y)][
    ,min(N)]
  data<-data[,.SD[sample(.N,minimo)],by=y]
  return(data)
}

#' @rdname balance
#' @method balance data.frame
#' @S3method balance data.frame
balance.data.frame <- function(data,y='y')
{
  minimo<-min(table(data[,y]))
data<-data%>%
    group_by(y)%>%
    do(sample_n(.,minimo))
return(data)
}

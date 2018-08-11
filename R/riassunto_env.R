
#' Environment di R Studio
#'
#' @param order_by
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
riassunto_env<-function(order_by="Name",...){

  if(ls(envir=globalenv()) %>% length ==0 ) return()

  ambiente_globale<-ls(envir=globalenv())
  tabella<-data.frame(Name=ambiente_globale,Type=sapply(ambiente_globale,function(i) class(get(i))[1]),Size=rep("0",length(ambiente_globale)),row.names = NULL,stringsAsFactors = F)
  spazio<-rep(0,length(ambiente_globale))

  for(i in 1:length(ambiente_globale)){
    tabella[i,3]<-stampa(pryr::object_size(get(ambiente_globale[i])))
    spazio[i]<-pryr::object_size(get(ambiente_globale[i]))
  }
  if(order_by == "Size") return(tabella[order(spazio,...),])
  else return(tabella[order(tabella[,order_by],...),])
}

stampa<-function (x, digits = 3, ...)
{
  power <- min(floor(log(abs(x), 1000)), 4)
  if (power < 1) {
    unit <- "B"
  }
  else {
    unit <- c("kB", "MB", "GB", "TB")[[power]]
    x <- x/(1000^power)
  }
  formatted <- format(signif(x, digits = digits), big.mark = ",",
                      scientific = FALSE)
  return(paste(formatted, " ", unit, sep = ""))
}

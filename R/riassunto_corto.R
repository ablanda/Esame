#' Riassunto  corto
#'
#' @param data 
#' @param subset 
#' @param summary 
#'
#' @return
#' @export

riassunto_corto<-function(data,subset,summary=F)
  {
  j<-0
  tabella<-data.frame(ncol=subset,nome=rep(0,length(subset)),unici=rep(0,length(subset)),'NA'=rep(0,length(subset)))
  for(i in subset){
  cat(i,paste('unici=',length(unique(data[,i])),sep=''),paste('NA=',sum(is.na(data[,i])),sep=''),colnames(data)[i],'\n')
  j<-j+1
    tabella[j,2:4]<-c(colnames(data)[i],length(unique(data[,i])),sum(is.na(data[,i])))
  if(summary)
    print(summary(data[,i]))
  }
  rownames(tabella)<-NULL
  return(tabella)
}
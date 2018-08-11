#' Proporzione NA
#'
#' @param data
#' @param table
#' @param risposta
#' @param subset
#'
#' @return
#' @export
#'
#' @examples
#' # Per affiancare 2 grafici in un unica finestra:
#' library(gridExtra)
#' grid.arrange(plot1,p,nrow=1,ncol=2)
#' data=dati
na_ratio<-function(data,table=F,risposta='y',subset=NULL){

  if(is.null(subset)) {
    subset<-sapply(data, anyNA)
    ncolonne<-which(subset)
  }
  if(is.numeric(subset)) ncolonne<-subset
  if(is.character(subset)) ncolonne<-which(colnames(data)%in%subset)
  subset<-colnames(data)[ncolonne]
  subset<-subset[subset!=risposta]

  y<-as.data.frame(data)[,risposta]
  data <-
    as.data.frame(data)[, subset]
  data<-data[,sapply(data,anyNA)]

  if(is.data.frame(data)){
  df<-apply(data,2,function(i) calcolo_prop(i,y))
  }
  else {
    df<-calcolo_prop(data,y)
  }

  p <- ggplot(data=melt(df), aes(x=Var1, y=value,fill=Var2))+
    geom_bar(stat='identity')+
    facet_wrap(c("Var2"))+
    theme_bw()+
  theme(legend.position="none",legend.text=element_text(size=20),legend.title=element_blank(),axis.title=element_text(size=20),axis.text = element_text(size=20),axis.title.x=element_blank(),strip.text.x = element_text(size = 20))+
    labs(x='Presenza NA',y = "proporzione 1/0")

    if(table) return(list(p,df))
  else return(p)
}

calcolo_prop<-function(x,y){
  if(anyNA(x)){
  tabella<-table(y,is.na(x))
  tabella[1,tabella[1,]==0]<-1
  return(df<-tabella[2,]/tabella[1,])
  }
  else NULL
}

#' Boxplot
#'
#' Boxplot by ggplot2
#' @param data
#' @param subset
#'
#' @return
#' @import ggplot2 data.table
#' @export
#'
#' @examples
#'
my_boxplot<-function(data,subset='numeric') UseMethod("my_boxplot")

#' @rdname my_boxplot
#' @method my_boxplot data.frame
#' @S3method my_boxplot data.frame

my_boxplot.data.frame<-function(data,subset='numeric'){
  if((subset=='numeric')[1]) {
    subset<-sapply(data, is.numeric)
    ncolonne<-which(subset)
  }
  if(is.numeric(subset)) ncolonne<-subset
  if(is.character(subset)) ncolonne<-which(colnames(data)%in%subset)
  subset<-colnames(data)[ncolonne]

  data <-
    as.data.frame(data)[, subset]%>%
    melt(data)%>%
    na.omit()

  grafico<-ggplot(data, aes(x=variable, y=value, fill=variable)) +
    geom_boxplot()  +theme_bw()+
    theme(legend.position="none",legend.text=element_text(size=20),legend.title=element_blank(),axis.title=element_text(size=20),axis.text = element_text(size=20),axis.title.x=element_blank())+
    ylim(range(data$value))

  return(grafico)
}

#' @rdname my_boxplot
#' @method my_boxplot data.table
#' @S3method my_boxplot data.table

my_boxplot.data.table<-function(data,subset='numeric'){
  if((subset=='numeric')[1]) {
    subset<-sapply(data, is.numeric)
    ncolonne<-which(subset)
  }
  if(is.numeric(subset)) ncolonne<-subset
  if(is.character(subset)) ncolonne<-which(colnames(data)%in%subset)
  subset<-colnames(data)[ncolonne]

  data <-data[, subset,with=F]%>%
    melt(.,measure.vars = colnames(.))%>%
    na.omit()

  grafico<-ggplot(data, aes(x=variable, y=value, fill=variable)) +
    geom_boxplot()  +theme_bw()+
    theme(legend.position="none",legend.text=element_text(size=20),legend.title=element_blank(),axis.title=element_text(size=20),axis.text = element_text(size=20),axis.title.x=element_blank())+
    ylim(range(data$value))
  return(grafico)
}

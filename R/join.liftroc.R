#' join.liftroc
#'
#' funzione che unisce in un unico grafico tutte le curve lift e tutte le curve ROC
#'
#' @param g lista dei grafici da unire
#' @param legend vettore con i nomi della legenda 
#' @export
join.liftroc<-function(g,legend=NULL){
  op<-par(mfrow=c(1,2))
  b <-  sm::binning(g[[1]]$x1,g[[1]]$x2, breaks=(-0.001:10)/9.999)
  x <- c(0,seq(0.05,0.95, by=0.1),1)
  plot(x, c(g[[1]]$x2[1],b$means,1), type="b", xlim=c(0,1),
       ylim=c(1,max(sapply(g,function(i) max(i$x2)))), cex=0.75, col=1,lty=1, pch=1,
       xlab="frazione di soggetti previsti",
       ylab="fattore di miglioramento")
  abline(h=1, lty = 2, col = 3)
  
  for(i in 2:length(g)){
    b <-  sm::binning(g[[i]]$x1,g[[i]]$x2, breaks=(-0.001:10)/9.999)
    x <- c(0,seq(0.05,0.95, by=0.1),1)
    lines(x, c(g[[i]]$x2[1],b$means,1), type="b",cex=0.75, col=i, lty=i, pch=i)
  }
  if(!is.null(legend))
    legend('topright',legend=legend,col=c(1:length(g)), lty=c(1:length(g)),pch=c(1:length(g)))
  
  b <- sm::binning(g[[1]]$`1-spec`,g[[1]]$sens, breaks=(-0.001:10)/9.999)
  x <- c(0,seq(0.05,0.95, by=0.1),1)
  y<- c(0,b$means,1)
  plot(x, y, type="b", xlim=c(0,1),
       ylim=c(0,1),cex=0.75, xlab="1-specificita`",
       ylab="sensibilita`", col=1,,lty=1, pch=1)
  
  for(i in 2:length(g)){
    b <-  sm::binning(g[[i]]$`1-spec`,g[[i]]$sens, breaks=(-0.001:10)/9.999)
    x <- c(0,seq(0.05,0.95, by=0.1),1)
    y<- c(0,b$means,1)
    lines(x, c(g[[i]]$sens[1],b$means,1), type="b",cex=0.75, col=i, lty=i, pch=i)
  }
  abline(0,1, lty=2, col=3)
  if(!is.null(legend))
    legend('bottomright',legend=legend,col=c(1:length(g)), lty=c(1:length(g)),pch=c(1:length(g)))
  par(op)
}


#' soglia
#'
#' disegna su un grafico una retta verticale che rappresenta la soglia al 50%
#'
#' @param a oggetto ottenuto dalla funzione tabella.sommario
#' @param roc logical. T disegna la soglia della curva ROC,altrimenti quella della curva lift.
#' @return solo i grafici
#' @export

soglia<-function(a,b,c=0,d=0,e=0,f=0,g=0,h=0,roc=T){
  if (roc==T)
    for(i in 1:10){
      v<-eval(parse(text=paste(letters[i],'$fp',sep='')))
      abline(v=v,col=i,lty=3,cex=0.75)
      if (class(eval(parse(text=letters[i+1])))=='list') next else break
    }
  else{
    for(i in 1:10){
      v<-eval(parse(text=paste(letters[i],'$fsp',sep='')))
      abline(v=v,col=i,lty=3,cex=0.75)
      if (class(eval(parse(text=letters[i+1])))=='list') next else break
    }
  }
  legend('top',legend='soglia=0.5',bty='n',cex=0.75)
}

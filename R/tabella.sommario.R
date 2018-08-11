#' tabella.sommario
#'
#' funzione che calcola matrice di confusione e gli errori di classificazione
#'
#' @param previsti mettere un vettore di 0 o 1 (o T o F)
#' @param osservati variabile risposta
#' @return a list
#' \item{n}{tabella di frequenza tra previsti e osservati}
#' \item{'1-spec'}{numero. 1-specificita' del modello corrente,ovvero i falsi positivi}
#' \item{fsp}{frazione soggetti previsti, serve per calcolare la soglia nella curva lift}
#' @export
tabella.sommario <- function(previsti, osservati,print=T){
  previsti<-previsti>0.5
  n <-  table(previsti,osservati)

  if(var(previsti)!=0)
  {
    err.tot <- 1-sum(diag(n))/sum(n)#1-(n11+n22)/n, ovvero quante volte non ci ho preso
  fn <- n[1,2]/(n[1,2]+n[2,2])
  fp <- n[2,1]/(n[1,1]+n[2,1])
  fsp<-sum(n[2,])/sum(n)
  auc=ModelMetrics::auc(osservati,previsti)
  }
  else { err.tot <- 1-sum(n[1,1])/sum(n)#1-(n11+n22)/n, ovvero quante volte non ci ho preso
  fn <- n[1,2]/(n[1,2])
  fp <- 0
  fsp<-0
  }
  if(print){
  print(n)
  cat("falsi positivi(1-spec) & falsi negativi: ",format(c(fp, fn)),"\n")
  cat("errore totale: ", format(err.tot),"\n")
  cat("media: ",format((fp+fn)/2),"\n")
  cat("auc: ",auc,"\n")
  }
  cbind(err_tot=err.tot,fp=fp,fn=fn,media=(fp+fn)/2,auc=auc,fsp=fsp)%>%
    list(tabella=n,.)%>%
    invisible()
}

#' Tabella riassuntiva
#'
#' @param t list delle tabelle da tabella.sommario
#' @param a vettore degli auc
#' @param rownames nome delle righe
#' @param e mse
#'
#' @return
#' @export

tabella_riassunto<-function(e=NULL,previsti,osservati,nome_righe=c("err_tot","falsi_pos","falsi_neg","err_media","auc"))
{
  if(!is.null(e)){
    finale<-data.frame(mse=unlist(e))
    scala<-grep('a',attr(unlist(e),'names'))
    if(length(scala)!=0)
    {
      nome<-rep('originale',length(e))
      nome[scala]<-'logaritmica'
      finale<-cbind(nome,finale)
      colnames(finale)[colnames(finale)=='nome']<-'scala ottim.'
    }
    variabili<-grep('b',attr(unlist(e),'names'))
    if(length(variabili)!=0)
    {
      nome<-rep('tutte',length(e))
      nome[variabili]<-'selezionate '
      finale<-data.frame(variabili=nome,finale)
    }
  }
  else {
    finale<-apply(previsti,2,function(i) finale_rows(i,osservati=osservati))
    finale<-finale[-nrow(finale),]
    rownames(finale)<-nome_righe
    colnames(finale)<-sub('p_','',colnames(finale))
    finale<-t(finale)
  }

  if(!is.null(e)) finale<-finale[order(finale$mse),]
  else finale<-finale[order(finale[,"err_tot"]),]
  return(finale)
}

finale_rows<-function(i,osservati) {
  t_tmp<-tabella.sommario(i,osservati,print=F)
  return(t_tmp[[2]])
}

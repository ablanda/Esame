#' multiple numeric
#'
#'convertire più righe contemporaneamente in numerici
#' @param data data.frame
#' @param subset vector numero colonna o header
#'
#' @return
#' @export
multinumeric<-function(data,subset=NULL){
  if(is.null(subset)) subset<-1:ncol(data)
  for(i in subset)
  {
    if(!is.numeric(data[,i])){
    data[,i]<-as.numeric(data[,i])-1
    cat(colnames(data)[i],', ')}
  }
  return(data)
}
#' elimina categoria
#'
#' mette i valori corrispondenti a quella categoria a NA, elimina poi la categoria vuota
#'
#' @param data data.frame
#' @param cat vector, allora cerca elimina la categoria da tutto il dataset, lista e allora elimina categorie specifiche per ogni colonna
#'
#' @return il dataset modificato
#' @export

elim_cat<-function(data,cat)
  {
  if (NCOL(data) < 2)
    data[data %in% cat] <- NA
  else
    for(i in 1:NCOL(data)){
      data[data[,i]%in%cat,i]<-NA
      if(is.factor(data[,i])) data[,i] <- factor(data[,i])
    }
  return(data)
}

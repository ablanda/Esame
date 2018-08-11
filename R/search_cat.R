#' Ricerca categoria
#'
#' ricerca una o più categorie in un dataset
#'
#' @param data data.frame
#' @param cat vettore delle categorie
#'
#' @return quali colonne hanno quella categoria
#' @export

search_cat<-function (data, cat)
{
  data<-data[, sapply(data, class) == "factor" | sapply(sapply(data, class), length) == 2]
  x <- sapply(1:ncol(data),function(i) any(cat %in% data[,i],cat%in%levels(data[,i])))
  colnames(data)[x]
}

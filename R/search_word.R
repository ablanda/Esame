#' search word
#'
#' Gli inserisci le iniziali o le finali di una colonna e ti darà le posizioni corrispondenti
#'
#' @param word parola da cercare
#' @param data data.frame
#' @param head logical: se guardi le prime o le ultime le lettere
#' @return le posizioni corrispondenti
#' @export
search_word <- function(word, data, head=T){
  nomi <- names(data)
  quali <- c()
  for(i in 1:length(nomi)){
    if(head){
      if(substr(nomi[i],1,nchar(word))==word) quali <- c(quali,i)
    }
    else {
      if(substr(nomi[i],nchar(nomi[i])-nchar(word)+1,nchar(nomi[i]))==word) quali <- c(quali,i)

    }
  }
  quali}

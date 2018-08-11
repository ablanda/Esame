#' crea liste
#'
#' crea una lista con oggetti che iniziano per una stessa iniziale, ad es. 'm', e poi hanno un numero identificativo (es.'1:16')
#'
#' @param iniziale 
#' @param id 
#' @param ...  altri elementi da aggiungere alla lista
#'
#' @return
#' @export


create_list <- function(iniziale, id, ...)
{
  lista <-
    eval(parse(text = paste(
      'list(', paste(c(paste(iniziale, id, sep = ''), ...), sep = '', collapse =
          ','), ')'
    )))
  names(lista) <- c(paste(iniziale, id, sep = ''), ...)
  return(lista)
}

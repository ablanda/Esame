#' Search environment
#' Search an element of environment by name
#'
#' @param name 
#'
#' @return
#' @export
#'
#' @examples
search_env<-function(name){
  tabella<-Esame::riassunto_env()
  return(tabella[grep(name,tabella),])
}
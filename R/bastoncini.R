#' diagramma a bastoncini
#'
#' @param x vettore
#' @param perc 
#' @param nome 
#'
#' @return
#' @export

bastoncini <- function(x,perc=TRUE,nome=NULL) 
{
  if (is.null(nome)) nome <- substitute(x)
  nome <- as.character(nome)
  if (length(nome)>1) nome <- nome[length(nome)]
  d <- table(x)
  Ylab <- "fr. assolute"
  if (perc) {
    d <- (d/sum(d))*100
    Ylab <- "percentuale"
  }
  plot(d,ylab=Ylab,xlab=nome)
}

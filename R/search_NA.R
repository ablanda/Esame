#' Ricerca NA
#'
#' Ricerca gli NA per l'imputazine successiva
#'
#' @param data data.frame
#' @param exclude numeric. Esclude le colonne corrispondenti. Viene usato se certe colonne sono trasformazione deterministica di altre.
#'
#' @return a list
#' \item{comb}{vettore: ogni elemento dice quali variabili mancano a ogni riga con NA}
#' \item{riga}{ in base al numero di modalita' di comb, crea una lista dove il primo elemento ha gli indice di riga della 1 modalita', il secondo della seconda modalita' ecc.}
#' \item{col}{stessa cosa precedente, ma ha gli indici di colonna.}
#' @export
search_NA <- function(data, exclude = NULL)
{
  id <- which(apply(data, 2, anyNA))
  if (class(exclude) != 'NULL')
    id <- id[!id %in%exclude]
  if (length(id) == 0)
    stop('not NA')
  num.NA <- apply(is.na(data), 1, sum)
  data <- data[num.NA > 0, id]
  n <- length(num.NA[num.NA > 0])
  x <- matrix(rep('0', n), n, 2)
  colnames(x) <- c('variabili', 'classi')
  if (length(id)==1)
  {
    x[,1]<-rep(attr(id,'names'),n)
    x[,2]<-class(data)
  }
  else 
  {
  for (i in 1:n)
  {
    x[i, 1] <- paste(c(colnames(data)[is.na(data[i, ])]), collapse = ',')
    x[i, 2] <-
      paste(unlist(sapply(data, function(i) paste(class(i),collapse = '_'))[is.na(data[i, ])]), collapse = ',')
  }
  }
  num.NA[num.NA > 0] <- x[, 1]
  indice <-
    sapply(1:length(unique(x[, 1])), function(i)
      which(num.NA == unique(x[, 1])[i]))
  num.NA <- num.NA[num.NA > 0]
  idcol <-if(length(id)>1){ sapply(1:length(unique(x[, 1])), function(i)
      id[apply(data[num.NA == unique(x[, 1])[i], ], 2, anyNA)], simplify = F)} else id
  return(list(comb = x, riga = indice, col = idcol))
}

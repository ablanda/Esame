#' frqcat
#'
#' distribuzione di frequenza per le variabili categoriche sfruttando l'output della funzione summary di R
#'
#' @export

frqcat <- function(x)
{
  nmx <- names(x)
  nvar <- dim(x)[2]
  tmp  <- summary(x)
  for (i in 1:nvar) {
    a <- tmp[1,i]
    b <- leftdol(a,4)
    if (b != "Min.") {
      cat("Variabile numero",i,"-",nmx[i],"\n")
      for (j in 1:7) if (!is.na(tmp[j,i])) print(tmp[j,i])
      nmsng <- sum(is.na(x[,i]))
      cat("Osservazioni mancanti:",nmsng,"\n\n")
    }
  }
}

leftdol <- function(strdol,k)
{
  blk <- ""
  for (i in 1:k) blk <- paste(blk," ",sep="")
  adol <- paste(strdol,blk,sep="")
  bdol <- substr(adol,1,k)
  bdol
}

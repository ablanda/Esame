#' frequenze
#'
#' stampa una tabella con le frequenze di ogni singola modalita'
#'
#' @param x vettore
#' @param head vettore delle frequenze che si vuol selezionare
#' @param cumul se vuoi le frequenze cumulate
#' @param reverse dal più piccolo al più grande
#'
#' @export
frequenze   <- function(x,cumul=FALSE,head=NULL,reverse=F)
{
  mult <- 100
  f <- if(is.null(head)) table(x) else {table(x)[head]}
  f<-f[!is.na(f)]
  #print(reverse)
  if(reverse) f<-rev(f)
  t <- sum(f)
  pcg <- round((f/t)*mult,2)
  F <- cumsum(f)
  PCG <- round((F/t)*mult,2)
  nf <- names(f)
  n <- length(f)
  f <- append(f,t)
  pcg <- append(pcg,sum(pcg))
  nf <- append(nf,length(nf))

  F <- append(F,NA)
  PCG <- append(PCG,NA)

  FX <- cbind(format(nf),format(f),format(pcg))
  if (cumul) FX <- cbind(format(names(f)),format(f),format(pcg),format(F),format(PCG))
  nch <- nchar(FX[1,])+2
  r <- ""
  nii <- 3
  if (cumul) nii <- 5
  for (i in 1:nii) {
    r <- paste(r,"+",sep="")
    for (j in 1:nch[i]) r <- paste(r,"-",sep="")
  }
  riga <- paste(r,"+",sep="")

  abcd <- c("x ","n ","f ")
  if (cumul) abcd <- c("x ","n ","f ","N ","F ")

  r <- ""
  for (i in 1:nii) {
    r <- paste(r," ",sep="")
    for (j in 1:(nch[i]-2)) r <- paste(r," ",sep="")
    r <- paste(r,abcd[i],sep="")
  }
  intesta <- r

  cat(intesta,"\n")
  cat(riga,"\n")
  for (i in 1:n) {
    r <- paste("|",FX[i,1],"|",FX[i,2],"|",FX[i,3],"|")
    if (cumul) r <- paste("|",FX[i,1],"|",FX[i,2],"|",FX[i,3],"|",FX[i,4],"|",FX[i,5],"|")
    cat(r,"\n")
  }
  cat(riga,"\n")
  if (!cumul) {
    r <- paste(" ",FX[n+1,1]," ",FX[n+1,2]," ",FX[n+1,3]," ")
    cat(r,"\n")
  }
  cat("Osservazioni mancanti:",sum(is.na(x)),"\n\n")
}

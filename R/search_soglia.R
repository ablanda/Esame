#' Cercare la soglia
#'
#' Cercare la soglia per avere un determinato numero di falsi negativi
#'
#' @param previsti mettere le probabilità di 1
#' @param g la variabile risposta
#' @param fn percentuale di falsi negativi
#' @export
# soglia1a<-mean(sapply(1:k,function(i) search_soglia(p1a[[i]],y[folds[[i]]],t1$fn)))

search_soglia<-function(previsti, g,fn)
{
ind <- rev(order(previsti))
  u2<- cumsum(g[ind])/sum(g)
i0<-order(abs(u2-1+fn))[1]
return(previsti[ind[i0]])
}
function(fn)
search_soglia(p1a,y,t1$fn)

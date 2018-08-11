#' Lift e roc curve
#'
#' disegna la funzione e ti calcola la specificita' e sensitivita'
#' @param previsti mettere le probabilità di 1
#' @param g la variabile risposta
#' @param type crude, bin o sm. default bin
#' @param plot.it  logical. Se vuoi che ti disegni il grafico
#' @param soglia la soglia desiderata verra tracciata. default 0.5
#' @return a list: i primi 2 elementi servono per la curva lift, gli altri 2 per la curva roc
#' \item{x1}{vettore, proporzione cumulata dei soggetti previsti}
#' \item{x2}{fattore di miglioramento, ovvero \code{(n22/n2.)/(n.2/n)}. Ovvero faccio la somma dei soggetti previsti al diminuire della soglia (n22) fratto la proporzione degli 1 (n.2/n) fratto il numero di soggetti che ho classificato come 1 (n2.)}
#' \item{'1-spec'}{ vettore 1-specificita'}
#' \item{sens}{ vettore sensitivita'}
#' @import sm
#' @export

lift_roc<- function(previsti, g, type="bin", plot.it=TRUE,soglia=0.5)
{
  op<-par(mfrow=c(1,2))
  if(!is.numeric(g)) stop("g not numeric")
  ind <- rev(order(previsti))#ordina in maniera decrescente previsti creando un vettore con l'indice corrispondente della posizione originaria
  n <- length(g)
  i0<-order(abs(previsti-soglia))[1]
  x1 <-  (1:n)/n#proporzione cumulata
  x2 <- cumsum(g[ind])/(mean(g)*(1:n))
  q0<-sum(previsti>previsti[i0])/n
  #cumsum(g[ind])=metto in ordine il vettore risposta in ordine decrescente secondo le probabilità stimate e faccio la somma cumulata. Ovvero faccio la somma dei soggetti previsti al diminuire della soglia (n22)
  #mean(g)= proporzione degli 1 (n.2)/n
  #(1:n)=il numero di soggetti che ho classificato come 1 (n2.)
  #(1:n)/n=frazione di soggetti previsti
  if(type=="crude" & plot.it)
    plot(x1, x2, type="l", col=2,
         xlab="frazione di soggetti previsti", ylab="lift")

    if(type=="sm") {
    a<- sm::sm.regression(x1, x2, h=0.1, display="none")
    if(plot.it)
      plot(a$eval, a$estimate, type="l",xlim=c(0,1), col=2,
           xlab="frazione di soggetti previsti", ylab="lift")
  }
  if(type=="bin") {
    b <-  sm::binning(x1,x2, breaks=(-0.001:10)/9.999)#prende il primo vettore e lo suddivide mediante intervalli breaks, calcola una media perturbata e la frequenza in ogni intervallo. Ordina in maniera crescente il secondo vettore e lo suddivide come il primo e calcola media, somma e sd di quest'ultimi valori ripartiti.
    x <- c(0,seq(0.05,0.95, by=0.1),1)
    if(plot.it) plot(x, c(x2[1],b$means,1), type="b", xlim=c(0,1),
                     ylim=c(1,max(x2)), cex=0.75, col=2,
                     xlab="frazione di soggetti previsti",
                     ylab="fattore di miglioramento")
    x1<- x
    x2<- c(x2[1],b$means,1)
  }
  abline(v=q0,col=2,lty=3)
  text(q0,0,paste('Threshold=',format(soglia),sep='',collapse=NULL))
  abline(h=1, lty = 2, col = 3)
  
  u1<- cumsum(1-g[ind])/sum(1-g)#1-specificità=alpha. g codificata come 0 1, 1-g codificata come 1 0, quindi cumsum(1-g[ind])= n21 facendo variare la soglia ottengo il numero di falsi positivi
  u2<- cumsum(g[ind])/sum(g)#sensibilità
  q0<-u1[sum(previsti>previsti[i0])]
  if(type=="crude" & plot.it)
    plot(u1, u2, type="l", xlim=c(0,1), ylim=c(0,1), col=2,
         xlab="1-specificita`", ylab="sensibilita`")
  abline(v=q0,col=2,lty=3)
  text(q0,0,paste('Threshold=',format(soglia),sep='',collapse=NULL))
  if(type=="sm") {
    # browser()
    eps<- 0.00001
    a<- sm::sm.regression(u1,log((u2+eps)/(1-u2+2*eps)), h=0.1, display="none")
    q<- exp(a$estimate)/(1+exp(a$estimate))
    if(plot.it) plot(a$eval, q, type="l", xlim=c(0,1), ylim=c(0,1),
                     xlab="1-specificita`", ylab="sensibilita`", col=2)
    abline(v=q0,col=2,lty=3)
    text(q0,0,paste('Threshold=',format(soglia),sep='',collapse=NULL))
  }
  if(type=="bin") {
    b <- sm::binning(u1,u2, breaks=(-0.001:10)/9.999)
    x <- c(0,seq(0.05,0.95, by=0.1),1)
    y<- c(0,b$means,1)
    if(plot.it)
      plot(x, y, type="b", xlim=c(0,1),
           ylim=c(0,1),cex=0.75, xlab="1-specificita`",
           ylab="sensibilita`", col=2)
    u1<- x
    u2<- y
    abline(v=q0,col=2,lty=3)
    text(q0,0,paste('Threshold=',format(soglia),sep='',collapse=NULL))
  }
  if(plot.it) {
    abline(0,1, lty=2, col=3)
  par(op)
  }
  invisible(list(x1=x1,x2=x2,'1-spec'=u1,sens=u2))
}

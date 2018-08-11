#' plot principal components
#'
#' @param pc object princomp
#' @param npc numero pc da plottare
#' @param cumsum quale dei 2 grafici
#'
#' @return
#' @export
plot.pc<-function(pc,npc=min(10, length(pc$sdev)),cumsum=F) 
  {
    if(!cumsum)
  plot(1:npc,((pc$sdev^2)/sum(pc$sdev^2))[1:npc],type='b',xlab='principal components',ylab='Proportion of Variance')
    else 
      plot(1:npc,cumsum((pc$sdev^2)/sum(pc$sdev^2))[1:npc],type='b',xlab='principal components',ylab='Proportion of Variance')
}

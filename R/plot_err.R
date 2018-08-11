#' grafico errore di classificazione
#'
#' @param m lista oggetti randomforest o ada
#' @param legend vettore dei nomi della legenda
#'
#' @return
#' @export
plot_err <- function(m, legend = NULL,ylab = 'classification error',oob=T)
{
 if(oob)
  {
    y <-
    sapply(1:length(m), function(i) {
      if (any(class(m[[i]]) %in% "randomForest"))
        m[[i]]$err.rate[, 1]
      if ((any(class(m[[i]]) %in% "ada")))
      m[[i]]$model$oob.str$oobm.err
      if(is.vector(m[[i]]) & length(m[[i]])>1) m[[i]]
      else rep(m[[i]],500)
    },simplify = F)
  }
   else {
     y <-
    sapply(1:length(m), function(i) {
      if (any(class(m[[i]]) %in% "randomForest"))
        m[[i]]$test$err.rate[, 1]
      if ((any(class(m[[i]]) %in% "ada"))) m[[i]]$model$errs[,3]
      else{
      if(is.vector(m[[i]]) & length(m[[i]])>1) m[[i]]
      else {rep(m[[i]],500)}
    } },simplify = F)
      }
  x <- min(sapply(y, function(i)
    length(i)))
  plot(
    1:x,
    y[[1]][1:x],
    type = 'l',
    xlim = c(1, x),
    ylim = c(min(unlist(y)), max(unlist(y))),
    xlab = 'Number of trees',
    ylab =ylab
  )
  sapply(2:length(y), function(i)
    lines(1:x, y[[i]][1:x], type = 'l', col = i,lty=i))
  if (!is.null(legend))
    legend(
      'topright',
      legend = legend,
      col = c(1:length(m)),
      lty = c(1:length(m))
      )
}

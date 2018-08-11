#' Variable importance plot
#' 
#' Altra possibilità di grafico rispetto a vip
#'
#' @param object random forest object or vector
#' @param method 1, 2,3. 1= Mean decrease in accuracy, 2=Mean decrease in Gini Index, se l'oggetto non è random forest serve solo come titolo del grafico. 3=Relative importance
#' @import dplyr
#' @export

vip2 <- function(object, method = 2,max.var.show=30,cex.names = 0.8, cex.axis=0.8,cex.lab=0.8,ylim=c(0,mvs),xlab=NULL,...) {
  op <- par(mai = c(1,1.5,1,0.2))
  if(is.vector(object))
  {
    mvs<-min(length(object),max.var.show)
    if(is.null(xlab))
    {
      if(method==1) xlab='Mean decrease in accuracy'
      if(method==2) xlab='Mean decrease in Gini Index'
      if(method==3) xlab='Relative importance'
    }
      barplot(
        sort(object*100/max(object),decreasing = T)[1:mvs],
        horiz = T,
        las=1,
        cex.names = cex.names, cex.axis=cex.axis,cex.lab=cex.lab,ylim=c(0,mvs),
        xlab = xlab,
        col = 'red',...
      )
  }
  else {
    var_importance <-
      data_frame(
        variable = attr(object$importance, 'dimnames')[[1]] ,
        importance = as.vector(object$importance[, method])
      )
    var_importance <- dplyr::arrange(var_importance, desc(importance))
    var_importance$variable <-
      factor(var_importance$variable, levels = var_importance$variable)
    var_importance$importance <-
      var_importance$importance * 100 / max(var_importance$importance)
    mvs<-min(length(var_importance$importance),max.var.show)
    
    if (method == 2)
    {
      barplot(
        var_importance$importance[1:mvs],
        names.arg = var_importance$variable[1:mvs],
        horiz = T,
        las = 1,
        cex.names = cex.names, cex.axis=cex.axis,cex.lab=cex.lab,ylim=c(0,mvs),
        xlab = "Mean decrease in Gini Index",
        col = 'red',...
      )
    }
    else
      barplot(
        var_importance$importance[1:mvs],
        names.arg = var_importance$variable[1:mvs],
        horiz = T,
        las = 1,
        cex.names = cex.names, cex.axis=cex.axis,cex.lab=cex.lab,ylim=c(0,mvs),
        xlab = "Mean decrease in accuracy",
        col = 'red',...
      )
  }
  par(op)
}
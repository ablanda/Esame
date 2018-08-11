#' Importanza variabili

# Extracts variable importance (Mean Decrease in Gini Index)
# Sorts by variable importance and relevels factors to match ordering
#'
#' @param object class randomforest
#' @import dplyr ggplot2
#' @export
#'
#' @examples
vip<-function(object)
  {
var_importance <- data_frame(variable=attr(object$importance,'dimnames')[[1]] ,
                             importance=as.vector(importance(object)))
var_importance <- dplyr::arrange(var_importance, desc(importance))
var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)

p <- ggplot2::ggplot(var_importance, ggplot2::aes(x=variable, weight=importance, fill=variable))
p <- p + ggplot2::geom_bar() + ggplot2::ggtitle("Variable Importance from Random Forest Fit")
p <- p + xlab("Demographic Attribute") + ylab("Variable Importance (Mean Decrease in Gini Index)")
p <- p + ggplot2::scale_fill_discrete(name="Variable Name")
p + ggplot2::theme(axis.text.x=element_blank(),
          axis.text.y=element_text(size=12),
          axis.title=element_text(size=16),
          plot.title=element_text(size=18),
          legend.title=element_text(size=16),
          legend.text=element_text(size=12))

}
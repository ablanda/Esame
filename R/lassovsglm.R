#' Title
#'
#' @param cv.lasso object from cv.glmnet
#' @param glm glm
#'
#' @return un dataframe con i 2 coefficienti e la significatività del glm
#' @export

lassovsglm<-function(cv.lasso,glm)
{
pvals <- summary(glm)$coef[,4]
sigSymbols <- symnum(pvals, na = FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", "*", ".", " "))
lassovsglm<-data.frame(coef.lasso=coef(lasso, s="lambda.min")@x,coef.glm=glm$coef[-1],pvals=pvals,Signif=sigSymbols)
colnames(lassovsglm)[3]<-"Pr(>|z|)"
return(lassovsglm)
}

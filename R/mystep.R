#' mystep
#'
#' step un po' più veloce in parallelo, la funzione non crea i cluster in automatico
#'
#' @param object modello completo glm
#' @return modello migliore in base all'aic
#' @import snowfall

mystep<-function(object)
{
  h<-paste(colnames(object$model)[1],'~',sep='')
  variables<-setdiff(names(attr(object$term,'dataClasses'))[-1],select.var(object))
  #ciclo for
  a124<-a123<-1e9
  for (i in 1:length(variables))
  {
    a124<-a123
    a123<-sfSapply(variables,function(i) glm(as.formula(paste(h,paste(c(i,select.var(object)),collapse='+'))),family=object$family,data=object$model)$aic)
    if (min(a123)>min(a124)) break
    h<-paste(h,variables[which.min(a123)],'+')
    variables<-variables[-which.min(a123)]
  }
  return(mf<-glm(as.formula(paste(h,paste(select.var(m2),collapse='+'))),family=object$family,data=object$model))
}

# object<-m1
# mystep<-function(object,formula=NULL,y,x)
# {
#   a124<-a123<-1e9
#   for (i in 1:length(variables))
#   {
#     a124<-a123
#     a123<-sfApply(x,2,function(i) glm(y~.-i,family=object$family,data=x)$aic)
#     if (min(a123)>min(a124)) break
#     h<-paste(h,variables[which.min(a123)],'+')
#     variables<-variables[-which.min(a123)]
#   }
#   return(mf<-glm(as.formula(paste(h,paste(select.var(m2),collapse='+'))),family=object$family,data=object$model))
# }

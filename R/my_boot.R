#' Title
#'
#' @param data data.frame
#' @param folds a vector
#' @param time1 quante volte il campione originale deve essere ingrandito
#'
#' @return
#' @export
#'
#' @examples
my_boot<-function(data,folds,time1=10)
{
  
  data=data[folds,]
  data1<- data[data$y==1, ]
  data0<- data[data$y==0, ]
  acaso1 <- sample(1:nrow(data1), nrow(data1)*time1,replace=T)
  acaso0 <- sample(1:nrow(data0), nrow(data1)*time1)
  data<-rbind(data0[acaso0,],data1[acaso1,])
  return(data)
}

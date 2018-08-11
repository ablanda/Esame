#' Title
#'
#' @param data 
#' @param train_size 
#' @param test_size 
#' @param seed 
#' @param stratify 
#'
#' @return
#' @export
#'
#' @examples
train_test_split<-function(data,train_size,test_size,seed=0,stratify=F){
  
  if(!stratify){
    set.seed(seed)
  folds <- sample(1:nrow(data),nrow(data))
  if((train_size+test_size)<1){
    resto = 1-(train_size+test_size)
    folds<-folds[-(1:round(length(folds)*resto))]
  }
  train_size_n= train_size/(train_size+test_size)
  return(list(folds[1:round(length(folds)*train_size_n)],folds[-(1:round(length(folds)*train_size_n))]))
  }
  else {
    NULL
  }
}
#' Formula per gam
#'
#'crea una formula additiva dove lascia inalterato i fattori, le altre le racchiude tra s().
#'
#' @param data
#' @param k
#'
#' @return
#' @export

formula_gam <- function(x,y=NULL,kdf=NULL,formula_y='as.factor(y)')
{
  if(is.null(y)) y<-x$y
  data<-x[,!colnames(x)=='y']
  fattori <-
    colnames(data)[which(sapply(data, is.factor) |
                           sapply(sapply(data, class), length) == 2)]
  if(length(fattori)==0) nonfattori<-colnames(data)
  else
  nonfattori <-
    colnames(data)[-which(sapply(data, is.factor) |
                            sapply(sapply(data, class), length) == 2)]
  k<-sapply(nonfattori,function(i) length(unique(data[,i])))
  tol<-sapply(nonfattori,function(i) IQR(data[,i]))
  fattori<-c(nonfattori[k<4 | tol==0],fattori)
    nonfattori<-nonfattori[k>3 & tol!=0]
  df<-ceiling(sapply(nonfattori, function(i) smooth.spline(data[,i],y,cv=F)$df))
  if(length(fattori)==0)
  {
  if(!is.null(kdf))
  {
  # nonfattori1 <-
  #   nonfattori[sapply(1:length(nonfattori), function(i)
  #     length(unique(datib[, nonfattori[i]]))) > 3]
  # nonfattori2 <-
  #   nonfattori[!sapply(1:length(nonfattori), function(i)
  #     length(unique(datib[, nonfattori[i]]))) > 3]

    f <-
    formula(paste(formula_y,
      "~s(",
  paste(nonfattori,collapse = ")+s(",',',kdf,'=',df),
      ')'    ))
  }
  else {
  f <-
    formula(paste(formula_y,
      "~s(",
      paste(nonfattori,collapse = ")+s("),
      ')'))
  }
  }
  if(length(nonfattori)==0)
  {
    f <-
    formula(paste(formula_y,
      "~", paste(c(fattori), collapse = '+')
    ))
  }
    if(length(nonfattori)!=0 & length(fattori)!=0)
    {
    if(!is.null(kdf))
  {
    f <-
    formula(paste(formula_y,
      "~s(",
  paste(nonfattori,collapse = ")+s(",',',kdf,'=',df),
      ')+',
     paste(c(fattori), collapse = '+')
    ))
  }
  else {
  f <-
    formula(paste(formula_y,
      "~s(",
      paste(nonfattori,collapse = ")+s("),
      ')+',
      paste(c(fattori), collapse = '+')
    ))
  }
}
  return(f)
}


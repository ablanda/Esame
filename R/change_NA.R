#' sostituisce NA
#'
#'sostituisce gli NA con un value
#' @param data
#' @param subset
#' @param value
#'
#' @return
#' @export

change_NA<-function(data,value)
{
  if (NCOL(data) < 2)
    data[is.na(data)]<-value
  else
    for(i in 1:NCOL(data))
        data[,i] <- change_value(data[,i],value)
 return(data)
}

change_value<-function(data,
                       value) UseMethod("change_value")

#' @rdname change_value
#' @method change_value factor
#' @S3method change_value factor
change_value.factor<-function(data,
                              value){
  if (!value %in% levels(data))
  {
    data <-
      factor(data, levels = c(levels(data), value))
  }
    data[is.na(data)] <- value
  data <- factor(data)
  return(data)
}

#' @rdname change_value
#' @method change_value numeric
#' @S3method change_value numeric
#' @S3method change_value factor
change_value.numeric<-function(data,
                               value){
    data[is.na(data)] <- value
  return(data)
}

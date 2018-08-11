#' Convert to datetime
#' convert string or number to POSIXct
#'
#' @param data
#' @param pattern
#' @param columns
#' @param format
#'
#' @return
#' @export
#'
#' @examples
to_datetime <- function(data,pattern=NULL,columns=NULL,format = "%Y-%m-%d %H:%M:%S")
{
  data <- data %>% as.data.frame
  if(!is.null(pattern))
    columns <- grep(pattern,colnames(data))
  if(is.null(format)) data[,columns] <- data[,columns] %>% sapply(anytime)
  else
  data[,columns] <- data[,columns] %>% sapply(function(i) strptime(i,format=format))
  return(data)
}

#' Root Mean Squared Log Error
#'
#' @param previsti
#' @param osservati
#'
#' @return
#' @export
#'
#' @examples
rmsle <- function(previsti,osservati){

error = ((log( previsti+1)-log(osservati+1))^2) %>% mean %>% sqrt

return(error)
}

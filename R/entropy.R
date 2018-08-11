#' Entropy
#'
#' @param cat.vect
#'
#' @return
#' @export
#'
#' @examples
entropy = function(cat.vect,useNA){
  px  = table(cat.vect,useNA=useNA)/NROW(cat.vect)
  lpx = log(px, base=exp(1))
  ent = -sum(px*lpx)
  return(ent)
}

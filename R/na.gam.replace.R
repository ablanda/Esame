#'Missing Data Filter for GAMs
#'
#' Cambiato leggermente la funzione na.gam.replace perchè possa essere utilizzata con i data.frame
#'
#' @param frame data frame
#'
#' @details \code{\link{na.gam.replace}}
#' @export
ab.na.gam.replace<-function (frame)
{
  vars <- names(frame)
  for (j in vars) {
    x <- frame[[j]]
    pos <- is.na(x)
    if (any(pos)) {
      if (length(levels(x))) {
        xx <- as.character(x)
        xx[pos] <- "NA"
        x <- factor(xx, exclude = NULL)
      }
      else if (is.matrix(x)) {
        ats <- attributes(x)
        w <- !pos
        x[pos] <- 0
        n <- nrow(x)
        TT <- array(1, c(1, n))
        xbar <- (TT %*% x)/(TT %*% w)
        xbar <- t(TT) %*% xbar
        x[pos] <- xbar[pos]
        attributes(x) <- ats
      }
      else {
        ats <- attributes(x)
        x[pos] <- mean(x[!pos])
        attributes(x) <- ats
      }
      frame[[j]] <- x
    }
  }
  frame
}

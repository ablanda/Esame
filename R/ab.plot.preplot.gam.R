#' Title
#'
#' @param x 
#' @param y 
#' @param residuals 
#' @param rugplot 
#' @param se 
#' @param scale 
#' @param fit 
#' @param ... 
#'
#' @return
#' @export

ab.plot.preplot.gam<-function (x, y = NULL, residuals = NULL, rugplot = TRUE, se = FALSE, 
    scale = 0, fit = TRUE, ...) 
{
    listof <- inherits(x[[1]], "preplot.gam")
    if (listof) {
        TT <- names(x)
        scales <- rep(0, length(TT))
        names(scales) <- TT
        for (i in TT) scales[i] <- ab.plot.preplot.gam(x[[i]], y = NULL, 
            residuals, rugplot, se, scale, fit, ...)
        invisible(scales)
    }
    else {
        dummy <- function(residuals = NULL, rugplot = TRUE, se = FALSE, 
            scale = 0, fit = TRUE, ...) c(list(residuals = residuals, 
            rugplot = rugplot, se = se, scale = scale, fit = fit), 
            list(...))
        d <- dummy(residuals, rugplot, se, scale, fit, ...)
        uniq.comps <- unique(c(names(x), names(d)))
        Call <- c(as.name("ab.gplot.factor"), c(d, x)[uniq.comps])
        mode(Call) <- "call"
        invisible(eval(Call))
    }
}

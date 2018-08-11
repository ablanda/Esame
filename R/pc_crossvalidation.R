#' principal component regression predictions cv
#'
#' fonisce le previsioni della cv, da utilizzare dentro sapply(1:k,...)
#'
#' @param i i-esimo fold
#' @param folds prodotto da kfold function
#' @param y  vettore. variabile risposta
#' @param x matrice variabili esplicative
#' @param family qualsiasi
#' @param cor logical. if correlation or covariance
#' @param npc numero componenti principali
#'
#' @export

pc_crossvalidation <- function(i,
                               y,
                               x,
                               folds,
                               family = binomial,
                               cor = T,
                               npc) {
  x<-x[,!sapply(as.data.frame(x[-folds[[i]],]),var)%in%0]
  pc <- princomp(x[-folds[[i]], ], cor = cor)
  pc.x <- pc$scores[, 1:npc]
  pc.x <- as.data.frame(pc.x)
  m <- glm(y[-folds[[i]]] ~ ., data = pc.x, family = family)
  pc.x <- predict(pc, newdata = x[folds[[i]], ])[, 1:npc]
  pc.x <- as.data.frame(pc.x)
  pred <- predict(m, newdata = pc.x, type = 'response')
  pred
}

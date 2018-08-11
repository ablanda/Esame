#' principal component regression predictions cv
#'
#' fonisce le previsioni della cv, da utilizzare dentro sapply(1:k,...)
#'
#' @param i i-esimo fold
#' @param folds prodotto da kfold function
#' @param y  vettore. variabile risposta
#' @param x matrice variabili esplicative
#' @param cor logical. if correlation or covariance
#' @param npc numero componenti principali
#' @param K  numero vicini più prossimi
#' @param use.all quando più di K punti sono ugualmente vicini, se prenderli tutti o sceglierne K a caso.
#'
#' @export

pcknn_crossvalidation <- function(i,
                                  y,
                                  x,
                                  folds,
                                  cor = T,
                                  npc,
                                  K,
                                  use.all = F) {
  x<-x[,!sapply(x[-folds[[i]],],var)%in%0]
  if (class(x) != "numeric") {
    pc <- princomp(x[-folds[[i]], ], cor = cor)
    pc.train <- pc$scores[, 1:npc]
    pc.test <- predict(pc, newdata = x[folds[[i]], ])[, 1:npc]
  }
  else {
    pc <- princomp(x[-folds[[i]]], cor = cor)
    pc.train <- pc$scores[, 1:npc]
    pc.test <- predict(pc, newdata = x[folds[[i]]])[, 1:npc]
  }
  if (npc == 1)
  {
    pred <-
      class::knn(
        train = as.matrix(pc.train),
        test = as.matrix(pc.test),
        cl = y[-folds[[i]]],
        k = K,
        prob = T,
        use.all = use.all
      )
  }
  else
    pred <-
      class::knn(
        train = pc.train,
        test = pc.test,
        cl = y[-folds[[i]]],
        k = K,
        prob = T,
        use.all = use.all
      )
  
  prob = attr(pred, "prob")
  prob[which(pred == 0)] <- 1 - attr(pred, "prob")[which(pred ==
                                                           0)]
  return(prob)
}


#' Grafici a barre
#'
#' Puo' disegnare 2 grafici a barre, ho riportare le rispettive tabelle di frequenza.
#'
#' @param subset colonne delle esplicative selezionate
#' @param data data.frame: la variabile risposta deve y
#'
#' @return a list: le tabelle di frequenza con cui ha fatto i grafici
#' @export
freq <- function(data, subset) {
  data<-ab.na.gam.replace(data)
  if (length(subset) > 1)
  {
    tabella <- list()
    for (i in 1:length(subset))
    {
      tabella <-
        c(tabella, round((table(data[, subset[i]], data$y)[, 2] + 0.5) / (table(data[, subset[i]], data$y)[, 1] +
            0.5), 2))
      tabella[[i]] <-
        data.frame(freq = tabella[[i]], table(data[, subset[i]], data$y)[, 1], table(data[, subset[i]], data$y)[, 2])
      barplot(tabella[[i]][, 1])
      colnames(tabella[[i]]) <- c('freq', '0', '1')
      tabella[[i]] <-
        tabella[[i]][order(tabella[[i]][, 1], decreasing = T), ]
      tabella[[i]] <-
        cbind(categoria = rownames(tabella[[i]]),
          tabella[[i]],
          sum = rowSums(tabella[[i]][, 2:3]))
      rownames(tabella[[i]]) <- NULL
    }
  }
  else
  {
    tabella <-
      round((table(data[, subset], data$y)[, 2] + 0.5) / (table(data[, subset], data$y)[, 1] + 0.5), 2)
    tabella <-
      data.frame(freq = tabella, table(data[, subset], data$y)[, 1], table(data[, subset], data$y)[, 2])
    barplot(tabella[, 1])
    colnames(tabella) <- c('freq', '0', '1')
    tabella <- tabella[order(tabella[, 1], decreasing = T), ]
    tabella <-
      cbind(categoria = rownames(tabella),
        tabella,
        'sum' = rowSums(tabella[, 2:3]))
    rownames(tabella) <- NULL
  }
  return(tabella)
}
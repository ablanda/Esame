#' Trovare le variabili leakers
#'
#' cerca le variabili leakers
#'
#' @param data data.frame
#'
#' @return una tabella con le variabili che hanno ottenuto r^2 piu' alto sulla risposta e una con le variabili che hanno un tempo eccessivo di lm
#' @export

search_leakers <- function(data, soglia = 0.5) {
  leakers <- NULL
  data<-data[, !sapply(1:NCOL(data), function(i)
      all(duplicated(data[, i])[-1]))]
  cat<-search_vuote(data)
  if(all(!is.null(cat), length(cat)>0)) data<-elim_cat(data,cat)
  n <- search_word('y', data)
  for (i in 1:NCOL(data)) {
    if (i != n) {
      if (nlevels(data[, i]) > 100)
      {
        cat(i,
            colnames(data)[i],
            nlevels(data[, i]),
            sep = ' ',
            fill = T)
        leakers <-
          rbind(leakers,
                data.frame(
                  ncol = i,
                  nomi = colnames(data)[i],
                  r= nlevels(data[, i])
                ))
      }
      else
      {
        m0 <-
          summary(lm(as.numeric(y) ~ data[, i], data = data))
        if (m0$r.squared > soglia) {
          cat(
            i,
            colnames(data)[i],
            round(m0$r.squared, 3),
            sep = ' ',
            fill = T
          )
          leakers <-
            rbind(leakers,
                  data.frame(
                    ncol = i,
                    nomi = colnames(data)[i],
                   r= round(m0$r.squared, 3)
                  ))
        }
      }
    }
  }
  if(!is.null(leakers)) colnames(leakers)[3]<-'r^2 & nlev'
  return(leakers)
}

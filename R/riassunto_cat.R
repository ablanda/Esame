#' Riassunto delle variabili categoriali
#'
#' Analisi delle variabili categoriali
#'
#' @param data data.frame
#' @param dropNA logical: se vuoi la varianza delle variabili considerando gli NA come categoria.
#' @param freqmod logical: se vuoi l'elenco delle categoria e la loro numerosita, e la loro somma cumulata
#' @param subset numero delle colonne, nome delle colonne o \code{'factor'}.
#' @return a list: il primo elemento da l'elenco delle categorie, e la somma cumulata da utilizzare quando si decidono di aggregare le categorie.
#' Il secondo elemento è una matrice contenente la percentuale di valori mancanti, oss*livello e la varianza eliminando le oss mancanti, opzionale la 4 colonna varNA.
#' @examples
#' @import data.table
#' @export
riassunto_cat<-function(data,
                        dropNA = F,
                        freqmod = F,subset=1:ncol(data),order_by = 'ncol',...) UseMethod("riassunto_cat")

#' @rdname riassunto_cat
#' @method riassunto_cat data.frame
#' @S3method riassunto_cat data.frame

riassunto_cat.data.frame <- function(data,
                          dropNA = F,
                          freqmod = F,subset=1:ncol(data),order_by = 'ncol',...)
{
    if((subset=='factor')[1]) {
        subset<-sapply(data, function(i) is.factor(i) | is.character(i))
        ncolonne<-which(subset)
    }
    if(is.numeric(subset)) ncolonne<-subset
    if(is.character(subset)) ncolonne<-which(colnames(data)%in%subset)
    subset<-colnames(data)[ncolonne]
    data <-
    as.data.frame(data[, subset])
 # data<-multifactor(data)
  nlev = sapply(data, function(i) length(unique(i)))
  ossXlev <- round(nrow(data) / nlev, 2)
  percNA <- round(sapply(data, function(i) sum(is.na(i))*100 / NROW(i)), 2)
  entropia<-sapply(data,function(i) round(Esame::entropy(i),3))
  if (freqmod)
  {
    for(i in 1:ncol(data)){
      print(subset[i])
      cat(Esame::frequenze(data[,subset[i]],cumul = T,reverse = T))
    }
  }
  if (!dropNA)
  {
    data <- Esame::ab.na.gam.replace(data)
    entropia_notNA <-sapply(data,function(i) round(Esame::entropy(i),3))

    out <- data.frame(ncol=ncolonne,
                      perc.NA = percNA,
                      nlevels = nlev,
                      #oss*level = ossXlev,
                      entropia=signif(entropia,3),
                      entropia_notNA=signif(entropia_notNA,3)

    )
  }
  else
      out <- data.frame(ncol=ncolonne,
        perc.NA = percNA,
        nlevels = nlev,
       'oss*level' = ossXlev,
       entropia=signif(entropia,3)
    )
  if(length(subset)==ncol(data)) out <- cbind(out,class=codifiche)
  out <- out[order(out[,order_by],...),]
  return(out)
}

 #' @rdname riassunto_cat
#' @method riassunto_cat data.table
#' @S3method riassunto_cat data.table

riassunto_cat.data.table <- function(data,
                                     dropNA = F,
                                     freqmod = F,subset=1:ncol(data),order_by = 'ncol',...)
{
  if((subset=='factor')[1]) {
    subset<-sapply(data, function(i) is.factor(i) | is.character(i))
    ncolonne<-which(subset)
  }
  if(is.numeric(subset)) ncolonne<-subset
  if(is.character(subset)) ncolonne<-which(colnames(data)%in%subset)
  subset<-colnames(data)[ncolonne]

  train <-data[, ncolonne,with=FALSE]
 # multifactor(train)
  codifiche<-sapply(train,class)
# NA viene conteggiato come 1 valore
  nlev = sapply(train, function(i) length(unique(i)))
  ossXlev <- round(nrow(train) / nlev, 2)
  percNA <- train[,lapply(.SD,function(i) round(sum(is.na(i))*100 / NROW(i), 2))] %>%
    as.numeric

  entropia<-train[,lapply(.SD,function(i) round(Esame::entropy(i,useNA='ifany'),3))]%>%
    as.numeric()

 # sing = ""
  if (freqmod)
  {
    for(i in 1:ncol(train)){
    print(subset[i])
    cat(Esame::frequenze(train[,subset[i],with=F],cumul = T,reverse = T))
    }
    # train <- Esame::ab.na.gam.replace(train)
    # nlevNA = sapply(train, nlevels)
    # sing <- matrix(0, nrow = sum(nlevNA), ncol = 2)
    # sing[1:nlevNA[1], 1] <- as.matrix(table(train[, 1]))
    # rn <-
    #   paste(colnames(train)[1], levels(train[, 1]), sep = ',')[order(sing[1:nlevNA[1], 1], decreasing =
    #                                                                  T)]
    # sing[1:nlevNA[1], 1] <- sort(sing[1:nlevNA[1], 1], decreasing = T)
    # sing[1:nlevNA[1], 2] <-
    #   sort(cumsum(sing[nlevNA[1]:1, 1]), decreasing = T)
    # id <- cumsum(nlevNA)
    # for (i in 2:ncol(train))
    # {
    #   sing[(id[i - 1] + 1):id[i], 1] <- as.matrix(table(train[, i]))
    #   rn <-
    #     c(rn, paste(colnames(train)[i], levels(train[, i]), sep = ',')[order(sing[(id[i -
    #                                                                                   1] + 1):id[i], 1], decreasing = T)])
    #   sing[(id[i - 1] + 1):id[i], 1] <-
    #     sort(sing[(id[i - 1] + 1):id[i], 1], decreasing = T)
    #   sing[(id[i - 1] + 1):id[i], 2] <-
    #     sort(cumsum(sing[id[i]:(id[i - 1] + 1), 1]), decreasing = T)
    # }
    # rownames(sing) <- rn
    # colnames(sing) <- c('freq', 'cumsum')
  }
  if (!dropNA)
  {
    train <- Esame::ab.na.gam.replace(train)
    entropia_notNA <-train[,lapply(.SD,function(i) round(Esame::entropy(i,useNA='no'),3))]%>%
        as.numeric()

      out <- data.frame(ncol=ncolonne,
                 perc.NA = percNA,
                 nlevels = nlev,
                 #oss*level = ossXlev,
                 entropia=signif(entropia,3),
                 entropia_notNA=signif(entropia_notNA,3)
    )
  }
  else
      out <- data.frame(ncol=ncolonne,
                 perc.NA = percNA,
                 nlevels = nlev,
                 'oss*level' = ossXlev,
                 entropia=signif(entropia,3)
    )
  if(length(subset)==ncol(data)) out <- cbind(out,class=codifiche)
  out <- out[order(out[,order_by],...),]
  return(out)
  }

#' @rdname riassunto_cat
#' @method riassunto_cat defaut
#' @S3method riassunto_cat default

riassunto_cat.default <- function(data,
                                     dropNA = F,
                                     freqmod = F)
{
  data<-as.factor(data)
  nlev = nlevels(data)
  ossXlev <- round(length(data) / nlev, 2)
  percNA <- round((sum(is.na(data)) / length(data))*100, 2)
  varNA <-round(
      var(as.numeric(data[!is.na(data)]) - 1),3)
  sing = NULL
  if (freqmod)
  {
    data <- Esame::ab.na.gam.replace(data)
    nlevNA = nlevels(data)
    sing <- matrix(0, nrow = sum(nlevNA), ncol = 2)
    sing[1:nlevNA[1], 1] <- as.matrix(table(data))
    rn <-
      paste(levels(data), sep = ',')[order(sing[1:nlevNA[1], 1], decreasing =
                                                                     T)]
    sing[1:nlevNA[1], 1] <- sort(sing[1:nlevNA[1], 1], decreasing = T)
    sing[1:nlevNA[1], 2] <-
      sort(cumsum(sing[nlevNA[1]:1, 1]), decreasing = T)
    id <- cumsum(nlevNA)
    rownames(sing) <- rn
    colnames(sing) <- c('freq', 'cumsum')
  }
  else sing=""
  if (!dropNA)
  {
    data <- Esame::ab.na.gam.replace(data)
    varNA <-round(
      var(as.numeric(data[!is.na(data)]) - 1),3)
    return(list(
      sing,
      data.frame(
        perc.NA = percNA,
        nlevels = nlev,
        'oss*level' = ossXlev,
        varNA,
        var
      )
    ))
  }
  else
    return(list(
      sing,
      data.frame(
        perc.NA = percNA,
        nlevels = nlev,
        'oss*level' = ossXlev,
        varNA
      )
    ))
  }

#' Sostituire categorie
#'
#' sostituisce categorie con un'altra
#'
#' @param data data.frame o vettore
#' @param new la new da inserire
#' @param old le categorie da sostituire
#' @export

change_cat <- function(data,
                       old = NULL,
                       new = "other",
                       condition=1)
{
  if(is.null(old)) old<-Esame::search_vuote(data,condition)

  base::ifelse(is.na(old),
    return(Esame:::change_NA(data,new)),
    base::ifelse(is.na(new),
    return(elim_cat(data,old)),
{
    if (NCOL(data)<2)
      data <- change_value_2(data,new,old)
     else
       for(i in 1:NCOL(data)){
         if(is.list(old)) old_tmp <- old[[i]]
         else old_tmp <- old
         if(is.list(new)) new_tmp <- new[[i]]
         else new_tmp <- new

         data[,i] <- change_value_2.factor(data[,i],new_tmp,old_tmp)
    }
    return(data)
    }))

        # for (i in 1:ncol(data)) {
        #   if (is.factor(data[, i]))
        #   {
        #     old <- search_vuote(data[, i], table(data[, i]) < 2)
        #     if (!new %in% levels(data[, i]))
        #     {
        #       data[, i] <-
        #         factor(data[, i], levels = c(levels(data[, i]), new))
        #     }
        #     data[, i][data[, i] %in% old] <- new
        #     data[, i] <- factor(data[, i])
        #   }
        # }

        #     if(length(id)>1)
        # id2 <-
        #   id[!sapply(1:length(id), function(i)
        #     new %in% levels(data[, id[i]]))]
        # else id2<-id[!new %in% levels(data[, id])]
        # if (length(id2) > 0)
        # {
        #   data[, id2] <-
        #     sapply(1:length(id2) , function(i)
        #       factor(data[, id2[i]], levels = c(levels(data[, id2[i]]), new)))
        # }
        # data[, id][sapply(1:length(id), function(i)
        #   data[, id[i]] %in% old[[i]])] <- new
        # if(length(id)>1)
        # data[, id] <- sapply(data[, id], factor, simplify = F)
        # else data[,id]<-factor(data[,id])
}

change_value_2<-function(data,
                       new = "other",
                       old = NULL) UseMethod("change_value_2")

#' @rdname change_value_2
#' @method change_value_2 factor
#' @S3method change_value_2 factor
change_value_2.factor<-function(data,
                              new,
                              old){
    if (!new %in% levels(data))
    {
      data <-
        factor(data, levels = c(levels(data), new))
    }
  if(NROW(new)==1)
  {
    data[data %in% old] <- new
}
else {
  if(NROW(new)!=NROW(old)) stop("new's length not equal to old's length")
  for(i in 1:NROW(new)){
    data[data==old[i]] <- new[i]
  }
}
  data <- factor(data)
  return(data)
}

#' @rdname change_value_2
#' @method change_value_2 numeric
#' @S3method change_value_2 numeric
#' @S3method change_value_2 factor
change_value_2.numeric<-function(data,
                              new ,
                              old){
  if(NROW(new)==1)
  {
    data[data %in% old] <- new
  }
  else {
    if(NROW(new)!=NROW(old)) stop("new's length not equal to old's length")
    for(i in 1:NROW(new)){
      data[data==old[i]] <- new[i]
    }
  }
  return(data)
}

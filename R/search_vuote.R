#' Trovare certe categorie
#'
#' Trova le categorie che soddisfano una determinata espressione, utilizzata per vedere quelle con bassa numerosità campionaria. Se è un data.frame guardare le categorie vuote
#'
#'
#' @param data vector or data.frame.
#' @param condition grandezza massima della categoria
#' @return il vettore dei nomi delle categorie
#' @export

search_vuote <- function(data, condition)
{
	# if (is.data.frame(data)) {
	# 	nomi_col <- colnames(data)[sapply(data, class) == "factor" |
	# 			sapply(sapply(data, class), length) == 2]
	# 	data <-
	# 		data[, sapply(data, class) == "factor" |
	# 				sapply(sapply(data, class), length) == 2]
		if (NCOL(data) < 2)
		{
			cat_vuote <- attr(table(data)[table(data) < condition], "names")
		}
		else {
			cat_vuote <-
				sapply(data, function(i)
					attr(table(i)[table(i) < condition], "names"))
			names(cat_vuote)[lengths(cat_vuote) > 0] <-
				colnames(data)[lengths(cat_vuote) > 0]
			cat_vuote <- cat_vuote[lengths(cat_vuote) > 0]
		}
		# if (is.null(cond))
		# {
		# 	cond <- table(data) < 1
		# 	cat_vuote <- attr(table(data)[cond], "names")
		# }
		# else {
		# 	if (length(cond) == length(data))
		# 		cat_vuote <- unique(data[cond])
		# 	else
		# 		cat_vuote <- names(table(data))[cond]
		# }
	return(cat_vuote)
}

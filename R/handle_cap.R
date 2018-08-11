#' Gestire cap
#'
#' @param data
#' @param state
#'
#' @return
#' @export
#'
#' @examples
handle_cap <- function(data,state = 'italy'){
  cap_inexistent <- c('00000','10000','11111')
  #cleaning data
  data<-gsub("[]$*+.?[^{|(\\#%&~/<=>!,:; \")}@-]", "", data)
  data <- data %>% as.character()
  data[data %>% nchar()!=5] <- NA
  data[data%in%cap_inexistent] <-NA
  data <- as.numeric(data)
  if(state=='italy'){
    macrogroups<- floor(data/10000)
    region <- floor(data/1000)
    capital <-  (data %% 1000)>100
    out <- data.frame(macrogroups,region,capital)
  }
  if(state=='usa'){
    macrogroups<- floor(data/10000)
    region <- (data%%10000/100)%>% floor
    out <- data.frame(macrogroups,region)
  }
  return(out)
}

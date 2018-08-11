#' estrarre categorie
#'
#' @param data vettore
#' @param nstring vettore se si deve estrarre una sola variabile, lista per estrarre più variabili.
#'
#' @return
#' @export
estrai_cat<-function(data,nstring,label=NULL)
{
  if(!is.list(nstring))
  {
      if(length(nstring)==1)
  x<-substr(data,nstring,nstring)
  if(length(nstring)==2)
  x<-substr(data,nstring[1],nstring[2])
  }

else
  {
    x<-matrix(0,length(data),length(nstring))
for (i in 1:length(nstring))
{
  if(length(nstring[[i]])==1)
  x[,i]<-substr(data,nstring[[i]],nstring[[i]])
  if(length(nstring[[i]])==2)
  x[,i]<-substr(data,nstring[[i]][1],nstring[[i]][2])
}
  x<-as.data.frame(x)
  if(length(label)!=length(nstring))
  colnames(x)<-paste(label,1:ncol(x),sep='')
  else colnames(x) <- label
  }
  return(x)
}

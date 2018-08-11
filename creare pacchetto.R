#Ctrl+1: Set working directory to current document's directory
create('Esame')

library(devtools)
library(roxygen2)
#setwd("./Esame")
document()
setwd("..")
install('Esame')
######## aspettare
?join.liftroc
?freq

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/notify.R
\name{notify}
\alias{notify}
\title{Title}
\usage{
notify(msg = "Operation complete")
}
\arguments{
\item{msg}{}
}
\description{
Title
}
\examples{
\dontrun{
# try it!
library(source.gist) # install.packages("source.gist")
source.gist(6545433)
system("sleep 10")
notify("Long op complete")
library(beepr)
beep(3)
}
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/search_NA.R
\name{search_NA}
\alias{search_NA}
\title{Ricerca NA}
\usage{
search_NA(data, exclude = NULL)
}
\arguments{
\item{data}{data.frame}

\item{exclude}{numeric. Esclude le colonne corrispondenti. Viene usato se certe colonne sono trasformazione deterministica di altre.}
}
\value{
a list
\item{comb}{vettore: ogni elemento dice quali variabili mancano a ogni riga con NA}
\item{riga}{ in base al numero di modalita' di comb, crea una lista dove il primo elemento ha gli indice di riga della 1 modalita', il secondo della seconda modalita' ecc.}
\item{col}{stessa cosa precedente, ma ha gli indici di colonna.}
}
\description{
Ricerca gli NA per l'imputazine successiva
}

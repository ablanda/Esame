% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/knn_crossvalidation.R
\name{knn_crossvalidation}
\alias{knn_crossvalidation}
\title{knn cv}
\usage{
knn_crossvalidation(i, K = 1, y, x, folds, package = "FNN")
}
\arguments{
\item{i}{i-esimo fold}

\item{K}{number of neighbours considered.}

\item{y}{vettore. variabile risposta}

\item{x}{matrice variabili esplicative}

\item{folds}{prodotto da kfold function}
}
\description{
fonisce le previsioni della cv, da utilizzare dentro sapply(1:k,...)
}

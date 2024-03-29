% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/analysis_selecting.R
\name{select_by_length}
\alias{select_by_length}
\title{Selects documents from a collection based on their length}
\usage{
select_by_length(coll, ids = NULL, min = 0, max = 1e+06, unit = "tokens")
}
\arguments{
\item{coll}{A collection object}

\item{ids}{A vector of document ids to select from. If NULL, all documents are selected.}

\item{min}{Minimum length of the document in tokens or characters. Default is 0.}

\item{max}{Maximum length of the document in tokens or characters. Default is 1000000.}

\item{unit}{Unit of length. Either "tokens" (default) or "char".}
}
\value{
A vector of document ids that meet the length criteria.
}
\description{
Selects documents from a collection based on their length
}

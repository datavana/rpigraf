% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fetch.R
\name{fetch_table}
\alias{fetch_table}
\title{Fetch tables such as articles, projects or properties}
\usage{
fetch_table(table, columns = c(), params = c(), db = NA, maxpages = 1)
}
\arguments{
\item{table}{The table name (e.g. "articles")}

\item{columns}{A vector of column names.}

\item{params}{A named list of query params}

\item{db}{The database name}

\item{maxpages}{Maximum number of pages to request.
Set to 1 for non-paginated tables.}
}
\description{
Returns a row with defined columns for each record matched by the params.
The procedure corresponds to calling the index action in the Epigraf interface.
}

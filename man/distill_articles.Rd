% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/distill.R
\name{distill_articles}
\alias{distill_articles}
\title{Get articles (including selected item values)}
\usage{
distill_articles(
  df,
  cols = c(),
  item.type = NULL,
  item.cols = c(),
  property.cols = c()
)
}
\arguments{
\item{df}{A RAM data frame}

\item{cols}{Article columns}

\item{item.type}{Item types to join}

\item{item.cols}{Cols to join from the items}

\item{property.cols}{Cols to join from the property}
}
\value{
A tibble with articles
}
\description{
Get articles (including selected item values)
}

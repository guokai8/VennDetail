% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data-extraction.R
\name{add_prefix}
\alias{add_prefix}
\title{Add prefix to column names}
\usage{
add_prefix(x, prefix, sep = "_")
}
\arguments{
\item{x}{A data.frame}

\item{prefix}{Prefix to add}

\item{sep}{Separator between prefix and original column name}
}
\value{
A data.frame with prefixed column names
}
\description{
Adds a prefix to column names except for "RowNxyz"
}
\author{
Kai Guo
}
\keyword{internal}

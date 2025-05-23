% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{$.Venn}
\alias{$.Venn}
\title{Extract a column from a Venn object}
\usage{
\method{$}{Venn}(x, name)
}
\arguments{
\item{x}{A Venn object}

\item{name}{Column name}
}
\value{
A vector with the column values
}
\description{
Extract a column from the result data frame
}
\examples{
# Create a Venn object
A <- sample(1:100, 40, replace = FALSE)
B <- sample(1:100, 60, replace = FALSE)
res <- venndetail(list(A = A, B = B))

# Extract the Subset column
res$Subset
}
\author{
Kai Guo
}

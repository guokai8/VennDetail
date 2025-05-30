% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{[.Venn}
\alias{[.Venn}
\title{Subset a Venn object}
\usage{
\method{[}{Venn}(x, i, j)
}
\arguments{
\item{x}{A Venn object}

\item{i}{Row indices}

\item{j}{Column indices or names}
}
\value{
A subset of the result data frame
}
\description{
Extract rows and/or columns from the result data frame
}
\examples{
# Create a Venn object
A <- sample(1:100, 40, replace = FALSE)
B <- sample(1:100, 60, replace = FALSE)
res <- venndetail(list(A = A, B = B))

# Extract the first 5 rows
res[1:5, ]

# Extract a specific column
res[, "Subset"]
}
\author{
Kai Guo
}

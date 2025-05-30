% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statistical.R
\name{vennStats,Venn-method}
\alias{vennStats,Venn-method}
\title{Perform statistical tests on set intersections}
\usage{
\S4method{vennStats}{Venn}(
  object,
  universe = NULL,
  method = c("hypergeometric", "permutation"),
  nperm = 1000,
  adjust.method = "BH",
  include_singles = FALSE
)
}
\arguments{
\item{object}{A Venn object}

\item{universe}{Size of the universe for hypergeometric test (default: NULL, will use the union of all sets)}

\item{method}{Statistical method to use: "hypergeometric" or "permutation" (default: "hypergeometric")}

\item{nperm}{Number of permutations if method="permutation" (default: 1000)}

\item{adjust.method}{Method for multiple testing correction (default: "BH")}

\item{include_singles}{Logical: include tests for single sets? (default: FALSE)}
}
\value{
A data.frame with statistical test results
}
\description{
Performs statistical tests to evaluate the significance of set intersections
}
\examples{
# Create a Venn object
A <- sample(1:1000, 100, replace = FALSE)
B <- sample(1:1000, 150, replace = FALSE)
C <- sample(1:1000, 120, replace = FALSE)
res <- venndetail(list(A = A, B = B, C = C))

# Perform statistical tests
stats <- vennStats(res)

# With custom universe size
stats <- vennStats(res, universe = 2000)

# Using permutation test
stats <- vennStats(res, method = "permutation", nperm = 500)
}
\author{
Kai Guo
}

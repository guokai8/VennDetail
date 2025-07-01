#' @title Return summary information about a Venn object
#' @description Summary method for Venn objects
#' @param object A Venn object
#' @param ... Additional arguments (not used)
#' @return Invisibly returns the Venn object
#' @method summary Venn
#' @author Kai Guo
#' @export
#' @examples
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#' res <- venndetail(list(A = A, B = B, C = C))
#' summary(res)
summary.Venn <- function(object, ...) {
  cat("Venn Diagram Summary\n")
  cat("---------------------\n")
  cat("Input groups:", paste(object@GroupNames, collapse = ", "), "\n")
  cat("Group sizes:", paste(paste(object@GroupNames, "=", object@raw), collapse = ", "), "\n")
  cat("Total unique elements:", sum(object@detail), "\n")
  cat("Total subsets:", length(object@detail), "\n")

  cat("\nSubset details:\n")

  # Create a nicely formatted table
  subset_table <- data.frame(
    Subset = names(object@detail),
    Size = as.numeric(object@detail),
    stringsAsFactors = FALSE
  )

  # Sort by size
  subset_table <- subset_table[order(subset_table$Size, decreasing = TRUE), ]

  # Print the table
  print(subset_table, row.names = FALSE)

  invisible(object)
}

#' @title Show method for Venn objects
#' @description Display method for Venn objects
#' @param object A Venn object
#' @return Invisibly returns the Venn object
#' @author Kai Guo
#' @export
setMethod("show", signature = (object="Venn"), function(object) {
  cat("=== Venn Diagram Details ===\n")
  cat("Input groups:", paste(object@GroupNames, collapse = ", "), "\n")
  cat("Total elements:", sum(object@detail), "\n")
  cat("Total subsets:", length(object@detail), "\n")

  cat("\nTop subsets by size:\n")
  # Display the top 6 subsets by size
  top_subsets <- names(sort(object@detail, decreasing = TRUE)[1:min(6, length(object@detail))])
  subset_sizes <- object@detail[top_subsets]

  for (i in seq_along(top_subsets)) {
    cat(sprintf("  %s: %d\n", top_subsets[i], subset_sizes[i]))
  }

  if (length(object@detail) > 6) {
    cat("... and", length(object@detail) - 6, "more subsets\n")
  }

  cat("\nSample of results:\n")
  print(head(object@result), row.names = FALSE)

  if (nrow(object@result) > 6) {
    cat("... with", nrow(object@result) - 6, "more rows\n")
  }

  invisible(object)
})

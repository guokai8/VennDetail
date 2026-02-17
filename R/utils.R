#' @title Utility functions for VennDetail package
#' @description Internal utility functions for the VennDetail package

#' @title Convert a Venn object to a data frame
#' @description Converts a Venn object to a data frame for easier manipulation
#' @param x A Venn object
#' @param ... Additional arguments (not used)
#' @return A data frame with subset information
#' @method as.data.frame Venn
#' @author Kai Guo
#' @export
#' @examples
#' # Create a Venn object
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' res <- venndetail(list(A = A, B = B))
#'
#' # Convert to data frame
#' df <- as.data.frame(res)
#' head(df)
as.data.frame.Venn <- function(x, ...) {
  return(x@result)
}

#' @title Extract subset names from a Venn object
#' @description Returns the names of all subsets in a Venn object
#' @param x A Venn object
#' @return A character vector of subset names
#' @method names Venn
#' @author Kai Guo
#' @export
#' @examples
#' # Create a Venn object
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' res <- venndetail(list(A = A, B = B))
#'
#' # Get subset names
#' names(res)
names.Venn <- function(x) {
  return(names(x@detail))
}

#' @title Save a Venn object to a file
#' @description Saves a Venn object to an RDS file for later use
#' @param object A Venn object
#' @param file File name to save to
#' @return The file name (invisibly)
#' @author Kai Guo
#' @export
#' @examples
#' \dontrun{
#' # Create a Venn object
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' res <- venndetail(list(A = A, B = B))
#'
#' # Save to a file
#' saveVenn(res, "my_venn.rds")
#' }
saveVenn <- function(object, file) {
  # Validate input
  if (!is(object, "Venn")) {
    stop("object must be a Venn object")
  }

  # Save the object
  saveRDS(object, file)

  # Return the file name invisibly
  invisible(file)
}

#' @title Load a Venn object from a file
#' @description Loads a Venn object from an RDS file
#' @param file File name to load from
#' @return A Venn object
#' @author Kai Guo
#' @export
#' @examples
#' \dontrun{
#' # Load a saved Venn object
#' res <- loadVenn("my_venn.rds")
#'
#' # Plot the loaded object
#' plot(res)
#' }
loadVenn <- function(file) {
  # Load the object
  object <- readRDS(file)

  # Validate that it's a Venn object
  if (!is(object, "Venn")) {
    stop("File does not contain a Venn object")
  }

  return(object)
}

#' @title Compare two Venn objects
#' @description Compares two Venn objects and returns a list of differences
#' @param x First Venn object
#' @param y Second Venn object
#' @param what What to compare: "groups" (default), "subsets", or "all"
#' @return A list with differences between the objects
#' @author Kai Guo
#' @export
#' @examples
#' # Create two Venn objects
#' A1 <- sample(1:100, 40, replace = FALSE)
#' B1 <- sample(1:100, 60, replace = FALSE)
#' res1 <- venndetail(list(A = A1, B = B1))
#'
#' A2 <- sample(1:100, 45, replace = FALSE)
#' B2 <- sample(1:100, 55, replace = FALSE)
#' res2 <- venndetail(list(A = A2, B = B2))
#'
#' # Compare the objects
#' compareVenn(res1, res2)
compareVenn <- function(x, y, what = c("groups", "subsets", "all")) {
  # Validate inputs
  if (!is(x, "Venn") || !is(y, "Venn")) {
    stop("x and y must be Venn objects")
  }

  # Get comparison type
  what <- match.arg(what)

  # Initialize results
  result <- list()

  # Compare groups
  if (what %in% c("groups", "all")) {
    result$groups <- list(
      x_only = setdiff(x@GroupNames, y@GroupNames),
      y_only = setdiff(y@GroupNames, x@GroupNames),
      both = intersect(x@GroupNames, y@GroupNames),
      x_size = x@raw,
      y_size = y@raw
    )
  }

  # Compare subsets
  if (what %in% c("subsets", "all")) {
    x_subsets <- names(x@detail)
    y_subsets <- names(y@detail)

    result$subsets <- list(
      x_only = setdiff(x_subsets, y_subsets),
      y_only = setdiff(y_subsets, x_subsets),
      both = intersect(x_subsets, y_subsets)
    )

    # Compare sizes of common subsets
    common_subsets <- result$subsets$both
    if (length(common_subsets) > 0) {
      x_sizes <- x@detail[common_subsets]
      y_sizes <- y@detail[common_subsets]

      # Calculate differences
      size_diff <- x_sizes - y_sizes

      result$subset_sizes <- data.frame(
        Subset = common_subsets,
        X_Size = x_sizes,
        Y_Size = y_sizes,
        Difference = size_diff,
        stringsAsFactors = FALSE
      )
    }
  }

  # Compare all elements if requested
  if (what == "all") {
    x_elements <- unique(x@result$Detail)
    y_elements <- unique(y@result$Detail)

    result$elements <- list(
      x_only = setdiff(x_elements, y_elements),
      y_only = setdiff(y_elements, x_elements),
      both = intersect(x_elements, y_elements),
      x_count = length(x_elements),
      y_count = length(y_elements),
      intersection_count = length(intersect(x_elements, y_elements))
    )
  }

  return(result)
}

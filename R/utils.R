#' @title Utility functions for VennDetail package
#' @description Internal utility functions for the VennDetail package

#' @title Merge two or more Venn objects
#' @description Combines multiple Venn objects into a single Venn object
#' @param x First Venn object
#' @param y Second Venn object
#' @param ignore.case Logical: ignore case in group names? Default: FALSE
#' @param useupper Logical: convert all group names to upper case? Default: TRUE
#' @param plot Logical: plot the combined result? Default: FALSE
#' @param ... Additional arguments passed to venndetail
#' @return A new Venn object combining the input objects
#' @importFrom purrr flatten
#' @importFrom methods slot
#' @method merge Venn
#' @author Kai Guo
#' @export
#' @examples
#' # Create two Venn objects
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#' res1 <- venndetail(list(A = A, B = B))
#' res2 <- venndetail(list(A = A, C = C))
#'
#' # Merge the two objects
#' res <- merge(res1, res2)
#'
#' # Plot the merged result
#' plot(res)
merge.Venn <- function(x, y, ignore.case = FALSE, useupper = TRUE,
                       plot = FALSE, ...) {
  # Combine the input objects into a list
  objects <- list(x, y)

  # Extract input slots from all objects
  inputs <- lapply(objects, function(obj) slot(obj, "input"))
  inputs <- flatten(inputs)

  # Get group names
  group_names <- names(inputs)

  # Handle case sensitivity if requested
  if (isTRUE(ignore.case)) {
    if (isTRUE(useupper)) {
      # Convert to upper case
      upper_names <- toupper(group_names)
      names(inputs) <- upper_names
      group_names <- upper_names
    } else {
      # Convert to lower case
      lower_names <- tolower(group_names)
      names(inputs) <- lower_names
      group_names <- lower_names
    }
  }

  # Keep only unique group names
  unique_names <- unique(group_names)
  inputs <- inputs[unique_names]

  # Create a new Venn object
  result <- venndetail(inputs, ...)

  # Plot if requested
  if (isTRUE(plot)) {
    plot(result)
  }

  return(result)
}

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

#' @title Extract the first few rows of a Venn object
#' @description Extract the first n rows of the result data frame
#' @param x A Venn object
#' @param n Number of rows to extract
#' @param ... Additional arguments passed to head
#' @return A data frame with the first n rows
#' @method head Venn
#' @importFrom utils head
#' @author Kai Guo
#' @export
#' @examples
#' # Create a Venn object
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' res <- venndetail(list(A = A, B = B))
#'
#' # Get the first few rows
#' head(res)
head.Venn <- function(x, n = 6L, ...) {
  head(x@result, n, ...)
}

#' @title Extract the last few rows of a Venn object
#' @description Extract the last n rows of the result data frame
#' @param x A Venn object
#' @param n Number of rows to extract
#' @param ... Additional arguments passed to tail
#' @return A data frame with the last n rows
#' @method tail Venn
#' @importFrom utils tail
#' @author Kai Guo
#' @export
#' @examples
#' # Create a Venn object
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' res <- venndetail(list(A = A, B = B))
#'
#' # Get the last few rows
#' tail(res)
tail.Venn <- function(x, n = 6L, ...) {
  tail(x@result, n, ...)
}

#' @title Get the dimensions of a Venn object
#' @description Returns the dimensions of the result data frame
#' @param x A Venn object
#' @return A numeric vector with the number of rows and columns
#' @method dim Venn
#' @author Kai Guo
#' @export
#' @examples
#' # Create a Venn object
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' res <- venndetail(list(A = A, B = B))
#'
#' # Get dimensions
#' dim(res)
dim.Venn <- function(x) {
  dim(x@result)
}

#' @title Subset a Venn object
#' @description Extract rows and/or columns from the result data frame
#' @param x A Venn object
#' @param i Row indices
#' @param j Column indices or names
#' @return A subset of the result data frame
#' @method [ Venn
#' @author Kai Guo
#' @export
#' @examples
#' # Create a Venn object
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' res <- venndetail(list(A = A, B = B))
#'
#' # Extract the first 5 rows
#' res[1:5, ]
#'
#' # Extract a specific column
#' res[, "Subset"]
`[.Venn` <- function(x, i, j) {
  x@result[i, j]
}

#' @title Extract a column from a Venn object
#' @description Extract a column from the result data frame
#' @param x A Venn object
#' @param name Column name
#' @return A vector with the column values
#' @method $ Venn
#' @author Kai Guo
#' @export
#' @examples
#' # Create a Venn object
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' res <- venndetail(list(A = A, B = B))
#'
#' # Extract the Subset column
#' res$Subset
`$.Venn` <- function(x, name) {
  x@result[, name]
}

#' @title Save a Venn object to a file
#' @description Saves a Venn object to an RDS file for later use
#' @param object A Venn object
#' @param file File name to save to
#' @return The file name (invisibly)
#' @author Kai Guo
#' @export
#' @examples
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' res <- venndetail(list(A = A, B = B))
#' tmp <- tempfile(fileext = ".rds")
#' saveVenn(res, tmp)
#' file.exists(tmp)
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
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' res <- venndetail(list(A = A, B = B))
#' tmp <- tempfile(fileext = ".rds")
#' saveVenn(res, tmp)
#' res2 <- loadVenn(tmp)
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


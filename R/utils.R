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

#' @title Set color palette for Venn diagrams
#' @description Returns a color palette for Venn diagrams
#' @param n Number of colors needed
#' @param palette Type of palette: "default", "categorical", "sequential", or "diverging"
#' @return A vector of colors
#' @importFrom grDevices colors
#' @author Kai Guo
#' @export
#' @examples
#' # Get 5 colors from the default palette
#' cols <- setcolor(5)
#'
#' # Get 10 colors from the categorical palette
#' cols <- setcolor(10, palette = "categorical")
setcolor <- function(n, palette = "default") {
  if (palette == "default") {
    # Default colorful palette
    default_colors <- c(
      "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
      "#1B9E77", "brown", "#7570B3", "#E7298A", "#7FC97F", "#A6761D",
      "#BEAED4", "#FDC086", "chartreuse1", "cyan3", "purple", "pink4",
      "cyan", "royalblue", "violet", "springgreen2", "gold3",
      "darkseagreen4", "#E5D8BD", "#E64B35", "#4DBBD5", "#00A087",
      "#3C5488", "#F39B7F", "#8491B4", "#91D1C2", "#DC0000",
      "#7E6148", "#B09C85", "#008B45", "#631879", "#008280", "#BB0021",
      "#5F559B", "#A20056", "#808180", "#1B1919", "#374E55", "#DF8F44",
      "#00A1D5", "#B24745", "#79AF97", "#6A6599", "#80796B"
    )

    if (n <= length(default_colors)) {
      return(default_colors[1:n])
    } else {
      # Generate additional random colors if needed
      return(c(default_colors, sample(colors(), n - length(default_colors), replace = FALSE)))
    }
  } else if (palette == "categorical") {
    # Categorical palette (more distinct colors)
    categorical_colors <- c(
      "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33",
      "#A65628", "#F781BF", "#999999", "#66C2A5", "#FC8D62", "#8DA0CB",
      "#E78AC3", "#A6D854", "#FFD92F", "#E5C494", "#B3B3B3", "#8DD3C7",
      "#FFFFB3", "#BEBADA", "#FB8072", "#80B1D3", "#FDB462", "#B3DE69",
      "#FCCDE5", "#D9D9D9", "#BC80BD", "#CCEBC5", "#FFED6F"
    )

    if (n <= length(categorical_colors)) {
      return(categorical_colors[1:n])
    } else {
      # Generate additional random colors if needed
      return(c(categorical_colors, sample(colors(), n - length(categorical_colors), replace = FALSE)))
    }
  } else if (palette == "sequential") {
    # Sequential blue palette
    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      return(RColorBrewer::brewer.pal(min(9, n), "Blues")[1:min(9, n)])
    } else {
      # Fallback if RColorBrewer is not available
      sequential_blues <- c(
        "#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6",
        "#4292C6", "#2171B5", "#08519C", "#08306B"
      )

      if (n <= length(sequential_blues)) {
        return(sequential_blues[1:n])
      } else {
        # Repeat colors with different transparency
        base_colors <- sequential_blues
        return(rep(base_colors, length.out = n))
      }
    }
  } else if (palette == "diverging") {
    # Diverging red-blue palette
    if (requireNamespace("RColorBrewer", quietly = TRUE)) {
      return(RColorBrewer::brewer.pal(min(11, n), "RdBu")[1:min(11, n)])
    } else {
      # Fallback if RColorBrewer is not available
      diverging_colors <- c(
        "#67001F", "#B2182B", "#D6604D", "#F4A582", "#FDDBC7",
        "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061"
      )

      if (n <= length(diverging_colors)) {
        return(diverging_colors[1:n])
      } else {
        # Repeat colors with different transparency
        base_colors <- diverging_colors
        return(rep(base_colors, length.out = n))
      }
    }
  } else {
    stop("Unknown palette: ", palette)
  }
}

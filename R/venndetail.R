#' @title Create a Venn object for set analysis
#' @description Extracts shared and unique elements from multiple sets and creates
#'   a Venn object for analysis and visualization
#' @param x A list of vectors with group names
#' @param sep Symbol character used when concatenating group names into subset names
#'   (default: '_')
#' @param abbr Logical: abbreviate subset names? Default: FALSE
#' @param minlength Minimal length for abbreviated subset names. Default: 3
#' @param abbr.method Method for abbreviation: "both.sides", "left.sides", or "right.sides"
#' @param verbose Logical: show progress messages? Default: FALSE
#' @return A Venn object
#' @importFrom methods new validObject
#' @importFrom utils packageVersion
#' @author Kai Guo
#' @export
#' @examples
#' # Create a Venn object with three sets
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#' res <- venndetail(list(A = A, B = B, C = C))
#'
#' # Examine the results
#' summary(res)
#'
#' # Plot the results
#' plot(res, type = "venn")
#'
#' # With abbreviated set names
#' sets <- list(
#'   LongNameGroup1 = sample(1:100, 40),
#'   LongNameGroup2 = sample(1:100, 50),
#'   LongNameGroup3 = sample(1:100, 45)
#' )
#' res <- venndetail(sets, abbr = TRUE, minlength = 4)
venndetail <- function(x, sep = "_", abbr = FALSE, minlength = 3,
                       abbr.method = "both.sides", verbose = FALSE) {
  # Validate input
  if (!is.list(x)) {
    stop("Input must be a list of vectors")
  }

  # Set group names if not provided
  if (is.null(names(x))) {
    names(x) <- paste("Group", seq_along(x), sep = "")
  }

  # Abbreviate group names if requested
  if (isTRUE(abbr)) {
    names(x) <- abbreviate(names(x), minlength = minlength,
                           method = abbr.method)
  }

  # Store group names and sizes
  GroupNames <- names(x)
  raw <- vapply(x, length, FUN.VALUE = numeric(1))
  names(raw) <- GroupNames

  # Check for single group
  if (length(x) == 1) {
    message("Only one group found. A Venn object requires at least two groups.")
    return(NULL)
  }

  if (verbose) {
    message("Identifying subsets...")
  }

  # Create subsets
  out <- make_subset(x, sep = sep)

  # Count elements in each subset
  detail <- vapply(out, length, FUN.VALUE = numeric(1))

  # Create long-form result data frame
  res <- data.frame(
    Subset = rep(names(out), as.numeric(detail)),
    Detail = unlist(out),
    stringsAsFactors = FALSE
  )
  rownames(res) <- NULL

  # Create wide-form result data frame
  if (verbose) {
    message("Creating wide-format result...")
  }

  inp <- unique(unlist(x))
  wide <- data.frame(Detail = inp, stringsAsFactors = FALSE)

  # For each input group, add a column indicating presence/absence
  for (g in GroupNames) {
    wide[[g]] <- as.integer(inp %in% x[[g]])
  }

  # Calculate how many sets each element belongs to
  wide$SharedSets <- rowSums(wide[, 2:(length(GroupNames) + 1)])

  # Sort by number of shared sets
  wide <- wide[order(wide$SharedSets, decreasing = TRUE), ]

  # Create metadata
  metadata <- list(
    created = Sys.time(),
    version = packageVersion("VennDetail"),
    n_sets = length(x),
    n_elements = length(unique(unlist(x)))
  )

  # Create Venn object
  if (verbose) {
    message("Creating Venn object...")
  }

  result <- new("Venn",
                input = x,
                raw = raw,
                sep = sep,
                GroupNames = GroupNames,
                result = res,
                detail = detail,
                wide = wide,
                metadata = metadata)

  # Validate the object
  validObject(result)

  return(result)
}

#' @title Create subsets from a list of sets
#' @description Identifies all possible intersections between sets and returns
#'   a list of subsets
#' @param x A list of vectors
#' @param sep Character used to separate set names in subset labels
#' @return A named list where each element contains the unique items in that subset
#' @importFrom stats setNames
#' @author Kai Guo
#' @export
#' @examples
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#' x <- list(A = A, B = B, C = C)
#' subsets <- make_subset(x)
#' lengths(subsets)  # Number of elements in each subset
make_subset <- function(x, sep = "_") {
  if (length(x) == 1) {
    message("Only one group found!")
    return(NULL)
  }

  # Create a truth table of all possible combinations
  truth_table <- make_truth_table(x)

  # Create subset names by combining the names of included sets
  subset_names <- apply(truth_table, 1, function(row) {
    paste(colnames(truth_table)[as.logical(row)], collapse = sep)
  })

  # Identify elements in each subset
  subsets <- apply(truth_table, 1, function(row) {
    included_sets <- as.logical(row)
    excluded_sets <- !included_sets

    # Elements in the intersection of included sets
    if (sum(included_sets) > 0) {
      included_elements <- Reduce(intersect, x[included_sets])
    } else {
      included_elements <- character(0)
    }

    # Elements in any of the excluded sets
    if (sum(excluded_sets) > 0) {
      excluded_elements <- Reduce(union, x[excluded_sets])
    } else {
      excluded_elements <- character(0)
    }

    # Subtract excluded elements from included elements
    setdiff(included_elements, excluded_elements)
  })

  # Name the subsets
  result <- setNames(subsets, subset_names)

  # Rename the subset for elements shared by all groups to "Shared"
  all_groups_idx <- which(subset_names == paste(names(x), collapse = sep))
  if (length(all_groups_idx) == 1) {
    names(result)[all_groups_idx] <- "Shared"
  }

  return(result)
}

#' @title Create a truth table for set combinations
#' @description Creates a logical matrix representing all possible combinations of sets
#' @param x A list of vectors
#' @return A data frame with logical columns for each set and rows for each combination
#' @importFrom stats setNames
#' @author Kai Guo
#' @keywords internal
make_truth_table <- function(x) {
  # Create a list of logical vectors (TRUE, FALSE) for each set
  combinations <- lapply(seq_along(names(x)), function(i) c(TRUE, FALSE))

  # Create all combinations using expand.grid
  truth_table <- setNames(do.call(expand.grid, combinations), names(x))

  # Remove the row where all values are FALSE (represents the empty set)
  truth_table <- truth_table[apply(truth_table, 1, any), ]

  return(truth_table)
}

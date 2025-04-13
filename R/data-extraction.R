#' @title Extract specific subsets from a Venn object
#' @description Extracts elements from specified subsets
#' @param object A Venn object
#' @param subset Character vector of subset names to extract
#' @param min Minimum number of sets an element must be in
#' @param wide Logical: return results in wide format?
#' @return A data.frame with elements from the specified subsets
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @importFrom methods is slot
#' @export
#' @examples
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#' res <- venndetail(list(A = A, B = B, C = C))
#' # Get elements unique to set A
#' unique_to_A <- getSet(res, "A")
#' # Get elements shared by all sets
#' shared <- getSet(res, "Shared")
setMethod("getSet", signature = (object="Venn"), function(object, subset = NULL,
                                                          min = 0, wide = FALSE) {
  # Get the results in wide format
  res <- result(object, wide = TRUE)

  # Get the long-format results
  dd <- object@result

  # Filter by minimum set membership
  rhs <- res %>% filter(SharedSets >= min)

  # Use all subsets if none specified
  if (is.null(subset)) {
    subset <- names(detail(object))
  }

  # Filter by subset names
  lhs <- dd %>% filter(Subset %in% subset)

  # Return in requested format
  if (isTRUE(wide)) {
    out <- rhs %>% filter(Detail %in% lhs$Detail)
  } else {
    out <- lhs %>% filter(Detail %in% rhs$Detail)
  }

  return(out)
})

#' @title Extract feature data for specific subsets
#' @description Combines subset information with user-supplied data frames
#' @param object A Venn object
#' @param subset Character vector of subset names to extract
#' @param rlist List of user-supplied data frames
#' @param userowname Logical: use row names for joining?
#' @param gind Column names or indices to use for joining if userowname=FALSE
#' @param sep Character used to separate group names in column names
#' @param wide Logical: return results in wide format?
#' @return A data.frame combining subset information with user data
#' @importFrom dplyr filter left_join
#' @importFrom tibble rownames_to_column
#' @importFrom magrittr %>%
#' @export
#' @examples
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#' dA <- data.frame(A = A, "FC" = rnorm(40))
#' dB <- data.frame(B = B, "FC" = rnorm(60))
#' dC <- data.frame(C = C, "FC" = rnorm(40))
#' res <- venndetail(list(A = A, B = B, C = C))
#' features <- getFeature(res, subset = "Shared",
#'                       rlist = list(dA, dB, dC),
#'                       userowname = FALSE,
#'                       gind = rep(1, 3))
setMethod("getFeature", signature = (object="Venn"), function(object, subset,
                                                              rlist, userowname = TRUE,
                                                              gind = NULL, sep = "_",
                                                              wide = FALSE) {
  # Get the result data frame
  dd <- object@result

  # Get the wide-format data frame
  wd <- object@wide
  wd$Detail <- as.character(wd$Detail)

  # If no subset specified, use all subsets
  if (missing(subset)) {
    subset <- unique(dd$Subset)
  }

  # Filter by subset names
  lhs <- dd %>% filter(Subset %in% subset)
  lhs$Detail <- as.character(lhs$Detail)

  # Convert to wide format if requested
  if (isTRUE(wide)) {
    lhs <- wd %>% filter(Detail %in% lhs$Detail)
  }

  # Process data frames for joining
  if (!userowname) {
    # Use column indices or names for joining
    if (is.null(gind)) {
      gind <- rep(1, length(rlist))
    }

    if (length(gind) < length(rlist)) {
      stop("Please specify column indices or names for each data frame")
    }

    # Set row names based on specified columns
    rlist <- mapply(set_rownames, rlist, gind, SIMPLIFY = FALSE)
  }

  # Add names to rlist if not present
  if (is.null(names(rlist))) {
    names(rlist) <- paste("Group", seq_along(rlist), sep = "")
  }

  # Add prefix to column names
  name <- names(rlist)
  rlist <- mapply(add_prefix, rlist, name, sep = sep, SIMPLIFY = FALSE)

  # Add row names column to each data frame
  rlist <- lapply(rlist, add_rownames_column)

  # Join all data frames
  rr <- Reduce(function(x, y) rowjoin(x, y, fun = "full_join"), rlist)
  rr$RowNxyz <- as.character(rr$RowNxyz)

  # Join with subset data
  rhs <- left_join(lhs, rr, by = c("Detail" = "RowNxyz"))

  return(rhs)
})

#' @title Join data.frames by row names or specified columns
#' @description Joins two data.frames using various join methods
#' @param x First data.frame
#' @param y Second data.frame
#' @param fun Join function to use: "full_join", "left_join", "right_join", etc.
#' @param by Optional vector of column names to join by
#' @return A joined data.frame
#' @importFrom dplyr full_join left_join right_join anti_join semi_join
#' @export
#' @examples
#' library(dplyr)
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' dA <- data.frame(A = A, "FC" = rnorm(40))
#' dB <- data.frame(B = B, "FC" = rnorm(60))
#' rownames(dA) <- A
#' rownames(dB) <- B
#' # Full join by row names
#' result <- rowjoin(dA, dB)
#' # Left join by row names
#' result <- rowjoin(dA, dB, fun = "left_join")
setMethod("rowjoin", signature(x = "data.frame", y = "data.frame"),
          function(x, y, fun = "full_join", by = NULL) {
            # Add row names column if not present
            x <- add_rownames_column(x)
            y <- add_rownames_column(y)

            # Get the join function
            join_fn <- match.fun(fun)

            # Default join by row names column
            if (is.null(by)) {
              return(join_fn(x, y, by = c("RowNxyz" = "RowNxyz")))
            } else {
              return(join_fn(x, y, by = by))
            }
          })

#' @title Create a bar plot of subset counts
#' @description Creates a bar plot showing counts for each subset
#' @param object A Venn object
#' @param order Logical: should bars be ordered by count?
#' @param textsize Numeric: size of text labels above bars
#' @param color Optional vector of colors for the bars
#' @param theme The ggplot2 theme to use
#' @param title Optional plot title
#' @param xlabel Optional x-axis label
#' @param ylabel Optional y-axis label
#' @return A ggplot2 object
#' @importFrom methods is slot
#' @importFrom ggplot2 ggplot geom_bar aes scale_fill_manual theme labs
#' @importFrom ggplot2 geom_text ylim ggtitle xlab ylab
#' @importFrom grDevices colorRampPalette
#' @author Kai Guo
#' @export
#' @examples
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#' res <- venndetail(list(A = A, B = B, C = C))
#' # Simple bar plot
#' dplot(res)
#' # Ordered bars with custom title
#' dplot(res, order = TRUE, textsize = 3, title = "Set Intersections")
setMethod("dplot", signature = (object="Venn"), function(object, order = FALSE,
                                                         textsize = 5, color = NULL,
                                                         theme = theme_light(),
                                                         title = NULL, xlabel = NULL,
                                                         ylabel = NULL) {
  # Create a data frame with subset names and counts
  df <- data.frame(
    Group = names(object@detail),
    Detail = object@detail,
    stringsAsFactors = FALSE
  )

  # Generate colors if not provided
  if (is.null(color)) {
    color <- setcolor(length(object@detail))
  }
  names(color) <- names(object@detail)

  # Order bars by count if requested
  if (isTRUE(order)) {
    df$Group <- factor(df$Group, levels = df$Group[order(df$Detail)])
  }

  # Create the bar plot
  p <- ggplot(df, aes(x = Group, y = Detail, fill = Group)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = color) +
    theme +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(fill = "Subset") +
    geom_text(aes(label = Detail), vjust = -0.3, size = textsize) +
    ylim(0, max(df$Detail) * 1.1)  # Add space for text labels

  # Add title and axis labels if provided
  if (!is.null(title)) {
    p <- p + ggtitle(title)
  }

  if (!is.null(xlabel)) {
    p <- p + xlab(xlabel)
  }

  if (!is.null(ylabel)) {
    p <- p + ylab(ylabel)
  }

  return(p)
})

# Helper functions

#' @title Add row names as a column
#' @description Adds row names as a column named "RowNxyz"
#' @param x A data.frame
#' @return A data.frame with row names as a column
#' @importFrom dplyr select everything
#' @author Kai Guo
#' @keywords internal
add_rownames_column <- function(x) {
  if (!"RowNxyz" %in% colnames(x)) {
    x$RowNxyz <- rownames(x)
  } else {
    # Ensure the column name is exactly "RowNxyz"
    colnames(x)[colnames(x) == "RowNxyz"] <- "RowNxyz"
  }

  # Move RowNxyz to the first column
  x <- x %>% select(RowNxyz, everything())

  return(x)
}

#' @title Add prefix to column names
#' @description Adds a prefix to column names except for "RowNxyz"
#' @param x A data.frame
#' @param prefix Prefix to add
#' @param sep Separator between prefix and original column name
#' @return A data.frame with prefixed column names
#' @author Kai Guo
#' @keywords internal
add_prefix <- function(x, prefix, sep = "_") {
  # Skip RowNxyz column if it exists
  if ("RowNxyz" %in% colnames(x)) {
    rownames_col <- which(colnames(x) == "RowNxyz")
    other_cols <- setdiff(seq_len(ncol(x)), rownames_col)
    colnames(x)[other_cols] <- paste(prefix, colnames(x)[other_cols], sep = sep)
  } else {
    # Add prefix to all columns
    colnames(x) <- paste(prefix, colnames(x), sep = sep)
  }

  return(x)
}

#' @title Set row names from a column
#' @description Sets row names from a specified column
#' @param x A data.frame
#' @param col Column index or name to use for row names
#' @return A data.frame with row names from the specified column
#' @author Kai Guo
#' @keywords internal
set_rownames <- function(x, col) {
  if (is.character(col)) {
    if (!col %in% colnames(x)) {
      stop("Column '", col, "' not found in data frame")
    }
    ind <- which(colnames(x) == col)
  } else {
    if (col < 1 || col > ncol(x)) {
      stop("Column index out of bounds: ", col)
    }
    ind <- col
  }

  # Set row names from the selected column
  rownames(x) <- x[[ind]]

  # Optionally, rename the column to "RowNxyz"
  colnames(x)[ind] <- "RowNxyz"

  return(x)
}

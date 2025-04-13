#' @title Plot a Venn object
#' @description Unified plotting function for Venn objects that supports multiple visualization types
#' @param x A Venn object
#' @param type Type of plot: "venn" (traditional Venn diagram), "vennpie" (pie-chart style),
#'   "upset" (UpSet plot), or "bar" (bar plot of subset sizes)
#' @param title Optional plot title
#' @param interactive Logical: create an interactive plot? Default: FALSE
#' @param filename Optional file name to save the plot
#' @param width Width of the saved plot (default: 8)
#' @param height Height of the saved plot (default: 6)
#' @param dpi Resolution in dots per inch (default: 300)
#' @param ... Additional arguments passed to the specific plotting function
#' @return A ggplot2 or plotly object
#' @method plot Venn
#' @author Kai Guo
#' @export
#' @examples
#' # Create a Venn object
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#' res <- venndetail(list(A = A, B = B, C = C))
#'
#' # Traditional Venn diagram
#' plot(res, type = "venn")
#'
#' # Venn-pie visualization
#' plot(res, type = "vennpie")
#'
#' # UpSet plot
#' plot(res, type = "upset")
#'
#' # Bar plot of subset sizes
#' plot(res, type = "bar")
plot.Venn <- function(x, type = "venn", title = NULL, interactive = FALSE,
                      filename = NULL, width = 8, height = 6, dpi = 300, ...) {
  # Create the plot based on the type
  p <- switch(type,
              venn = vennDiagram(x, title = title, interactive = interactive, ...),
              vennpie = vennpie(x, title = title, interactive = interactive, ...),
              upset = upsetPlot(x, title = title, interactive = interactive, ...),
              bar = dplot(x, title = title, ...),
              stop("Unsupported plot type: ", type))

  # Save the plot if filename is provided
  if (!is.null(filename)) {
    ggsave(p, filename = filename, width = width, height = height, dpi = dpi)
  }

  return(p)
}


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
#' @importFrom methods show
#' @exportMethod show
#' @author Kai Guo
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

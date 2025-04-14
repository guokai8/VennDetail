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
#'
#' @section Parameters for Traditional Venn Diagram (type = "venn"):
#' \describe{
#'   \item{fill}{Colors for filling the circles}
#'   \item{alpha}{Transparency level for the circles (0-1), default: 0.5}
#'   \item{labels}{Logical: show set labels? Default: TRUE}
#'   \item{counts}{Logical: show counts? Default: TRUE}
#'   \item{showNumbers}{Logical: whether to show counts and percentages in each region, default: TRUE}
#'   \item{numberSize}{Size of the count labels, default: 4}
#'   \item{numberColor}{Color of the count labels, default: "black"}
#'   \item{labelSize}{Size of the set labels, default: 4}
#'   \item{labelColor}{Color of the set labels, default: "black"}
#'   \item{borderCol}{Logical: whether to color the borders of circles, default: FALSE}
#'   \item{fillCol}{Logical: whether to fill circles with colors, default: TRUE}
#'   \item{fixedCoords}{Logical: whether to use fixed coordinates, default: TRUE}
#'   \item{xlim}{Vector with 2 numbers, x axis limits for the venn diagram, default: c(0, 1)}
#'   \item{ylim}{Vector with 2 numbers, y axis limits for the venn diagram, default: c(0, 1)}
#'   \item{show_percentages}{Logical: show percentages alongside counts? Default: TRUE}
#'   \item{show_unique_only}{Logical: show counts only for unique elements? Default: FALSE}
#'   \item{scaled}{Logical: scale circles by set size? Default: FALSE}
#' }
#'
#' @section Parameters for Venn-Pie Plot (type = "vennpie"):
#' \describe{
#'   \item{subset}{Character vector of subset names to highlight}
#'   \item{top}{Maximum number of subsets to display, default: 31}
#'   \item{min}{Minimum number of sets an element must be in, default: 0}
#'   \item{color}{Optional vector of colors for the subsets}
#'   \item{revcolor}{Color for non-highlighted subsets, default: "lightgrey"}
#'   \item{any}{Highlight subsets shared by exactly this many sets}
#'   \item{show.number}{Logical: show counts in labels? Default: TRUE}
#'   \item{show.x}{Logical: show subset labels? Default: TRUE}
#'   \item{sep}{Character separator for subset names, default: "_"}
#'   \item{log}{Logical: use log scale for counts? Default: FALSE}
#'   \item{base}{Base for log transformation if log=TRUE}
#'   \item{percentage}{Logical: show percentages instead of counts? Default: FALSE}
#' }
#'
#' @section Parameters for UpSet Plot (type = "upset"):
#' \describe{
#'   \item{nintersects}{Maximum number of intersections to show, default: 40}
#'   \item{min_size}{Minimum intersection size to include, default: 1}
#'   \item{sets_bar_color}{Colors for the set size bars}
#'   \item{main_bar_color}{Color for the intersection size bars, default: "steelblue"}
#'   \item{point_size}{Size of points in the matrix, default: 3}
#'   \item{line_size}{Width of lines in the matrix, default: 1}
#'   \item{show_numbers}{Logical: show counts on bars? Default: TRUE}
#'   \item{sort_intersections_by}{How to sort intersections: "freq" (default), "degree", or "custom"}
#'   \item{sort_sets_by}{How to sort sets: "size" (default), "name", or "custom"}
#'   \item{sort_sets_decreasing}{Whether to sort sets in decreasing order, default: TRUE}
#'   \item{custom_sets_order}{Custom order for sets if sort_sets_by="custom"}
#'   \item{sort_intersections_decreasing}{Whether to sort intersections in decreasing order, default: TRUE}
#'   \item{custom_intersections_order}{Custom order for intersections if sort_intersections_by="custom"}
#'   \item{intersection_color}{Color for intersection dots and lines, default: "black"}
#'   \item{highlight_intersections}{Vector of intersection IDs to highlight}
#'   \item{highlight_color}{Color for highlighted intersections, default: "darkorange"}
#'   \item{empty_point_size}{Size of empty points in the matrix, default: 1.5}
#'   \item{bar_width}{Width of bars (0-1 scale), default: 0.7}
#'   \item{text_angle}{Angle for text labels, default: 0}
#'   \item{text_size}{Size of text in the plot, default: 10}
#'   \item{set_label_size}{Size of set size labels, default: 3}
#'   \item{intersection_label_size}{Size of intersection size labels, default: 3}
#'   \item{point_outline_color}{Color for the outline of points, default: "black"}
#'   \item{point_stroke}{Width of point outline, default: 0.3}
#'   \item{set_size_show_values}{Whether to show set size values, default: TRUE}
#'   \item{intersection_size_show_values}{Whether to show intersection size values, default: TRUE}
#'   \item{show_empty_intersections}{Whether to show empty intersections, default: FALSE}
#'   \item{show_set_labels}{Whether to show set labels, default: TRUE}
#'   \item{plot_margin}{Margin around the plots in cm, default: 0.5}
#'   \item{height_ratio}{Ratio of matrix to total height, default: 0.7}
#'   \item{width_ratio}{Ratio of set size to total width, default: 0.3}
#'   \item{bar_offset}{Horizontal offset for top bars, default: -0.01}
#'   \item{set_text_size}{Size of set labels, default: 10}
#'   \item{intersection_title}{Title for the intersection size plot, default: "Intersection Size"}
#'   \item{set_size_title}{Title for the set size plot, default: "Set Size"}
#'   \item{matrix_point_shape}{Shape of the dots in the matrix (21=filled circle), default: 21}
#'   \item{number_color_threshold}{Fraction of max value where number color switches, default: 0.75}
#'   \item{number_colors}{Named vector with colors for labels on/off bars, default: c(on_bar="black", off_bar="black")}
#'   \item{theme_params}{List of theme parameters for customization}
#'   \item{return_data}{Whether to return the data along with the plot, default: FALSE}
#' }
#'
#' @section Parameters for Bar Plot (type = "bar"):
#' \describe{
#'   \item{order}{Logical: should bars be ordered by count? Default: FALSE}
#'   \item{textsize}{Numeric: size of text labels above bars, default: 5}
#'   \item{color}{Optional vector of colors for the bars}
#'   \item{theme}{The ggplot2 theme to use, default: theme_light()}
#'   \item{xlabel}{Optional x-axis label}
#'   \item{ylabel}{Optional y-axis label}
#' }
#'
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
#' # Venn diagram with custom colors and transparency
#' plot(res, type = "venn", fill = c("red", "blue", "green"), alpha = 0.3)
#'
#' # Venn-pie visualization
#' plot(res, type = "vennpie")
#'
#' # Venn-pie with highlighted subsets
#' plot(res, type = "vennpie", any = 2, log = TRUE)
#'
#' # UpSet plot
#' plot(res, type = "upset")
#'
#' # UpSet plot with custom sorting and highlighting
#' plot(res, type = "upset",
#'      sort_sets_by = "size",
#'      highlight_intersections = c(1, 2),
#'      highlight_color = "red")
#'
#' # Bar plot of subset sizes
#' plot(res, type = "bar")
#'
#' # Ordered bar plot with larger text
#' plot(res, type = "bar", order = TRUE, textsize = 8)
#'
#' # Save plot to file
#' plot(res, type = "venn", filename = "my_venn.png", width = 10, height = 8)
#'
#' # Create interactive plot
#' if(interactive()) {
#'   plot(res, type = "venn", interactive = TRUE)
#' }
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

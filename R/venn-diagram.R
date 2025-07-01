
#' Create a traditional Venn diagram
#'
#' @description Creates a traditional Venn diagram for 2-5 sets
#' @param object A Venn object
#' @param fill Colors for filling the circles
#' @param alpha Transparency level for the circles (0-1)
#' @param labels Logical: show set labels? (default: TRUE)
#' @param counts Logical: show counts? (default: TRUE)
#' @param showNumbers Logical: whether to show counts and percentages in each region
#' @param numberSize Size of the count labels
#' @param numberColor Color of the count labels
#' @param labelSize Size of the set labels
#' @param labelColor Color of the set labels
#' @param borderCol Logical: whether to color the borders of circles
#' @param fillCol Logical: whether to fill circles with colors
#' @param fixedCoords Logical: whether to use fixed coordinates
#' @param xlim Vector with 2 numbers, x axis limits for the venn diagram
#' @param ylim Vector with 2 numbers, y axis limits for the venn diagram
#' @param show_percentages Logical: show percentages alongside counts? (default: TRUE)
#' @param show_unique_only Logical: show counts only for unique elements? (default: FALSE)
#' @param scaled Logical: scale circles by set size? (default: FALSE)
#' @param title Optional plot title
#' @param interactive Logical: create interactive plot? (default: FALSE)
#' @param ... Additional arguments passed to ggplot2 functions
#' @return A ggplot2 or plotly object
#' @import ggplot2
#' @importFrom stats setNames
#' @importFrom utils head tail
#' @importFrom grDevices colorRampPalette colors
#' @importFrom plotly ggplotly
#' @importFrom methods is slot
#' @author Kai Guo
#' @export
setMethod("vennDiagram", signature("Venn"), function(object,
                                                     fill = NULL,
                                                     alpha = 0.5,
                                                     labels = TRUE,
                                                     counts = TRUE,
                                                     showNumbers = TRUE,
                                                     numberSize = 4,
                                                     numberColor = "black",
                                                     labelSize = 4,
                                                     labelColor = "black",
                                                     borderCol = FALSE,
                                                     fillCol = TRUE,
                                                     fixedCoords = TRUE,
                                                     xlim = c(0, 1),
                                                     ylim = c(0, 1),
                                                     show_percentages = TRUE,
                                                     show_unique_only = FALSE,
                                                     scaled = FALSE,
                                                     title = NULL,
                                                     interactive = FALSE,
                                                     ...) {
  # Extract data from the Venn object
  set_lists <- object@input
  set_names <- object@GroupNames

  # Validate number of sets
  if (length(set_lists) < 2 || length(set_lists) > 5) {
    stop("Traditional Venn diagrams support between 2 and 5 sets")
  }

  # Generate colors if not provided
  if (is.null(fill)) {
    fill <- setcolor(length(set_lists), palette = "categorical")
  } else if (length(fill) < length(set_lists)) {
    warning("Not enough colors provided, recycling")
    fill <- rep(fill, length.out = length(set_lists))
  }

  # Create the plot using the standalone vennDiagram function
  p <- .vennDiagram(
    setlist = set_lists,
    title = title,
    colors = fill,
    alpha = alpha,
    showNumbers = showNumbers,
    numberSize = numberSize,
    numberColor = numberColor,
    labelSize = labelSize,
    labelColor = labelColor,
    borderCol = borderCol,
    fillCol = fillCol,
    fixedCoords = fixedCoords,
    xlim = xlim,
    ylim = ylim
  )

  # Add custom theme elements
  p <- p + theme(plot.title = element_text(hjust = 0.5, size = 16))

  # Make interactive if requested
  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      warning("Package 'plotly' is required for interactive plots. Using static plot instead.")
      return(p)
    }
    p <- plotly::ggplotly(p)
  }

  return(p)
})
# Helper function to create ellipse
.ellipse <- function(group, x = 0, y = 0, r = 0, npoints = 100, radiusX = 1, radiusY = 1) {
  positions <- seq(0, 2 * pi, length.out = npoints)
  output <- data.frame(
    group = as.character(group),
    x = radiusX * cos(positions),
    y = radiusY * sin(positions)
  )
  xcoord <- output$x
  ycoord <- output$y
  output$x <- (xcoord * cos(r) - ycoord * sin(r)) + x
  output$y <- (ycoord * cos(r) + xcoord * sin(r)) + y

  return(output)
}

#' Find overlap between sets in a list
#'
#' @param setlist list of character vectors.
#' @param xlim vector with 2 numbers, x axis limits for the venn diagram.
#' @param ylim vector with 2 numbers, y axis limits for the venn diagram.
#' @return list with 2 items: a data.frame with information about the groups (sizes, coordinates, etc.),
#'         and a data.frame containing the x and y coordinates for the venn diagram
#' @keywords internal
.findOverlap <- function(setlist, xlim = c(0, 1), ylim = c(0, 1)) {
  # names of the sets must not contain points, as this will be used to distinguish them in the loop
  names(setlist) <- gsub("\\.", "_", names(setlist))

  ## iteratively compare all sets to each other. Every iteration will reach a higher level (overlap of overlaps)
  overlap <- setlist
  for (i in 1:length(setlist)) { # i is not used in the loop; it is just a counter
    overlap <- lapply(setlist, function(x) lapply(overlap, function(y) x[x %in% y])) # find overlap between everything vs. everything
    overlap <- unlist(overlap, recursive = F)
    betterNames <- sapply(names(overlap), function(x) paste(sort(unique(unlist(strsplit(x, "\\.")))), collapse = ".")) # a list of re-sorted item names (so that redundant comparisons have the same name, e.g. a.b & b.a become a.b & a.b)
    names(overlap) <- betterNames
    overlap <- overlap[!duplicated(names(overlap))] # remove redundant comparisons
  }

  ##
  contents <- sapply(overlap, function(x) paste(x, collapse = ","))
  sizes <- sapply(overlap, length)
  groups <- unlist(lapply(betterNames, function(x) strsplit(x, "\\.")), recursive = F)

  overlap2 <- overlap
  for (i in length(setlist):1) {
    groupi <- groups[sapply(groups, length) == i]
    groupsmalleri <- groups[sapply(groups, length) < i]
    groupi2 <- unlist(lapply(groupi, function(x) paste(x, collapse = ".")))
    groupsmalleri2 <- unlist(lapply(groupsmalleri, function(x) paste(x, collapse = ".")))

    for (z in 1:length(groupi2)) {

      overlap2 <- mapply(function(x, y) {
        if (y %in% groupsmalleri2) {
          setdiff(x, overlap[[groupi2[[z]]]])
        } else {
          x
        }
      }, x = overlap2, y = names(overlap2), SIMPLIFY = FALSE)

    }
  }

  contents <- sapply(overlap2, function(x) paste0(x, collapse = ","))
  sizes <- sapply(overlap2, length)
  labels <- sapply(names(sizes), function(x) if (x %in% names(setlist)) {
    x
  } else {
    NA
  })
  # groups <- unlist( lapply(betterNames, function(x) strsplit(x, "\\.")), recursive=F)

  setlength <- as.character(length(setlist))
  coordinates <- switch(
    as.character(setlength),
    "2" = data.frame(x = c(a1 = -0.5, a12 = 0.5, a2 = 1.5),
                     y = c(a1 = 0, a12 = 0, a2 = 0)),
    "3" = data.frame(x = c(a1 = -0.3, a12 = 0.5, a13 = -.1, a123 = 0.5, a2 = 1.3, a23 = 1.1, a3 = 0.5),
                     y = c(a1 = -0.09, a12 = -1/4, a13 = 0.7, a123 = 1/3, a2 = -0.09, a23 = 0.7, a3 = 1.3)),
    "4" = data.frame(x = c(a1 = -1.7, a12 = 0, a13 = -1, a14 = -.9, a123 = .4, a124 = -.4, a134 = -.5, a1234 = 0, a2 = 1.7, a23 = 1, a24 = .9, a234 = .5, a3 = -.55, a34 = 0, a4 = .55),
                     y = c(a1 = .5, a12 = -1.3, a13 = 1, a14 = -.7, a123 = -1, a124 = -1, a134 = .5, a1234 = -.2, a2 = .5, a23 = -.7, a24 = 1, a234 = .5, a3 = 1.45, a34 = 1.2, a4 = 1.45)),
    "5" = data.frame(x = c(a1 = 0, a12 = .6, a13 = -.2, a14 = -.2, a15 = -.85, a123 = .15, a124 = .75, a125 = -.9, a134 = .3, a135 = -.6, a145 = -.5, a1234 = .5, a1235 = -.5, a1245 = -.7, a1345 = .4, a12345 = .1, a2 = 1.7, a23 = 1.4, a24 = 1.05, a25 = -1.06, a234 = 1.1, a235 = 1.25, a245 = -.99, a2345 = 1.1, a3 = 1.45, a34 = .54, a35 = 1.1, a345 = .85, a4 = -.85, a45 = -1, a5 = -1.4),
                     y = c(a1 = 1.3, a12 = 1, a13 = .98, a14 = -1.37, a15 = .6, a123 = .9, a124 = .84, a125 = 0, a134 = -1.35, a135 = .7, a145 = -1.06, a1234 = .75, a1235 = .5, a1245 = -.6, a1345 = -1.14, a12345 = -.2, a2 = .4, a23 = -.3, a24 = .65, a25 = -.26, a234 = .3, a235 = -.55, a245 = -.6, a2345 = -.3, a3 = -1.1, a34 = -1.4, a35 = -.9, a345 = -1, a4 = -1.4, a45 = -.8, a5 = .2))
  )
  labelcoordinates <- coordinates
  if (setlength == "2") labelcoordinates$y <- labelcoordinates$y + 0.3
  if (setlength == "3") {
    labelcoordinates$y <- labelcoordinates$y + c(-0.3, 0, 0, 0, -0.3, 0, 0.3)
    labelcoordinates$x <- labelcoordinates$x + c(-0.3, 0, 0, 0, +0.3, 0, 0)
  }
  if (setlength == "4") {
    labelcoordinates$y <- labelcoordinates$y + 0.25
    labelcoordinates$x <- labelcoordinates$x + c(-0.1, rep(0, 7), 0.1, 0, 0, 0, -0.1, 0, 0.1)
  }
  if (setlength == "5") {
    labelcoordinates$y <- labelcoordinates$y + c(0.25, rep(0, 15), 0.25, rep(0, 7), -0.25, 0, 0, 0, -0.25, 0, 0.25)
    labelcoordinates$x <- labelcoordinates$x + c(0, rep(0, 15), 0, rep(0, 7), 0, 0, 0, 0, 0, 0, 0)
  }
  colnames(labelcoordinates) <- c("labx", "laby")

  rot4 <- .9
  circleParams <- switch(
    as.character(length(setlist)),
    "2" = list(x = c(0, 1), y = c(0, 0), l = 1, r = 0),
    "3" = list(x = c(0, 1, 0.5), y = c(0, 0, 1), l = 1, r = 0),
    "4" = list(x = c(-.7, .7, -.1, .1), y = c(0, 0, .3, .3), l = 1.8, r = c(-rot4, rot4, -rot4, rot4)),
    "5" = list(x = c(0, .4, .4, .1, -.18), y = c(0, 0, -.3, -.5, -.2), l = 1.6, r = c(.5 * pi, .1 * pi, .7 * pi, .3 * pi, .9 * pi))
  )
  dfcircles <- mapply(function(label, x, y, l, r) .ellipse(group = label, x = x, y = y, r = r, radiusX = l), label = names(setlist),
                      x = circleParams$x, y = circleParams$y,
                      l = circleParams$l, r = circleParams$r,
                      SIMPLIFY = FALSE)
  dfcircles <- do.call(rbind, dfcircles)
  numbers <- data.frame(group = names(sizes), label = labels, size = sizes, content = contents, coordinates, labelcoordinates)

  # resize everything
  xdelta <- abs(diff(xlim))
  ydelta <- abs(diff(ylim))

  numbers$x <- numbers$x - min(dfcircles$x)
  numbers$y <- numbers$y - min(dfcircles$y)
  numbers$labx <- numbers$labx - min(dfcircles$x)
  numbers$laby <- numbers$laby - min(dfcircles$y)
  dfcircles$x <- dfcircles$x - min(dfcircles$x)
  dfcircles$y <- dfcircles$y - min(dfcircles$y)
  numbers$x <- numbers$x / max(dfcircles$x) * xdelta + min(xlim)
  numbers$y <- numbers$y / max(dfcircles$y) * ydelta + min(ylim)
  numbers$labx <- numbers$labx / max(dfcircles$x) * xdelta + min(xlim)
  numbers$laby <- numbers$laby / max(dfcircles$y) * ydelta + min(ylim)
  dfcircles$x <- dfcircles$x / max(dfcircles$x) * xdelta + min(xlim)
  dfcircles$y <- dfcircles$y / max(dfcircles$y) * ydelta + min(ylim)

  return(list(table = numbers, ellipses = dfcircles))
}

#' Create a standalone Venn diagram
#'
#' This function creates a standalone Venn diagram for 2-5 sets
#'
#' @param setlist A named list of character vectors, each representing a set
#' @param title Optional title for the plot
#' @param colors Vector of colors for the sets. If NULL, default colors will be used
#' @param alpha Transparency level for the circles (0-1)
#' @param showNumbers Logical, whether to show counts and percentages in each region
#' @param numberSize Size of the count labels
#' @param numberColor Color of the count labels
#' @param labelSize Size of the set labels
#' @param labelColor Color of the set labels
#' @param borderCol Logical, whether to color the borders of circles
#' @param fillCol Logical, whether to fill circles with colors
#' @param fixedCoords Logical, whether to use fixed coordinates
#' @param xlim Vector with 2 numbers, x axis limits for the venn diagram
#' @param ylim Vector with 2 numbers, y axis limits for the venn diagram
#'
#' @return A ggplot object
#' @author Kai Guo
#' @keywords internal
.vennDiagram <- function(setlist,
                         title = NULL,
                         colors = NULL,
                         alpha = 0.4,
                         showNumbers = TRUE,
                         numberSize = 4,
                         numberColor = "black",
                         labelSize = 4,
                         labelColor = "black",
                         borderCol = FALSE,
                         fillCol = TRUE,
                         fixedCoords = TRUE,
                         xlim = c(0, 1),
                         ylim = c(0, 1)) {

  # Check for required packages and load them
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 is required but not installed. Please install it with: install.packages('ggplot2')")
  }

  # Check if the input is valid
  if (!is.list(setlist)) {
    stop("setlist must be a list")
  }

  if (length(setlist) < 2 || length(setlist) > 5) {
    stop("This function supports between 2 and 5 sets")
  }

  if (is.null(names(setlist))) {
    warning("setlist has no names, using default names: Set1, Set2, etc.")
    names(setlist) <- paste0("Set", 1:length(setlist))
  }

  # Set default colors if not provided
  if (is.null(colors)) {
    default_colors <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD")
    colors <- default_colors[1:length(setlist)]
  } else if (length(colors) < length(setlist)) {
    warning("Not enough colors provided, recycling")
    colors <- rep(colors, length.out = length(setlist))
  }

  # Find overlaps and get coordinates
  overlap <- .findOverlap(setlist, xlim = xlim, ylim = ylim)
  df <- overlap[["table"]] # table contains labels and numbers with coordinates
  ellipses <- overlap[["ellipses"]] # table contains coordinates for the ellipses to display

  # Create the base plot
  p <- ggplot2::ggplot()

  # Add ellipses (circles)
  if (fillCol) {
    p <- p + ggplot2::geom_polygon(
      data = ellipses,
      mapping = ggplot2::aes(x = x, y = y, group = group, fill = group),
      alpha = alpha,
      color = if (borderCol) ggplot2::aes(color = group) else "black"
    )
  } else {
    p <- p + ggplot2::geom_polygon(
      data = ellipses,
      mapping = ggplot2::aes(x = x, y = y, group = group),
      fill = NA,
      color = if (borderCol) ggplot2::aes(color = group) else "black"
    )
  }

  # Add colors
  p <- p + ggplot2::scale_fill_manual(values = colors)
  if (borderCol) {
    p <- p + ggplot2::scale_color_manual(values = colors)
  }

  # Add numbers and percentages if requested
  if (showNumbers) {
    # Calculate total elements for percentage
    all_elements <- unique(unlist(setlist))
    total_count <- length(all_elements)

    # Create labels with number and percentage
    df$label_with_pct <- paste0(df$size, "\n(", round(df$size / total_count * 100, 1), "%)")

    p <- p + ggplot2::geom_text(
      data = df,
      mapping = ggplot2::aes(x = x, y = y, label = label_with_pct),
      size = numberSize,
      color = numberColor
    )
  }

  # Add set labels
  df_labels <- df[!is.na(df$label), ]
  p <- p + ggplot2::geom_text(
    data = df_labels,
    mapping = ggplot2::aes(x = labx, y = laby, label = label),
    size = labelSize,
    color = labelColor
  )

  # Adjust the plot ratio
  ratio <- abs(diff(xlim) / diff(ylim))
  ratio <- ifelse(length(setlist) == 2, ratio / 1.5,
                  ifelse(length(setlist) == 4, ratio / 1.2, ratio))

  if (fixedCoords) {
    p <- p + ggplot2::coord_fixed(ratio)
  }

  # Add title if provided
  if (!is.null(title)) {
    p <- p + ggplot2::ggtitle(title)
  }

  # Clean up the theme
  p <- p + ggplot2::theme_void() +
    ggplot2::theme(
      legend.position = "none",
      plot.title = ggplot2::element_text(hjust = 0.5)
    )

  return(p)
}

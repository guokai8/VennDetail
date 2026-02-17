# Generic function definitions for VennDetail package

#' @title Extract results from a Venn object
#' @description Retrieves results from a Venn object in long or wide format
#' @param object A Venn object
#' @param wide Logical: should results be returned in wide format? Default: FALSE
#' @return A data.frame containing subset information
#' @author Kai Guo
#' @export
#' @examples
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#' res <- venndetail(list(A = A, B = B, C = C))
#' # Get results in long format
#' result_long <- result(res)
#' # Get results in wide format
#' result_wide <- result(res, wide = TRUE)
setGeneric("result", function(object, wide = FALSE)
  standardGeneric("result")
)

#' @title Get subset details from a Venn object
#' @description Returns a named numeric vector with counts for each subset
#' @param object A Venn object
#' @return A named numeric vector with counts for each subset
#' @author Kai Guo
#' @export
#' @examples
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#' res <- venndetail(list(A = A, B = B, C = C))
#' detail(res)
setGeneric("detail", function(object)
  standardGeneric("detail")
)

#' @title Create a bar plot of subset counts
#' @description Creates a bar plot showing counts for each subset
#' @param object A Venn object
#' @param order Logical: should bars be ordered by count? Default: FALSE
#' @param textsize Numeric: size of text labels above bars. Default: 5
#' @param color Optional vector of colors for the bars
#' @param theme The ggplot2 theme to use. Default: theme_light
#' @param title Optional plot title
#' @param xlabel Optional x-axis label
#' @param ylabel Optional y-axis label
#' @return A ggplot2 object
#' @author Kai Guo
#' @export
#' @examples
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#' res <- venndetail(list(A = A, B = B, C = C))
#' dplot(res, order = TRUE, textsize = 3)
setGeneric("dplot", function(object, order = FALSE, textsize = 5,
                             color = NULL, theme = ggplot2::theme_light(),
                             title = NULL, xlabel = NULL, ylabel = NULL)
  standardGeneric("dplot")
)

#' @title Join data.frames by row names or specified columns
#' @description Joins two data.frames using various join methods
#' @param x First data.frame
#' @param y Second data.frame
#' @param fun Join function to use: "full_join", "left_join", "right_join", etc.
#' @param by Optional vector of column names to join by
#' @return A joined data.frame
#' @author Kai Guo
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
setGeneric("rowjoin", function(x, y, fun = "full_join", by = NULL)
  standardGeneric("rowjoin")
)

#' @title Extract feature data for specific subsets
#' @description Combines subset information with user-supplied data frames
#' @param object A Venn object
#' @param subset Character vector of subset names to extract
#' @param rlist List of user-supplied data frames
#' @param userowname Logical: use row names for joining? Default: TRUE
#' @param gind Column names or indices to use for joining if userowname=FALSE
#' @param sep Character used to separate group names in column names
#' @param wide Logical: return results in wide format? Default: FALSE
#' @return A data.frame combining subset information with user data
#' @author Kai Guo
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
#'                      rlist = list(dA, dB, dC),
#'                      userowname = FALSE,
#'                      gind = rep(1, 3))
setGeneric("getFeature", function(object, subset, rlist, userowname = TRUE,
                                  gind = NULL, sep = "_", wide = FALSE)
  standardGeneric("getFeature")
)

#' @title Create a Venn-pie visualization
#' @description Creates a pie-chart-like visualization of set intersections
#' @param object A Venn object
#' @param subset Character vector of subset names to highlight
#' @param top Maximum number of subsets to display
#' @param min Minimum number of sets an element must be in
#' @param color Optional vector of colors for the subsets
#' @param revcolor Color for non-highlighted subsets
#' @param any Highlight subsets shared by exactly this many sets
#' @param show.number Logical: show counts in labels?
#' @param show.x Logical: show subset labels?
#' @param sep Character separator for subset names
#' @param log Logical: use log scale for counts?
#' @param base Base for log transformation if log=TRUE
#' @param percentage Logical: show percentages instead of counts?
#' @param title Optional plot title
#' @param interactive Logical: create interactive plot?
#' @param ... Additional arguments
#' @return A ggplot2 or plotly object
#' @author Kai Guo
#' @export
setGeneric("vennpie", function(object, subset = NULL, top = 31,
                               min = 0, color = NULL, revcolor = "lightgrey",
                               any = NULL, show.number = TRUE, show.x = TRUE,
                               sep = "_", log = FALSE, base = NULL,
                               percentage = FALSE, title = NULL,
                               interactive = FALSE, ...)
  standardGeneric("vennpie")
)

#' @title Extract specific subsets from a Venn object
#' @description Extracts elements from specified subsets
#' @param object A Venn object
#' @param subset Character vector of subset names to extract
#' @param min Minimum number of sets an element must be in. Default: 0
#' @param wide Logical: return results in wide format? Default: FALSE
#' @return A data.frame with elements from the specified subsets
#' @author Kai Guo
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
setGeneric("getSet", function(object, subset = NULL, min = 0, wide = FALSE)
  standardGeneric("getSet")
)

#' Create a Venn diagram
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
#' @author Kai Guo
#' @export
setGeneric("vennDiagram", function(object,
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
                                   ...)
           standardGeneric("vennDiagram")
)

# This needs to be added to the appropriate file where you define your generics (AllGenerics.R)

#' @title Create an UpSet plot
#' @description Creates an UpSet plot to visualize set intersections
#' @param object A Venn object
#' @param nintersects Maximum number of intersections to show
#' @param min_size Minimum intersection size to include (default: 1)
#' @param sets_bar_color Colors for the set size bars
#' @param main_bar_color Color for the intersection size bars
#' @param point_size Size of points in the matrix
#' @param line_size Width of lines in the matrix
#' @param show_numbers Logical: show counts on bars?
#' @param sort_intersections_by How to sort intersections
#' @param sort_sets_by How to sort sets
#' @param sort_sets_decreasing Whether to sort sets in decreasing order
#' @param custom_sets_order Custom order for sets if sort_sets_by="custom"
#' @param sort_intersections_decreasing Whether to sort intersections in decreasing order
#' @param custom_intersections_order Custom order for intersections
#' @param intersection_color Color for intersection dots and lines
#' @param highlight_intersections Vector of intersection IDs to highlight
#' @param highlight_color Color for highlighted intersections
#' @param empty_point_size Size of empty points in the matrix
#' @param bar_width Width of bars
#' @param text_angle Angle for text labels
#' @param text_size Size of text in the plot
#' @param set_label_size Size of set size labels
#' @param intersection_label_size Size of intersection size labels
#' @param point_outline_color Color for the outline of points
#' @param point_stroke Width of point outline
#' @param set_size_show_values Whether to show set size values
#' @param intersection_size_show_values Whether to show intersection size values
#' @param show_empty_intersections Whether to show empty intersections
#' @param show_set_labels Whether to show set labels
#' @param plot_margin Margin around the plots in cm
#' @param height_ratio Ratio of matrix to total height
#' @param width_ratio Ratio of set size to total width
#' @param bar_offset Horizontal offset for top bars
#' @param set_text_size Size of set labels
#' @param intersection_title Title for the intersection size plot
#' @param set_size_title Title for the set size plot
#' @param matrix_point_shape Shape of the dots in the matrix
#' @param number_color_threshold Fraction of max value for label color switch
#' @param number_colors Colors for labels on/off bars
#' @param theme_params Theme parameters for customization
#' @param title Optional plot title
#' @param interactive Create interactive plot?
#' @param return_data Whether to return the data along with the plot
#' @param ... Additional arguments passed to internal functions
#' @return A ggplot object or a combined grid layout
#' @author Kai Guo
#' @examples
#' # Basic example
#' sets <- list(
#'   "Set A" = c(1:100),
#'   "Set B" = c(30:120),
#'   "Set C" = c(20:50, 90:110),
#'   "Set D" = c(10:40, 80:120)
#' )
#' ven <- venndetail(sets)
#' upsetPlot(ven, bar_offset = -0.02)
#'
#' # With highlighting
#' upsetPlot(ven,
#'           highlight_intersections = c(1, 2),
#'           highlight_color = "darkorange",
#'           bar_offset = -0.02)
#'
#' @export
setGeneric("upsetPlot", function(object, nintersects = 40,
                                 min_size = 1,
                                 sets_bar_color = NULL,
                                 main_bar_color = "steelblue",
                                 point_size = 3, line_size = 1,
                                 show_numbers = TRUE,
                                 sort_intersections_by = "freq",
                                 sort_sets_by = "size",
                                 sort_sets_decreasing = TRUE,
                                 custom_sets_order = NULL,
                                 sort_intersections_decreasing = TRUE,
                                 custom_intersections_order = NULL,
                                 intersection_color = "black",
                                 highlight_intersections = NULL,
                                 highlight_color = "darkorange",
                                 empty_point_size = 1.5,
                                 bar_width = 0.7,
                                 text_angle = 0,
                                 text_size = 10,
                                 set_label_size = 3,
                                 intersection_label_size = 3,
                                 point_outline_color = "black",
                                 point_stroke = 0.3,
                                 set_size_show_values = TRUE,
                                 intersection_size_show_values = TRUE,
                                 show_empty_intersections = FALSE,
                                 show_set_labels = TRUE,
                                 plot_margin = 0.5,
                                 height_ratio = 0.7,
                                 width_ratio = 0.3,
                                 bar_offset = -0.01,
                                 set_text_size = 10,
                                 intersection_title = "Intersection Size",
                                 set_size_title = "Set Size",
                                 matrix_point_shape = 21,
                                 number_color_threshold = 0.75,
                                 number_colors = c(on_bar="black", off_bar="black"),
                                 theme_params = list(
                                   background_color = "white",
                                   grid_color = "grey92",
                                   axis_text_color = "black",
                                   use_grid = TRUE,
                                   border_color = NA
                                 ),
                                 title = NULL,
                                 interactive = FALSE,
                                 return_data = FALSE,...)
           standardGeneric("upsetPlot")
)

#' @title Perform statistical tests on set intersections
#' @description Performs statistical tests to evaluate the significance of set intersections
#' @param object A Venn object
#' @param universe Size of the universe for hypergeometric test. Default: NULL (will use the union of all sets)
#' @param method Statistical method to use: "hypergeometric" or "permutation". Default: "hypergeometric"
#' @param nperm Number of permutations if method="permutation". Default: 1000
#' @param adjust.method Method for multiple testing correction. Default: "BH"
#' @param include_singles Logical: include tests for single sets? Default: FALSE
#' @return A data.frame with statistical test results
#' @author Kai Guo
#' @export
#' @examples
#' A <- sample(1:1000, 100, replace = FALSE)
#' B <- sample(1:1000, 150, replace = FALSE)
#' C <- sample(1:1000, 120, replace = FALSE)
#' res <- venndetail(list(A = A, B = B, C = C))
#' stats <- vennStats(res)
setGeneric("vennStats", function(object, universe = NULL,
                                 method = c("hypergeometric", "permutation"),
                                 nperm = 1000, adjust.method = "BH",
                                 include_singles = FALSE)
  standardGeneric("vennStats")
)


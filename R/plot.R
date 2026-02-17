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
#' @param fill Colors for filling the circles (venn)
#' @param alpha Transparency level (0-1), default: 0.5 (venn)
#' @param labels Logical: show set labels? Default: TRUE (venn)
#' @param counts Logical: show counts? Default: TRUE (venn)
#' @param showNumbers Logical: show counts and percentages? Default: TRUE (venn)
#' @param numberSize Size of count labels, default: 4 (venn)
#' @param numberColor Color of count labels, default: "black" (venn)
#' @param labelSize Size of set labels, default: 4 (venn)
#' @param labelColor Color of set labels, default: "black" (venn)
#' @param borderCol Logical: color borders? Default: FALSE (venn)
#' @param fillCol Logical: fill circles? Default: TRUE (venn)
#' @param fixedCoords Logical: fixed coordinates? Default: TRUE (venn)
#' @param xlim X axis limits, default: c(0, 1) (venn)
#' @param ylim Y axis limits, default: c(0, 1) (venn)
#' @param show_percentages Logical: show percentages? Default: TRUE (venn)
#' @param show_unique_only Logical: show unique only? Default: FALSE (venn)
#' @param scaled Logical: scale circles? Default: FALSE (venn)
#' @param subset Character vector of subsets to highlight (vennpie)
#' @param top Maximum subsets to display, default: 31 (vennpie)
#' @param min Minimum set membership, default: 0 (vennpie/getSet)
#' @param color Colors for subsets (vennpie/bar)
#' @param revcolor Color for non-highlighted, default: "lightgrey" (vennpie)
#' @param any Highlight subsets shared by this many sets (vennpie)
#' @param show.number Logical: show counts? Default: TRUE (vennpie)
#' @param show.x Logical: show labels? Default: TRUE (vennpie)
#' @param sep Character separator, default: "_" (vennpie)
#' @param log Logical: use log scale? Default: FALSE (vennpie)
#' @param base Base for log transformation (vennpie)
#' @param percentage Logical: show percentages? Default: FALSE (vennpie)
#' @param nintersects Maximum intersections, default: 40 (upset)
#' @param min_size Minimum intersection size, default: 1 (upset)
#' @param sets_bar_color Colors for set size bars (upset)
#' @param main_bar_color Color for intersection bars, default: "steelblue" (upset)
#' @param point_size Size of matrix points, default: 3 (upset)
#' @param line_size Width of matrix lines, default: 1 (upset)
#' @param show_numbers Logical: show bar counts? Default: TRUE (upset)
#' @param sort_intersections_by Sort method: "freq", "degree", or "custom" (upset)
#' @param sort_sets_by Sort method: "size", "name", or "custom" (upset)
#' @param sort_sets_decreasing Logical: decreasing order? Default: TRUE (upset)
#' @param custom_sets_order Custom set order (upset)
#' @param sort_intersections_decreasing Logical: decreasing? Default: TRUE (upset)
#' @param custom_intersections_order Custom intersection order (upset)
#' @param intersection_color Color for dots/lines, default: "black" (upset)
#' @param highlight_intersections IDs to highlight (upset)
#' @param highlight_color Highlight color, default: "darkorange" (upset)
#' @param empty_point_size Empty point size, default: 1.5 (upset)
#' @param bar_width Bar width (0-1), default: 0.7 (upset)
#' @param text_angle Text angle, default: 0 (upset)
#' @param text_size Text size, default: 10 (upset)
#' @param set_label_size Set label size, default: 3 (upset)
#' @param intersection_label_size Intersection label size, default: 3 (upset)
#' @param point_outline_color Point outline color, default: "black" (upset)
#' @param point_stroke Point outline width, default: 0.3 (upset)
#' @param set_size_show_values Show set values? Default: TRUE (upset)
#' @param intersection_size_show_values Show intersection values? Default: TRUE (upset)
#' @param show_empty_intersections Show empty? Default: FALSE (upset)
#' @param show_set_labels Show set labels? Default: TRUE (upset)
#' @param plot_margin Margin in cm, default: 0.5 (upset)
#' @param height_ratio Matrix height ratio, default: 0.7 (upset)
#' @param width_ratio Set size width ratio, default: 0.3 (upset)
#' @param bar_offset Bar offset, default: -0.01 (upset)
#' @param set_text_size Set label size, default: 10 (upset)
#' @param intersection_title Intersection title (upset)
#' @param set_size_title Set size title (upset)
#' @param matrix_point_shape Point shape, default: 21 (upset)
#' @param number_color_threshold Color threshold, default: 0.75 (upset)
#' @param number_colors Label colors vector (upset)
#' @param theme_params Theme parameters list (upset)
#' @param return_data Return data? Default: FALSE (upset)
#' @param order Logical: order bars? Default: FALSE (bar)
#' @param textsize Text size, default: 5 (bar)
#' @param theme ggplot2 theme (bar)
#' @param xlabel X-axis label (bar)
#' @param ylabel Y-axis label (bar)
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
#' # Save plot to file (not run during check)
#' \dontrun{
#' plot(res, type = "venn", filename = "my_venn.png", width = 10, height = 8)
#' }
#'
#' # Create interactive plot
#' if(interactive()) {
#'   plot(res, type = "venn", interactive = TRUE)
#' }
plot.Venn <- function(
    x,
    type = "venn",
    title = NULL,
    interactive = FALSE,
    filename = NULL,
    width = 8,
    height = 6,
    dpi = 300,

    # Parameters for venn diagram
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

    # Parameters for vennpie
    subset = NULL,
    top = 31,
    min = 0,
    color = NULL,
    revcolor = "lightgrey",
    any = NULL,
    show.number = TRUE,
    show.x = TRUE,
    sep = "_",
    log = FALSE,
    base = NULL,
    percentage = FALSE,

    # Parameters for upset plot
    nintersects = 40,
    min_size = 1,
    sets_bar_color = NULL,
    main_bar_color = "steelblue",
    point_size = 3,
    line_size = 1,
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
    return_data = FALSE,

    # Parameters for bar plot
    order = FALSE,
    textsize = 5,
    theme = ggplot2::theme_light(),
    xlabel = NULL,
    ylabel = NULL,

    ...
) {
  # Create the plot based on the type
  p <- switch(
    type,
    venn = vennDiagram(
      x,
      title = title,
      interactive = interactive,
      fill = fill,
      alpha = alpha,
      labels = labels,
      counts = counts,
      showNumbers = showNumbers,
      numberSize = numberSize,
      numberColor = numberColor,
      labelSize = labelSize,
      labelColor = labelColor,
      borderCol = borderCol,
      fillCol = fillCol,
      fixedCoords = fixedCoords,
      xlim = xlim,
      ylim = ylim,
      show_percentages = show_percentages,
      show_unique_only = show_unique_only,
      scaled = scaled,
      ...
    ),
    vennpie = vennpie(
      x,
      title = title,
      interactive = interactive,
      subset = subset,
      top = top,
      min = min,
      color = color,
      revcolor = revcolor,
      any = any,
      show.number = show.number,
      show.x = show.x,
      sep = sep,
      log = log,
      base = base,
      percentage = percentage,
      ...
    ),
    upset = upsetPlot(
      x,
      title = title,
      interactive = interactive,
      nintersects = nintersects,
      min_size = min_size,
      sets_bar_color = sets_bar_color,
      main_bar_color = main_bar_color,
      point_size = point_size,
      line_size = line_size,
      show_numbers = show_numbers,
      sort_intersections_by = sort_intersections_by,
      sort_sets_by = sort_sets_by,
      sort_sets_decreasing = sort_sets_decreasing,
      custom_sets_order = custom_sets_order,
      sort_intersections_decreasing = sort_intersections_decreasing,
      custom_intersections_order = custom_intersections_order,
      intersection_color = intersection_color,
      highlight_intersections = highlight_intersections,
      highlight_color = highlight_color,
      empty_point_size = empty_point_size,
      bar_width = bar_width,
      text_angle = text_angle,
      text_size = text_size,
      set_label_size = set_label_size,
      intersection_label_size = intersection_label_size,
      point_outline_color = point_outline_color,
      point_stroke = point_stroke,
      set_size_show_values = set_size_show_values,
      intersection_size_show_values = intersection_size_show_values,
      show_empty_intersections = show_empty_intersections,
      show_set_labels = show_set_labels,
      plot_margin = plot_margin,
      height_ratio = height_ratio,
      width_ratio = width_ratio,
      bar_offset = bar_offset,
      set_text_size = set_text_size,
      intersection_title = intersection_title,
      set_size_title = set_size_title,
      matrix_point_shape = matrix_point_shape,
      number_color_threshold = number_color_threshold,
      number_colors = number_colors,
      theme_params = theme_params,
      return_data = return_data,
      ...
    ),
    bar = dplot(
      x,
      title = title,
      order = order,
      textsize = textsize,
      color = color,
      theme = theme,
      xlabel = xlabel,
      ylabel = ylabel,
      ...
    ),
    stop("Unsupported plot type: ", type)
  )

  # Save the plot if filename is provided
  if (!is.null(filename)) {
    ggplot2::ggsave(p, filename = filename, width = width, height = height, dpi = dpi)
  }

  return(p)
}


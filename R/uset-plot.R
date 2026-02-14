
#' Create an UpSet plot for a Venn object
#'
#' @description Creates an UpSet plot to visualize set intersections
#' @param object A Venn object
#' @param nintersects Maximum number of intersections to show (default: 40)
#' @param min_size Minimum intersection size to include (default: 1)
#' @param sets_bar_color Colors for the set size bars (default: NULL for auto-generate)
#' @param main_bar_color Color for the intersection size bars (default: "steelblue")
#' @param point_size Size of points in the matrix (default: 3)
#' @param line_size Width of lines in the matrix (default: 1)
#' @param show_numbers Logical: show counts on bars? (default: TRUE)
#' @param sort_intersections_by How to sort intersections: "freq" (default), "degree"
#' @param sort_sets_by How to sort sets: "size" (default), "name"
#' @param sort_sets_decreasing Whether to sort sets in decreasing order (default: TRUE)
#' @param custom_sets_order Custom order for sets if sort_sets_by="custom"
#' @param sort_intersections_decreasing Whether to sort intersections in decreasing order (default: TRUE)
#' @param custom_intersections_order Custom order for intersections if sort_intersections_by="custom"
#' @param intersection_color Color for intersection dots and lines (default: "black")
#' @param highlight_intersections Vector of intersection IDs to highlight (default: NULL)
#' @param highlight_color Color for highlighted intersections (default: "darkorange")
#' @param empty_point_size Size of empty points in the matrix (default: 1.5)
#' @param bar_width Width of bars (0-1 scale) (default: 0.7)
#' @param text_angle Angle for text labels (default: 0)
#' @param text_size Size of text in the plot (default: 10)
#' @param set_label_size Size of set size labels (default: 3)
#' @param intersection_label_size Size of intersection size labels (default: 3)
#' @param point_outline_color Color for the outline of points (default: "black")
#' @param point_stroke Width of point outline (default: 0.3)
#' @param set_size_show_values Whether to show set size values (default: TRUE)
#' @param intersection_size_show_values Whether to show intersection size values (default: TRUE)
#' @param show_empty_intersections Whether to show empty intersections (default: FALSE)
#' @param show_set_labels Whether to show set labels (default: TRUE)
#' @param plot_margin Margin around the plots in cm (default: 0.5)
#' @param height_ratio Ratio of matrix to total height (default: 0.7)
#' @param width_ratio Ratio of set size to total width (default: 0.3)
#' @param bar_offset Horizontal offset for top bars to improve alignment (default: 0)
#' @param set_text_size Size of set labels (default: 10)
#' @param intersection_title Title for the intersection size plot (default: "Intersection Size")
#' @param set_size_title Title for the set size plot (default: "Set Size")
#' @param matrix_point_shape Shape of the dots in the matrix (21=filled circle) (default: 21)
#' @param number_color_threshold Fraction of max value where number color switches (default: 0.75)
#' @param number_colors Named vector with colors for labels on/off bars (default: c(on_bar="white", off_bar="black"))
#' @param theme_params List of theme parameters for customization (default: list of defaults)
#' @param title Optional plot title
#' @param interactive Logical: create interactive plot? (default: FALSE)
#' @param return_data Whether to return the data along with the plot (default: FALSE)
#' @return A ggplot object or a combined grid layout
#' @import ggplot2
#' @importFrom dplyr filter select group_by_all summarise arrange pull if_else
#' @importFrom tidyr pivot_longer
#' @importFrom stats setNames
#' @importFrom patchwork plot_layout plot_annotation plot_spacer
#' @importFrom plotly ggplotly subplot layout
#' @importFrom methods is slot
#' @importFrom grDevices colorRampPalette
#' @author Kai Guo
#' @export
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
setMethod("upsetPlot", signature("Venn"), function(object,
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
                                                   title = NULL,
                                                   interactive = FALSE,
                                                   return_data = FALSE) {

  # Extract data from the Venn object
  set_lists <- object@input

  # Create the plot using the upset_plot function
  result <- upset_plot(
    data_list = set_lists,
    min_intersection_size = min_size,
    max_sets_display = NULL,
    sort_sets_by = sort_sets_by,
    sort_sets_decreasing = sort_sets_decreasing,
    custom_sets_order = custom_sets_order,
    sort_intersections_by = sort_intersections_by,
    sort_intersections_decreasing = sort_intersections_decreasing,
    custom_intersections_order = custom_intersections_order,
    intersection_color = intersection_color,
    main_bar_color = main_bar_color,
    sets_bar_colors = sets_bar_color,
    highlight_intersections = highlight_intersections,
    highlight_color = highlight_color,
    filled_point_size = point_size,
    empty_point_size = empty_point_size,
    line_size = line_size,
    bar_width = bar_width,
    text_angle = text_angle,
    text_size = text_size,
    set_label_size = set_label_size,
    intersection_label_size = intersection_label_size,
    point_outline_color = point_outline_color,
    point_stroke = point_stroke,
    set_size_show_values = set_size_show_values,
    intersection_size_show_values = show_numbers,
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
    show_numbers_on_bars = show_numbers,
    number_color_threshold = number_color_threshold,
    number_colors = number_colors,
    theme_params = theme_params,
    return_data = TRUE  # Always return data for interactive mode support
  )

  # Add title if specified
  if (!is.null(title)) {
    # If using patchwork, we need to add the title to the combined plot
    if (requireNamespace("patchwork", quietly = TRUE)) {
      result$plot <- result$plot + patchwork::plot_annotation(title = title)
    }
  }

  # Make interactive if requested
  if (interactive) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      warning("Package 'plotly' is required for interactive plots. Using static plot instead.")
      return(result$plot)
    }

    # For upset plots, we need a different approach because patchwork doesn't convert well
    # Create separate plotly objects for each component
    try({
      int_plot <- plotly::ggplotly(result$intersection_plot)
      mat_plot <- plotly::ggplotly(result$matrix_plot)
      set_plot <- plotly::ggplotly(result$set_size_plot)

      # Create a 2x2 grid of plots with the appropriate arrangement
      # Empty plot in top-left, intersection plot in top-right
      # Set size plot in bottom-left, matrix plot in bottom-right
      empty_plot <- plotly::plot_ly() %>%
        plotly::layout(
          xaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE),
          yaxis = list(showticklabels = FALSE, zeroline = FALSE, showgrid = FALSE)
        )
      p <- plotly::subplot(
        empty_plot,
        int_plot,
        set_plot,
        mat_plot,
        nrows = 2,
        heights = c(0.4, 0.6),
        widths = c(0.3, 0.7),
        margin = 0.02,
        shareX = FALSE,
        shareY = FALSE
      )

      # Improve layout
      p <- p %>% plotly::layout(
        title = if(!is.null(title)) title else NULL,
        showlegend = FALSE,
        margin = list(l = 50, r = 20, b = 50, t = 80)
      )

      return(p)
    }, silent = TRUE)

    # Fallback if the above approach doesn't work
    warning("Creating simplified interactive version of UpSet plot")
    p <- plotly::ggplotly(result$intersection_plot)
    return(p)
  }

  # For non-interactive mode or if return_data is TRUE
  if (return_data) {
    return(result)
  } else {
    return(result$plot)
  }
})
#' @title Create an UpSet plot for set intersection visualization
#'
#' @description Creates a custom UpSet plot showing the intersections between sets.
#' It displays the size of each intersection and the sets involved in each intersection.
#'
#' @param data_list A named list of vectors, each containing elements in a set
#' @param sets Optional vector of set names to include (default: all sets in data_list)
#' @param min_intersection_size Minimum intersection size to include (default: 1)
#' @param max_sets_display Maximum number of sets to display (default: all)
#' @param sort_sets_by How to sort the sets: "size", "name", or "custom" (default: "size")
#' @param sort_sets_decreasing Whether to sort sets in decreasing order (default: TRUE)
#' @param custom_sets_order Custom order for sets if sort_sets_by="custom"
#' @param sort_intersections_by How to sort intersections: "freq", "degree", or "custom" (default: "freq")
#' @param sort_intersections_decreasing Whether to sort intersections in decreasing order (default: TRUE)
#' @param custom_intersections_order Custom order for intersections if sort_intersections_by="custom"
#' @param intersection_color Color for intersection dots and lines (default: "black")
#' @param main_bar_color Color for the intersection size bars (default: "steelblue")
#' @param sets_bar_colors Named vector of colors for each set (default: auto-generated)
#' @param highlight_intersections Vector of intersection IDs to highlight (default: NULL)
#' @param highlight_color Color for highlighted intersections (default: "#FF5500")
#' @param filled_point_size Size of filled points in the matrix (default: 2)
#' @param empty_point_size Size of empty points in the matrix (default: 1.5)
#' @param line_size Width of connecting lines (default: 0.5)
#' @param bar_width Width of bars (0-1 scale) (default: 0.7)
#' @param text_angle Angle for text labels (default: 0)
#' @param text_size Size of text in the plot (default: 10)
#' @param set_label_size Size of set size labels (default: 3)
#' @param intersection_label_size Size of intersection size labels (default: 3)
#' @param point_outline_color Color for the outline of points (default: "black")
#' @param point_stroke Width of point outline (default: 0.3)
#' @param set_size_show_values Whether to show set size values (default: TRUE)
#' @param intersection_size_show_values Whether to show intersection size values (default: TRUE)
#' @param show_empty_intersections Whether to show empty intersections (default: FALSE)
#' @param show_set_labels Whether to show set labels (default: TRUE)
#' @param plot_margin Margin around the plots in cm (default: 0.5)
#' @param height_ratio Ratio of matrix to total height (default: 0.7)
#' @param width_ratio Ratio of set size to total width (default: 0.3)
#' @param bar_offset Horizontal offset for top bars to improve alignment (default: 0)
#' @param set_text_size Size of set labels (default: 10)
#' @param intersection_title Title for the intersection size plot (default: "Intersection Size")
#' @param set_size_title Title for the set size plot (default: "Set Size")
#' @param matrix_point_shape Shape of the dots in the matrix (21=filled circle) (default: 21)
#' @param show_numbers_on_bars Logical, whether to display counts on bars (default: TRUE)
#' @param number_color_threshold Fraction of max value where number color switches (default: 0.75)
#' @param number_colors Named vector with colors for labels on/off bars (default: c(on_bar="white", off_bar="black"))
#' @param theme_params List of theme parameters for customization (default: list of defaults)
#' @param return_data Whether to return the data along with the plot (default: FALSE)
#'
#' @return If return_data=FALSE, returns the patchwork plot object.
#'         If return_data=TRUE, returns a list containing the plot and component data.
#'
#' @importFrom magrittr %>%
#' @import ggplot2
#' @importFrom dplyr n desc group_by
#' @importFrom dplyr filter select group_by_all summarise arrange pull if_else
#' @importFrom tidyr pivot_longer
#' @importFrom stats setNames
#' @importFrom patchwork plot_layout plot_annotation plot_spacer
#' @importFrom grid grid.newpage pushViewport viewport grid.layout grid.draw popViewport unit.c gTree gList
#' @importFrom gridExtra arrangeGrob
#' @importFrom plotly ggplotly
#' @importFrom methods is slot
#' @importFrom grDevices colorRampPalette
#' @author Kai Guo
#' @export
#'
#' @examples
#' # Basic example
#' sets <- list(
#'   "Set A" = c(1:100),
#'   "Set B" = c(30:120),
#'   "Set C" = c(20:50, 90:110),
#'   "Set D" = c(10:40, 80:120)
#' )
#' upset_plot(sets)
#'
#' # With highlighting
#' upset_plot(sets,
#'           highlight_intersections = c(1, 2),
#'           highlight_color = "darkorange",
#'           bar_offset = -0.02)
#'
#' # Custom colors
#' set_colors <- c("Set A" = "blue", "Set B" = "green",
#'                "Set C" = "orange", "Set D" = "purple")
#' upset_plot(sets, sets_bar_colors = set_colors,
#'           main_bar_color = "darkblue")
upset_plot <- function(data_list,
                       # Data parameters
                       sets = NULL,
                       min_intersection_size = 1,
                       max_sets_display = NULL,

                       # Sort and order parameters
                       sort_sets_by = "size",          # Options: "size", "name", "custom"
                       sort_sets_decreasing = TRUE,    # TRUE for descending, FALSE for ascending
                       custom_sets_order = NULL,       # For "custom" sort, provide set names in desired order
                       sort_intersections_by = "freq", # Options: "freq", "degree", "custom"
                       sort_intersections_decreasing = TRUE,
                       custom_intersections_order = NULL,

                       # Color parameters
                       intersection_color = "black",
                       main_bar_color = "steelblue",
                       sets_bar_colors = NULL,
                       highlight_intersections = NULL,  # List of pattern IDs to highlight
                       highlight_color = "darkorange",     # Color for highlighted intersections
                       point_outline_color = "black",   # Color for point outlines

                       # Size parameters
                       filled_point_size = 2,
                       empty_point_size = 1.5,
                       line_size = 0.5,
                       bar_width = 0.6,           # Width of all bars (0-1)
                       point_stroke = 0.3,        # Width of point outline

                       # Text parameters
                       text_angle = 0,
                       text_size = 10,
                       set_text_size = 10,  # Size of set labels
                       set_label_size = 3,  # Size of set size labels
                       intersection_label_size = 3,

                       # Title parameters
                       intersection_title = "Intersection Size",
                       set_size_title = "Set Size",

                       # Display options
                       matrix_point_shape = 21,  # 21 is filled circle with outline
                       set_size_show_values = TRUE,
                       intersection_size_show_values = TRUE,
                       show_empty_intersections = FALSE,
                       show_set_labels = TRUE,
                       show_numbers_on_bars = TRUE,
                       number_color_threshold = 0.75,  # Fraction of max value where number color switches
                       number_colors = c(on_bar="black", off_bar="black"),

                       # Layout parameters
                       plot_margin = 0.5,
                       height_ratio = 0.5,  # Ratio of matrix to total height
                       width_ratio = 0.3,   # Ratio of set size to total width
                       bar_offset = -0.01,      # Horizontal offset for top bars (default: 0)

                       # Theme options
                       theme_params = list(
                         background_color = "white",
                         grid_color = "grey92",
                         axis_text_color = "black",
                         use_grid = TRUE,
                         border_color = NA
                       ),

                       # Return options
                       return_data = FALSE) {

  # Parameter validation
  if (!is.list(data_list)) {
    stop("'data_list' must be a list where each element is a vector of elements in that set")
  }

  # If sets parameter is not provided, use all names from the list
  if (is.null(sets)) {
    sets <- names(data_list)
    if (is.null(sets)) {
      sets <- paste0("Set", 1:length(data_list))
      names(data_list) <- sets
    }
  } else {
    # Validate if all specified sets exist in data_list
    if (!all(sets %in% names(data_list))) {
      stop("Not all specified sets exist in data_list")
    }
    # Filter data_list to only include specified sets
    data_list <- data_list[sets]
  }

  # Limit the number of sets to display if specified
  if (!is.null(max_sets_display) && max_sets_display < length(sets)) {
    set_sizes <- sapply(data_list, length)
    sets_to_keep <- if (sort_sets_by == "size") {
      names(sort(set_sizes, decreasing = sort_sets_decreasing)[1:max_sets_display])
    } else if (sort_sets_by == "name") {
      sort(names(set_sizes), decreasing = sort_sets_decreasing)[1:max_sets_display]
    } else {
      sets[1:max_sets_display]
    }

    data_list <- data_list[sets_to_keep]
    sets <- sets_to_keep
  }

  # Get all unique elements
  all_elements <- unique(unlist(data_list))

  # Create a binary membership matrix as a data frame
  membership_df <- data.frame(element = all_elements)

  for (set_name in sets) {
    membership_df[[set_name]] <- as.integer(membership_df$element %in% data_list[[set_name]])
  }

  # Generate all possible intersections by grouping and counting
  pattern_df <- membership_df %>%
    select(-element) %>%
    group_by_all() %>%
    summarise(size = n(), .groups = "drop")

  # Filter out empty intersection (all zeros) unless explicitly requested
  if (!show_empty_intersections) {
    pattern_df <- pattern_df %>%
      filter(rowSums(select(., -size)) > 0)
  }

  # Filter by minimum intersection size
  pattern_df <- pattern_df %>%
    filter(size >= min_intersection_size)

  if (nrow(pattern_df) == 0) {
    stop("No intersections meet the minimum size requirement")
  }

  # Add degree (number of sets in each intersection)
  pattern_df$degree <- rowSums(select(pattern_df, -size))

  # Sort intersections
  if (sort_intersections_by == "freq") {
    pattern_df <- pattern_df %>%
      arrange(if(sort_intersections_decreasing) desc(size) else size)
  } else if (sort_intersections_by == "degree") {
    pattern_df <- pattern_df %>%
      arrange(
        if(sort_intersections_decreasing) desc(degree) else degree,
        if(sort_intersections_decreasing) desc(size) else size
      )
  } else if (sort_intersections_by == "custom" && !is.null(custom_intersections_order)) {
    # Use custom order - this assumes custom_intersections_order contains pattern IDs in desired order
    pattern_df <- pattern_df[custom_intersections_order, ]
  }

  # Create a pattern ID for each intersection
  pattern_df$pattern_id <- 1:nrow(pattern_df)

  # Calculate set sizes
  set_sizes <- sapply(data_list, length)
  set_size_df <- data.frame(
    set_name = names(set_sizes),
    size = as.numeric(set_sizes)
  )

  # Sort sets based on specified order
  if (sort_sets_by == "size") {
    sets_ordered <- set_size_df %>%
      arrange(if(sort_sets_decreasing) desc(size) else size) %>%
      pull(set_name)

    set_size_df$set_name <- factor(set_size_df$set_name, levels = sets_ordered)
    sets <- sets_ordered
  } else if (sort_sets_by == "name") {
    sets_ordered <- sort(sets, decreasing = sort_sets_decreasing)
    set_size_df$set_name <- factor(set_size_df$set_name, levels = sets_ordered)
    sets <- sets_ordered
  } else if (sort_sets_by == "custom" && !is.null(custom_sets_order)) {
    # Verify custom order contains all sets
    if (!all(sets %in% custom_sets_order)) {
      warning("custom_sets_order doesn't contain all sets. Using size ordering instead.")
      sets_ordered <- set_size_df %>%
        arrange(desc(size)) %>%
        pull(set_name)
    } else {
      sets_ordered <- custom_sets_order[custom_sets_order %in% sets]
    }
    set_size_df$set_name <- factor(set_size_df$set_name, levels = sets_ordered)
    sets <- sets_ordered
  }

  # Convert set names to numeric positions for uniform positioning
  set_size_df$y_pos <- 1:nrow(set_size_df)

  # For matrix visualization, we need all combinations of sets and patterns
  # Convert pattern_df to long format for joining with matrix data
  pattern_long <- pattern_df %>%
    pivot_longer(cols = all_of(sets),
                 names_to = "set_name",
                 values_to = "is_in_set") %>%
    select(pattern_id, set_name, is_in_set, size, degree)

  # Ensure consistent factor levels for sets
  pattern_long$set_name <- factor(pattern_long$set_name, levels = sets)

  # Join with set positions
  pattern_long <- pattern_long %>%
    left_join(set_size_df %>% select(set_name, y_pos), by = "set_name")

  # Define set colors if not provided
  if (is.null(sets_bar_colors)) {
    # Default color palette for sets
    default_colors <- c(
      "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
      "#FF7F00", "#FFFF33", "#A65628", "#F781BF",
      "#999999", "#66C2A5", "#FC8D62", "#8DA0CB"
    )

    # Generate colors for all sets
    sets_bar_colors <- setNames(
      default_colors[1:min(length(sets), length(default_colors))],
      sets
    )

    # For any remaining sets beyond the default colors, recycle colors
    if (length(sets) > length(default_colors)) {
      remaining_sets <- sets[(length(default_colors) + 1):length(sets)]
      remaining_colors <- default_colors[1:length(remaining_sets)]
      additional_colors <- setNames(remaining_colors, remaining_sets)
      sets_bar_colors <- c(sets_bar_colors, additional_colors)
    }
  }

  # Create a custom position for each intersection - use exact positions to ensure alignment
  pattern_df$x_pos <- 1:nrow(pattern_df)

  # Add x_pos to pattern_long for matrix plot
  pattern_long <- pattern_long %>%
    left_join(pattern_df %>% select(pattern_id, x_pos), by = "pattern_id")

  # Get the maximum set size and intersection size for scaling
  max_set_size <- max(set_size_df$size)
  max_intersection_size <- max(pattern_df$size)

  # Set up intersection bar colors based on highlighting
  intersection_bar_colors <- rep(main_bar_color, nrow(pattern_df))
  names(intersection_bar_colors) <- pattern_df$pattern_id

  # Apply highlights if specified
  highlight_indices <- NULL
  if (!is.null(highlight_intersections)) {
    if (is.list(highlight_intersections) || is.vector(highlight_intersections)) {
      valid_highlights <- highlight_intersections[highlight_intersections %in% pattern_df$pattern_id]
      intersection_bar_colors[as.character(valid_highlights)] <- highlight_color
      highlight_indices <- valid_highlights
    }
  }

  # Create a theme based on theme_params
  custom_theme <- theme_minimal() +
    theme(
      panel.background = element_rect(fill = theme_params$background_color, color = theme_params$border_color),
      panel.grid.major = if(theme_params$use_grid) element_line(color = theme_params$grid_color) else element_blank(),
      panel.grid.minor = if(theme_params$use_grid) element_line(color = theme_params$grid_color) else element_blank(),
      axis.text = element_text(color = theme_params$axis_text_color),
      text = element_text(color = theme_params$axis_text_color)
    )

  # CRITICAL: Use identical x-axis and y-axis limits for all plots
  x_limits <- c(0.5, nrow(pattern_df) + 0.5)
  y_limits <- c(0.5, nrow(set_size_df) + 0.5)

  # Create the dot matrix plot - now with exact matching limits
  # Apply highlight colors to dots if highlight_indices is provided
  if (!is.null(highlight_indices)) {
    # Flag rows in pattern_long that belong to highlighted intersections
    pattern_long$is_highlighted <- pattern_long$pattern_id %in% highlight_indices
  } else {
    pattern_long$is_highlighted <- FALSE
  }

  # Create a dot color vector based on highlight status
  pattern_long$dot_color <- ifelse(pattern_long$is_in_set == 1,
                                   ifelse(pattern_long$is_highlighted, highlight_color, intersection_color),
                                   "white")

  # Create the matrix visualization with direct positioning
  matrix_plot <- ggplot() +
    # Points for set membership
    geom_point(
      data = pattern_long,
      aes(
        x = x_pos,
        y = y_pos,
        size = if_else(is_in_set == 1, filled_point_size, empty_point_size)
      ),
      shape = matrix_point_shape,
      fill = pattern_long$dot_color,
      color = point_outline_color,
      stroke = point_stroke
    ) +
    scale_size_identity() +
    labs(x = "", y = "") +
    custom_theme +
    theme(
      axis.text.y = if(show_set_labels) element_text(size = set_text_size, angle = text_angle) else element_blank(),
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      plot.margin = margin(t = 0, r = plot_margin, b = plot_margin, l = 0, unit = "cm"),
      panel.background = element_rect(fill = theme_params$background_color, color = NA)
    ) +
    scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
    scale_y_continuous(
      breaks = set_size_df$y_pos,
      labels = set_size_df$set_name,
      limits = y_limits,
      expand = c(0, 0)
    )

  # Add connecting lines for intersections
  # Group data by pattern_id to easily extract sets for each intersection
  grouped_data <- pattern_long %>%
    filter(is_in_set == 1) %>%
    group_by(pattern_id, x_pos, is_highlighted) %>%
    summarise(
      sets = list(y_pos),  # Store numeric positions directly
      n_sets = n(),
      .groups = "drop"
    ) %>%
    filter(n_sets > 1)  # Only keep intersections with multiple sets

  # Add lines for each intersection with multiple sets
  for (i in 1:nrow(grouped_data)) {
    # Extract y positions directly
    y_positions <- unlist(grouped_data$sets[i])

    line_data <- data.frame(
      x_pos = grouped_data$x_pos[i],
      y_pos = y_positions,
      is_highlighted = grouped_data$is_highlighted[i]
    )

    # Line color based on highlight status
    line_color <- if (line_data$is_highlighted[1]) highlight_color else intersection_color

    # Add the line
    matrix_plot <- matrix_plot +
      geom_line(
        data = line_data,
        aes(x = x_pos, y = y_pos),
        linewidth = line_size,
        color = line_color
      )
  }

  # Add a small offset to x_pos for better alignment of top bars if requested
  #pattern_df$x_pos_adjusted <- pattern_df$x_pos + bar_offset
  pattern_df$x_pos_adjusted <- pattern_df$x_pos

  # Create the intersection size bar plot (top) - using geom_rect for precise alignment with offset
  intersection_plot <- ggplot() +
    geom_rect(
      data = pattern_df,
      aes(
        xmin = x_pos_adjusted - bar_width/2,
        xmax = x_pos_adjusted + bar_width/2,
        ymin = 0,
        ymax = size,
        fill = factor(pattern_id)
      )
    ) +
    scale_fill_manual(values = setNames(intersection_bar_colors, 1:length(intersection_bar_colors))) +
    labs(x = "", y = intersection_title) +
    custom_theme +
    theme(
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      legend.position = "none",
      plot.margin = margin(t = plot_margin, r = plot_margin, b = 0, l = plot_margin, unit = "cm"),
      panel.background = element_rect(fill = theme_params$background_color, color = NA)
    ) +
    # Match x limits with matrix plot for exact alignment
    scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
    # Scale y-axis with more room for text labels
    scale_y_continuous(limits = c(0, max_intersection_size * 1.3), expand = c(0, 0))

  # Add size labels if requested
  if (intersection_size_show_values && show_numbers_on_bars) {
    # Calculate threshold for when to change text color (for visibility on dark bars)
    threshold_value <- max_intersection_size * number_color_threshold

    intersection_plot <- intersection_plot +
      geom_text(
        data = pattern_df,
        aes(
          x = x_pos_adjusted,
          y = size,
          label = size,
          color = ifelse(size > threshold_value, "on_bar", "off_bar")
        ),
        vjust = -0.5,
        size = intersection_label_size
      ) +
      scale_color_manual(values = number_colors, guide = "none")
  }

  # Add set colors to the set size data frame
  set_size_df$color <- sets_bar_colors[as.character(set_size_df$set_name)]

  # Create the set size bar plot (left) - using geom_rect for precise alignment
  set_size_plot <- ggplot() +
    geom_rect(
      data = set_size_df,
      aes(
        ymin = y_pos - bar_width/2,
        ymax = y_pos + bar_width/2,
        xmin = 0,
        xmax = size,
        fill = set_name
      )
    ) +
    labs(x = set_size_title, y = "") +
    scale_fill_manual(values = sets_bar_colors) +
    custom_theme +
    theme(
      # Hide y-axis text as it will be shown in the matrix plot
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      legend.position = "none",
      plot.margin = margin(t = 0, r = 0, b = plot_margin, l = plot_margin, unit = "cm"),
      panel.background = element_rect(fill = theme_params$background_color, color = NA)
    ) +
    # Match y limits with matrix plot for exact alignment
    scale_y_continuous(limits = y_limits, expand = c(0, 0)) +
    # Scale x-axis for left direction
    scale_x_reverse(limits = c(max_set_size * 1.2, 0), expand = c(0, 0))

  # Add size labels if requested
  if (set_size_show_values) {
    set_size_plot <- set_size_plot +
      geom_text(
        data = set_size_df,
        aes(x = size/2, y = y_pos, label = size),
        size = set_label_size
      )
  }

 # ---- Use gtable-based alignment like UpSetR ----
  # Convert ggplots to gtables for precise alignment
  intersection_gtable <- ggplot_gtable(ggplot_build(intersection_plot))
  matrix_gtable <- ggplot_gtable(ggplot_build(matrix_plot))
  set_size_gtable <- ggplot_gtable(ggplot_build(set_size_plot))

  # Synchronize widths: intersection bars should align with matrix dots
  # This is the key alignment step from UpSetR
  intersection_gtable$widths <- matrix_gtable$widths

  # Synchronize heights: matrix rows should align with set size bars
  matrix_gtable$heights <- set_size_gtable$heights

  # Calculate layout dimensions (using 100-unit grid like UpSetR)
  size_plot_height <- round((1 - height_ratio) * 100)
  matrix_bottom <- 100
  size_bar_right <- round(width_ratio * 100)
  matrix_left <- size_bar_right + 1
  matrix_right <- 100

  # Helper function for viewport layout (same as UpSetR)
  vplayout <- function(x, y) {
    viewport(layout.pos.row = x, layout.pos.col = y)
  }

  # Create the combined grob following UpSetR's approach exactly:
  # Combine intersection bars + matrix (they share widths) and draw set size separately
  main_and_matrix <- arrangeGrob(
    intersection_gtable,
    matrix_gtable,
    heights = c(1 - height_ratio, height_ratio)
  )

  # Create a custom grob that draws using viewports like UpSetR
  make_upset_grob <- function() {
    # Return a gTree that contains all the plotting instructions
    gTree(
      children = gList(),
      intersection_gtable = intersection_gtable,
      matrix_gtable = matrix_gtable,
      set_size_gtable = set_size_gtable,
      main_and_matrix = main_and_matrix,
      size_plot_height = size_plot_height,
      size_bar_right = size_bar_right,
      matrix_left = matrix_left,
      matrix_right = matrix_right,
      matrix_bottom = matrix_bottom,
      height_ratio = height_ratio,
      cl = "upset_grob"
    )
  }

  combined_grob <- make_upset_grob()

  # Return just the plot or the full data based on return_data parameter
  if (return_data) {
    return(list(
      plot = combined_grob,
      intersection_plot = intersection_plot,
      matrix_plot = matrix_plot,
      set_size_plot = set_size_plot,
      intersection_gtable = intersection_gtable,
      matrix_gtable = matrix_gtable,
      set_size_gtable = set_size_gtable,
      intersection_data = pattern_df,
      set_sizes = set_size_df,
      set_colors = sets_bar_colors
    ))
  } else {
    return(combined_grob)
  }
}

#' @title Draw method for upset_grob objects
#' @description Internal method to draw upset_grob using grid viewports
#' @param x An upset_grob object
#' @param recording Logical for recording
#' @return Draws the upset plot
#' @export
#' @importFrom grid grid.draw pushViewport popViewport viewport grid.layout gTree gList makeContent
#' @importFrom gridExtra arrangeGrob
#' @keywords internal
makeContent.upset_grob <- function(x) {
  # This is called when the grob is drawn
  # We don't modify content here, drawing is handled by grid.draw
  x
}

#' Print method for upset_grob objects
#' @param x An upset_grob object
#' @param ... Additional arguments (ignored)
#' @return Invisibly returns the input object
#' @export
#' @importFrom grid grid.draw grid.newpage pushViewport popViewport viewport grid.layout
#' @importFrom gridExtra arrangeGrob
print.upset_grob <- function(x, ...) {
  grid.newpage()

  # Set up the grid layout (100x100 like UpSetR)
  pushViewport(viewport(layout = grid.layout(100, 100)))

  # Helper for viewport layout
  vplayout <- function(row, col) {
    viewport(layout.pos.row = row, layout.pos.col = col)
  }

  # Draw main bar + matrix (right side) using arrangeGrob for vertical stacking
  vp <- vplayout(1:x$matrix_bottom, x$matrix_left:x$matrix_right)
  pushViewport(vp)
  grid.draw(x$main_and_matrix)
  popViewport()

  # Draw set size bars (bottom left)
  vp <- vplayout((x$size_plot_height + 1):x$matrix_bottom, 1:x$size_bar_right)
  pushViewport(vp)
  grid.draw(x$set_size_gtable)
  popViewport()

  popViewport()  # Pop the main layout viewport

  invisible(x)
}

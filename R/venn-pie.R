#' @title Create a Venn-pie visualization
#' @description Creates a pie-chart-like visualization of set intersections,
#'   which is particularly useful for visualizing more than 5 sets
#' @param object A Venn object
#' @param subset Character vector of subset names to highlight
#' @param top Maximum number of subsets to display. Default: 31
#' @param min Minimum number of sets an element must be in. Default: 0
#' @param color Optional vector of colors for the subsets
#' @param revcolor Color for non-highlighted subsets. Default: "lightgrey"
#' @param any Highlight subsets shared by exactly this many sets
#' @param show.number Logical: show counts in labels? Default: TRUE
#' @param show.x Logical: show subset labels? Default: TRUE
#' @param sep Character separator for subset names
#' @param log Logical: use log scale for counts? Default: FALSE
#' @param base Base for log transformation if log=TRUE
#' @param percentage Logical: show percentages instead of counts? Default: FALSE
#' @param title Optional plot title
#' @param interactive Logical: create interactive plot? Default: FALSE
#' @param ... Additional arguments
#' @import ggplot2
#' @importFrom dplyr filter select
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#' @importFrom plotly ggplotly layout plot_ly
#' @importFrom methods is slot
#' @importFrom grDevices colorRampPalette
#' @author Kai Guo
#' @return A ggplot2 or plotly object
#' @export
setMethod("vennpie", signature = (object="Venn"), function(object,
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
                                                           title = NULL,
                                                           interactive = FALSE,
                                                           ...) {
  # Filter data based on minimum set membership
  result_wide <- result(object, wide = TRUE)
  filtered_details <- result_wide %>%
    filter(SharedSets >= min) %>%
    select(Detail)

  element_details <- as.vector(filtered_details[, 1])
  result_long <- result(object)
  filtered_subsets <- result_long %>%
    filter(Detail %in% element_details) %>%
    select(Subset)

  unique_subsets <- unique(as.vector(filtered_subsets[, 1]))

  # Get subset details and counts
  subset_counts <- object@detail
  subset_counts <- subset_counts[unique_subsets]

  # Limit to top subsets if needed
  if (length(subset_counts) > top) {
    top_subsets <- names(sort(subset_counts, decreasing = TRUE))[1:top]
    subset_counts <- subset_counts[top_subsets]
  }

  # Get group names and raw counts
  group_names <- object@GroupNames
  raw_counts <- object@raw

  # Calculate number of sets each subset belongs to
  sets_per_subset <- sapply(names(subset_counts), function(name) {
    if (name == "Shared") {
      return(length(group_names))
    } else {
      return(length(strsplit(name, sep)[[1]]))
    }
  })
  names(sets_per_subset) <- names(subset_counts)

  # Prepare data for visualization
  # Handle "Shared" subset special case
  shared_idx <- which(names(subset_counts) == "Shared")

  # Prepare data frame for plotting
  plot_data <- data.frame()

  # Process the "Shared" subset if it exists
  if (length(shared_idx) > 0) {
    shared_value <- subset_counts["Shared"]
    # For "Shared", add a row for each group
    for (group in group_names) {
      plot_data <- rbind(plot_data, data.frame(
        name = "Shared",
        group = group,
        val = shared_value,
        stringsAsFactors = FALSE
      ))
    }
  }

  # Process other subsets
  other_subsets <- setdiff(names(subset_counts), "Shared")
  for (subset in other_subsets) {
    subset_value <- subset_counts[subset]
    subset_groups <- strsplit(subset, sep)[[1]]

    # Add a row for each group in this subset
    for (group in subset_groups) {
      plot_data <- rbind(plot_data, data.frame(
        name = subset,
        group = group,
        val = subset_value,
        stringsAsFactors = FALSE
      ))
    }
  }

  # Apply log transformation if requested
  if (isTRUE(log)) {
    if (!is.null(base)) {
      plot_data$val <- round(log(plot_data$val, base = base), 2)
    } else {
      plot_data$val <- round(log2(plot_data$val), 2)
    }
  }

  # Create set labels
  if (show.number) {
    plot_data$sets <- paste(plot_data$name, plot_data$val, sep = ": ")
  } else {
    plot_data$sets <- plot_data$name
  }

  # Convert group to factor to preserve order
  plot_data$group <- factor(plot_data$group, levels = names(raw_counts))

  # Generate colors for subsets
  if (is.null(color)) {
    color <- setcolor(length(subset_counts))
    names(color) <- names(subset_counts)
  } else if (!is.null(names(color))) {
    # Use provided named colors, generate for missing ones
    missing_colors <- setdiff(names(subset_counts), names(color))
    if (length(missing_colors) > 0) {
      extra_colors <- setcolor(length(missing_colors))
      names(extra_colors) <- missing_colors
      color <- c(color, extra_colors)
    }
  } else if (length(color) < length(subset_counts)) {
    # Generate additional colors if needed
    color <- c(color, setcolor(length(subset_counts) - length(color)))
    names(color) <- names(subset_counts)
  } else {
    names(color) <- names(subset_counts)
  }

  # Handle subset highlighting
  if (!is.null(any)) {
    # Highlight subsets with exactly 'any' sets
    highlight_subsets <- names(sets_per_subset)[sets_per_subset == any]
  } else {
    highlight_subsets <- character(0)
  }

  if (!is.null(subset)) {
    # Highlight specified subsets
    color[setdiff(names(color), c(subset, highlight_subsets))] <- revcolor
  } else if (length(highlight_subsets) > 0) {
    # Highlight based on 'any' parameter
    color[setdiff(names(color), highlight_subsets)] <- revcolor
  }

  # Prepare the colors for plotting
  if (show.number) {
    names(color) <- paste(names(color), subset_counts[names(color)], sep = ": ")
  }

  # Create the ggplot object
  p <- ggplot(plot_data, aes(x = group, y = val, fill = sets)) +
    theme_light(base_size = 15)

  # Add bars with appropriate positioning
  if (isTRUE(percentage)) {
    p <- p + geom_bar(stat = "identity", position = "fill", alpha = 0.8)
  } else {
    p <- p + geom_bar(stat = "identity", alpha = 0.8)

    # Calculate y-axis breaks based on data range
    y_breaks <- as.integer(quantile(c(0, max(raw_counts)), probs = seq(0, 1, 0.25)))
    p <- p + scale_y_continuous(breaks = y_breaks)
  }

  # Add colors
  p <- p + scale_fill_manual(values = color)

  # Convert to polar coordinates for the pie-like effect
  p <- p + coord_polar("y", start = 0)

  # Set labels
  p <- p + labs(fill = "Subsets")

  # Create group labels with counts
  group_labels <- paste(names(raw_counts), raw_counts, sep = ": ")
  group_colors <- rep("white", length(group_labels))

  # Add group colors and labels
  p <- p + scale_color_manual(breaks = names(raw_counts),
                              labels = group_labels,
                              values = group_colors) +
    labs(color = "Input Groups") +
    guides(
      fill = guide_legend(order = 0),
      color = guide_legend(reverse = TRUE, order = 1)
    )

  # Clean up theme elements
  p <- p + theme(
    axis.title = element_blank(),
    axis.line = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank()
  )

  # Hide x-axis text if not showing labels
  if (!show.x) {
    p <- p + theme(axis.text.x = element_blank())
  }

  # Add title if provided
  if (!is.null(title)) {
    p <- p + ggtitle(title) +
      theme(plot.title = element_text(hjust = 0.5))
  }

  # Create interactive visualization if requested
  if (isTRUE(interactive)) {
    if (!requireNamespace("plotly", quietly = TRUE)) {
      warning("Package 'plotly' is required for interactive plots. Using static plot instead.")
      return(p)
    }

    # Use the custom function that creates a simpler visualization
    return(create_interactive_vennpie(
      object = object,
      subset = subset,
      any = any,
      color = color,
      revcolor = revcolor,
      title = title
    ))
  }

  return(p)
})

#' @title Create an interactive vennpie chart with plotly
#' @description Creates a simple pie chart visualization for interactive exploration of set intersections
#' @param object A Venn object
#' @param subset Character vector of subset names to highlight
#' @param any Highlight subsets shared by exactly this many sets
#' @param color Optional vector of colors for the subsets
#' @param revcolor Color for non-highlighted subsets
#' @param title Optional plot title
#' @return A plotly object
#' @importFrom plotly plot_ly layout add_annotations
#' @author Kai Guo
#' @export
create_interactive_vennpie <- function(object, subset = NULL, any = NULL,
                                       color = NULL, revcolor = "lightgrey",
                                       title = NULL) {
  # Add debugging
  message("Starting create_interactive_vennpie")

  # Try-catch to handle errors
  tryCatch({
    # Extract data from Venn object
    subset_counts <- object@detail
    message("Extracted subset counts")

    # Generate colors for subsets if not provided
    if (is.null(color)) {
      color <- setcolor(length(subset_counts))
      names(color) <- names(subset_counts)
    }

    # Calculate number of sets each subset belongs to
    group_names <- object@GroupNames
    sets_per_subset <- sapply(names(subset_counts), function(name) {
      if (name == "Shared") {
        return(length(group_names))
      } else {
        return(length(strsplit(name, object@sep)[[1]]))
      }
    })
    names(sets_per_subset) <- names(subset_counts)

    # Handle subset highlighting
    if (!is.null(any)) {
      # Highlight subsets with exactly 'any' sets
      highlight_subsets <- names(sets_per_subset)[sets_per_subset == any]
    } else {
      highlight_subsets <- character(0)
    }

    if (!is.null(subset)) {
      # Highlight specified subsets
      color[setdiff(names(color), c(subset, highlight_subsets))] <- revcolor
    } else if (length(highlight_subsets) > 0) {
      # Highlight based on 'any' parameter
      color[setdiff(names(color), highlight_subsets)] <- revcolor
    }

    message("Creating plotly pie chart")
    # Create a simple pie chart
    p <- plotly::plot_ly(
      labels = names(subset_counts),
      values = as.numeric(subset_counts),
      type = 'pie',
      textinfo = 'label+value',
      insidetextorientation = 'radial',
      hoverinfo = 'text',
      text = paste(names(subset_counts), ": ", subset_counts),
      marker = list(
        colors = unname(color),
        line = list(color = 'white', width = 1)
      )
    )

    # Add layout
    p <- plotly::layout(p,
                        title = if(!is.null(title)) list(text = title, x = 0.5) else NULL,
                        showlegend = TRUE
    )

    message("Created plotly object, returning")
    return(p)

  }, error = function(e) {
    # Handle error and provide informative message
    message("Error in create_interactive_vennpie: ", e$message)

    # Return a simple fallback plot on error
    message("Returning fallback plot")
    fallback_plot <- plotly::plot_ly() %>%
      plotly::add_annotations(
        text = paste("Error creating VennPie visualization:", e$message),
        x = 0.5, y = 0.5,
        showarrow = FALSE,
        font = list(size = 14, color = "red")
      )

    return(fallback_plot)
  })
}

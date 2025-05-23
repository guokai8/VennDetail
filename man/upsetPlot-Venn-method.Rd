% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uset-plot.R
\name{upsetPlot,Venn-method}
\alias{upsetPlot,Venn-method}
\title{Create an UpSet plot for a Venn object}
\usage{
\S4method{upsetPlot}{Venn}(
  object,
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
  number_colors = c(on_bar = "black", off_bar = "black"),
  theme_params = list(background_color = "white", grid_color = "grey92", axis_text_color
    = "black", use_grid = TRUE, border_color = NA),
  title = NULL,
  interactive = FALSE,
  return_data = FALSE
)
}
\arguments{
\item{object}{A Venn object}

\item{nintersects}{Maximum number of intersections to show (default: 40)}

\item{min_size}{Minimum intersection size to include (default: 1)}

\item{sets_bar_color}{Colors for the set size bars (default: NULL for auto-generate)}

\item{main_bar_color}{Color for the intersection size bars (default: "steelblue")}

\item{point_size}{Size of points in the matrix (default: 3)}

\item{line_size}{Width of lines in the matrix (default: 1)}

\item{show_numbers}{Logical: show counts on bars? (default: TRUE)}

\item{sort_intersections_by}{How to sort intersections: "freq" (default), "degree"}

\item{sort_sets_by}{How to sort sets: "size" (default), "name"}

\item{sort_sets_decreasing}{Whether to sort sets in decreasing order (default: TRUE)}

\item{custom_sets_order}{Custom order for sets if sort_sets_by="custom"}

\item{sort_intersections_decreasing}{Whether to sort intersections in decreasing order (default: TRUE)}

\item{custom_intersections_order}{Custom order for intersections if sort_intersections_by="custom"}

\item{intersection_color}{Color for intersection dots and lines (default: "black")}

\item{highlight_intersections}{Vector of intersection IDs to highlight (default: NULL)}

\item{highlight_color}{Color for highlighted intersections (default: "darkorange")}

\item{empty_point_size}{Size of empty points in the matrix (default: 1.5)}

\item{bar_width}{Width of bars (0-1 scale) (default: 0.7)}

\item{text_angle}{Angle for text labels (default: 0)}

\item{text_size}{Size of text in the plot (default: 10)}

\item{set_label_size}{Size of set size labels (default: 3)}

\item{intersection_label_size}{Size of intersection size labels (default: 3)}

\item{point_outline_color}{Color for the outline of points (default: "black")}

\item{point_stroke}{Width of point outline (default: 0.3)}

\item{set_size_show_values}{Whether to show set size values (default: TRUE)}

\item{intersection_size_show_values}{Whether to show intersection size values (default: TRUE)}

\item{show_empty_intersections}{Whether to show empty intersections (default: FALSE)}

\item{show_set_labels}{Whether to show set labels (default: TRUE)}

\item{plot_margin}{Margin around the plots in cm (default: 0.5)}

\item{height_ratio}{Ratio of matrix to total height (default: 0.7)}

\item{width_ratio}{Ratio of set size to total width (default: 0.3)}

\item{bar_offset}{Horizontal offset for top bars to improve alignment (default: 0)}

\item{set_text_size}{Size of set labels (default: 10)}

\item{intersection_title}{Title for the intersection size plot (default: "Intersection Size")}

\item{set_size_title}{Title for the set size plot (default: "Set Size")}

\item{matrix_point_shape}{Shape of the dots in the matrix (21=filled circle) (default: 21)}

\item{number_color_threshold}{Fraction of max value where number color switches (default: 0.75)}

\item{number_colors}{Named vector with colors for labels on/off bars (default: c(on_bar="white", off_bar="black"))}

\item{theme_params}{List of theme parameters for customization (default: list of defaults)}

\item{title}{Optional plot title}

\item{interactive}{Logical: create interactive plot? (default: FALSE)}

\item{return_data}{Whether to return the data along with the plot (default: FALSE)}
}
\value{
A ggplot object or a combined grid layout
}
\description{
Creates an UpSet plot to visualize set intersections
}
\examples{
# Basic example
sets <- list(
  "Set A" = c(1:100),
  "Set B" = c(30:120),
  "Set C" = c(20:50, 90:110),
  "Set D" = c(10:40, 80:120)
)
ven <- venndetail(sets)
upsetPlot(ven, bar_offset = -0.02)

# With highlighting
upsetPlot(ven,
          highlight_intersections = c(1, 2),
          highlight_color = "darkorange",
          bar_offset = -0.02)

}
\author{
Kai Guo
}

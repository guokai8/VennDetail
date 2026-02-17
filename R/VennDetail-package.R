#' VennDetail: Comprehensive Visualization and Analysis of Multi-Set Intersections
#'
#' @description
#' A comprehensive package for visualizing multi-set intersections and extracting
#' detailed subset information. VennDetail generates high-resolution visualizations including
#' traditional Venn diagrams, Venn-pie plots, and UpSet-style plots. It provides functions to
#' extract and combine subset details with user datasets in various formats.
#'
#' @details
#' The VennDetail package offers several powerful visualization and analysis tools:
#'
#' \strong{Visualization methods:}
#' \itemize{
#'   \item Traditional Venn diagram (for 2-5 sets)
#'   \item VennPie visualization (useful for more than 5 sets)
#'   \item UpSet plot (matrix-based visualization)
#'   \item Bar plot (simple visualization of subset sizes)
#' }
#'
#' \strong{Key features:}
#' \itemize{
#'   \item Extraction of elements in any subset combination
#'   \item Combining subset information with user-supplied data frames
#'   \item Statistical analysis of set intersections
#'   \item Enrichment analysis for set members
#'   \item Interactive visualizations
#'   \item High-resolution figure export
#'   \item Shiny app for interactive exploration
#' }
#'
#' @section Getting Started:
#'
#' To create a Venn object for analysis:
#'
#' ```
#' # Create sample datasets
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#'
#' # Create a Venn object
#' res <- venndetail(list(A = A, B = B, C = C))
#' ```
#'
#' @section Visualization:
#'
#' ```
#' # Traditional Venn diagram
#' vennDiagram(res)
#'
#' # VennPie visualization
#' vennpie(res)
#'
#' # UpSet plot
#' upsetPlot(res)
#'
#' # Bar plot
#' dplot(res, order = TRUE)
#'
#' # Generic plot function with type selection
#' plot(res, type = "venn")
#' ```
#'
#' @section Data Extraction:
#'
#' ```
#' # Extract elements shared by all sets
#' shared <- getSet(res, "Shared")
#'
#' # Extract elements unique to set A
#' unique_to_A <- getSet(res, "A")
#' ```
#'
#' @section Statistical Analysis:
#'
#' ```
#' # Test for significance of overlaps
#' stats <- vennStats(res)
#' ```
#'
#' @author Kai Guo, Brett McGregor
#' @keywords package
"_PACKAGE"

#' @title T2DM Dataset
#' @description T2DM data are differential expression genes (DEGs) with
#'   annotation from the publication by Hinder et al.
#' @format A list of data frames with five columns each
"T2DM"

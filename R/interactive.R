#' @title Create an interactive Venn diagram app
#' @description Creates a Shiny app for interactive exploration of Venn diagrams
#' @param object A Venn object
#' @param launch Launch the app immediately? Default: TRUE
#' @param ... Additional arguments passed to shiny::runApp
#' @return A Shiny app object (invisibly)
#' @importFrom shiny shinyApp fluidPage titlePanel sidebarLayout sidebarPanel mainPanel
#'   selectInput numericInput checkboxInput conditionalPanel selectizeInput hr
#'   downloadButton tabsetPanel tabPanel plotOutput renderPlot reactive renderPrint
#'   downloadHandler showNotification verbatimTextOutput runApp uiOutput renderUI
#' @importFrom plotly plotlyOutput renderPlotly ggplotly layout export
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom ggplot2 ggsave
#' @importFrom methods is
#' @importFrom utils write.csv browseURL
#' @importFrom htmlwidgets saveWidget
#' @export
#' @examples
#' \dontrun{
#' # Create a Venn object
#' A <- sample(1:100, 40, replace = FALSE)
#' B <- sample(1:100, 60, replace = FALSE)
#' C <- sample(1:100, 40, replace = FALSE)
#' res <- venndetail(list(A = A, B = B, C = C))
#'
#' # Launch interactive app
#' vennApp(res)
#' }
#' @author Kai Guo
vennApp <- function(object, launch = TRUE, ...) {
  # Check if shiny is available
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required for interactive apps")
  }

  if (!requireNamespace("DT", quietly = TRUE)) {
    stop("Package 'DT' is required for interactive apps")
  }

  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Package 'plotly' is required for interactive apps")
  }

  # Create the app
  app <- shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::titlePanel("VennDetail Interactive Explorer"),

      shiny::sidebarLayout(
        shiny::sidebarPanel(
          shiny::selectInput("plotType", "Plot Type:",
                             choices = c("Traditional Venn" = "venn",
                                         "VennPie" = "vennpie",
                                         "UpSet Plot" = "upset",
                                         "Bar Plot" = "bar")),

          shiny::conditionalPanel(
            condition = "input.plotType == 'vennpie'",
            shiny::numericInput("min", "Minimum Sets:", 0,
                                min = 0, max = length(object@GroupNames)),
            shiny::checkboxInput("log", "Use Log Scale", FALSE),
            shiny::selectInput("highlight", "Highlight Subsets:",
                               choices = c("None", "Any 1 Set" = "1",
                                           "Any 2 Sets" = "2",
                                           "Any 3 Sets" = "3",
                                           "Custom")),
            shiny::conditionalPanel(
              condition = "input.highlight == 'Custom'",
              shiny::selectizeInput("customSubsets", "Select Subsets:",
                                    choices = names(object@detail),
                                    multiple = TRUE)
            )
          ),

          shiny::conditionalPanel(
            condition = "input.plotType == 'upset'",
            shiny::numericInput("nintersects", "Number of Intersections:", 20,
                                min = 1, max = 50)
          ),

          shiny::conditionalPanel(
            condition = "input.plotType == 'bar'",
            shiny::checkboxInput("order", "Order by Size", TRUE)
          ),

          shiny::checkboxInput("interactive", "Interactive Plot", FALSE),

          shiny::hr(),

          shiny::selectInput("subset", "Subset to View:",
                             choices = c("All", names(object@detail))),

          shiny::downloadButton("downloadPlot", "Download Plot"),
          shiny::downloadButton("downloadData", "Download Data")
        ),

        shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel("Plot",
                            shiny::uiOutput("plotOutput")),
            shiny::tabPanel("Data", DT::DTOutput("subsetTable")),
            shiny::tabPanel("Statistics", DT::DTOutput("statsTable")),
            shiny::tabPanel("Summary",
                            shiny::verbatimTextOutput("summaryText"),
                            shiny::plotOutput("barPlot", height = "400px"))
          )
        )
      )
    ),

    server = function(input, output, session) {
      # Dynamic UI for plot output based on interactive mode
      output$plotOutput <- shiny::renderUI({
        if (input$interactive) {
          plotly::plotlyOutput("interactivePlot", height = "600px")
        } else {
          shiny::plotOutput("staticPlot", height = "600px")
        }
      })

      # Create plot
      plotObject <- shiny::reactive({
        # Get plot type
        type <- input$plotType

        # Get subset highlighting settings
        highlight <- NULL
        if (input$plotType == "vennpie") {
          if (input$highlight == "Custom" && !is.null(input$customSubsets)) {
            highlight <- input$customSubsets
          } else if (input$highlight %in% c("1", "2", "3")) {
            highlight <- as.numeric(input$highlight)
          }
        }

        # Create the plot based on type
        # Always set interactive=TRUE when requested to generate a proper interactive plot
        switch(type,
               venn = vennDiagram(object, interactive = input$interactive),
               vennpie = vennpie(object, min = input$min, log = input$log,
                                 any = highlight, subset = if(highlight == "Custom") input$customSubsets else NULL,
                                 interactive = input$interactive),
               upset = upsetPlot(object, nintersects = input$nintersects,
                                 interactive = input$interactive),
               bar = dplot(object, order = input$order)
        )
      })

      # Render static plot
      output$staticPlot <- shiny::renderPlot({
        if (!input$interactive) {
          plotObject()
        }
      })

      # Render interactive plot - now properly handles all plot types
      output$interactivePlot <- plotly::renderPlotly({
        if (input$interactive) {
          # No need for conversion - plot functions now return plotly objects directly when interactive=TRUE
          tryCatch({
            p <- plotObject()

            if (inherits(p, "plotly")) {
              return(p)
            } else if (inherits(p, "ggplot")) {
              return(plotly::ggplotly(p))
            } else {
              # Return a placeholder if we couldn't create an interactive plot
              shiny::showNotification("This plot type cannot be made interactive", type = "warning")
              return(plotly::plot_ly() %>%
                       plotly::add_annotations(text = "Interactive plot not available for this visualization",
                                               x = 0.5, y = 0.5, showarrow = FALSE,
                                               font = list(size = 16)))
            }
          }, error = function(e) {
            shiny::showNotification(paste("Error creating interactive plot:", e$message), type = "error")
            return(plotly::plot_ly() %>%
                     plotly::add_annotations(text = paste("Error:", e$message),
                                             x = 0.5, y = 0.5, showarrow = FALSE,
                                             font = list(size = 14, color = "red")))
          })
        }
      })

      # Subset data
      subsetData <- shiny::reactive({
        if (input$subset == "All") {
          result(object)
        } else {
          getSet(object, subset = input$subset)
        }
      })

      # Render data table
      output$subsetTable <- DT::renderDT({
        DT::datatable(subsetData(),
                      options = list(pageLength = 10, scrollX = TRUE),
                      filter = "top", rownames = FALSE)
      })

      # Calculate statistics
      statsData <- shiny::reactive({
        tryCatch({
          vennStats(object)
        }, error = function(e) {
          shiny::showNotification(paste("Error calculating statistics:", e$message),
                                  type = "error")
          data.frame(Error = "Failed to calculate statistics")
        })
      })

      # Render statistics table
      output$statsTable <- DT::renderDT({
        DT::datatable(statsData(),
                      options = list(pageLength = 10, scrollX = TRUE),
                      filter = "top", rownames = FALSE)
      })

      # Render summary text
      output$summaryText <- shiny::renderPrint({
        summary(object)
      })

      # Render bar plot for summary
      output$barPlot <- shiny::renderPlot({
        dplot(object, order = TRUE)
      })

      # Download handler for plot - handle both interactive and static plots
      output$downloadPlot <- shiny::downloadHandler(
        filename = function() {
          ext <- if(input$interactive) "html" else "png"
          paste("venndetail-plot-", Sys.Date(), ".", ext, sep = "")
        },
        content = function(file) {
          if(input$interactive && requireNamespace("htmlwidgets", quietly = TRUE)) {
            # For interactive plots, save as HTML
            p <- plotObject()
            if(inherits(p, "plotly")) {
              htmlwidgets::saveWidget(p, file)
            } else {
              # Fallback to static if not a plotly object
              ggsave(p, filename = file, width = 10, height = 8, dpi = 300)
            }
          } else {
            # For static plots
            p <- plotObject()
            if(inherits(p, "plotly")) {
              # Convert plotly to static image
              temp_file <- tempfile(fileext = ".png")
              plotly::export(p, file = temp_file)
              file.copy(temp_file, file)
              unlink(temp_file)
            } else {
              ggsave(p, filename = file, width = 10, height = 8, dpi = 300)
            }
          }
        },
        contentType = function() {
          if(input$interactive) "text/html" else "image/png"
        }
      )

      # Download handler for data
      output$downloadData <- shiny::downloadHandler(
        filename = function() {
          paste("venndetail-data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
          write.csv(subsetData(), file, row.names = FALSE)
        }
      )
    }
  )

  # Launch the app if requested
  if (launch) {
    shiny::runApp(app, ...)
  }

  # Return the app object invisibly
  invisible(app)
}

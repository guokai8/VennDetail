library(shinythemes)
library(shiny)
shinyUI(
  fluidPage(theme=shinytheme("lumen"),
            titlePanel("VennDetail Shiny App"),
            tabsetPanel(id="tabs",
              tabPanel("Upload file",
                       column(12,
                              sidebarLayout(
                                  sidebarPanel(
                                    fluidRow(
                                      column(4,aligh="left",
                                             textInput("filename1","Name",value="File1")),
                                      column(7,align = "center",
                                      fileInput(
                                      'file1',
                                      label = "Input file1",
                                      multiple = FALSE
                                    )
                                    )
                                   #width=8
                                    ),
                                      fluidRow(column(4,aligh="left",
                                                      textInput("filename2","Name",value="File2")),
                                               column(7,align = "center",
                                                      fileInput(
                                                        'file2',
                                                        label = "Input file2",
                                                        multiple = FALSE
                                                      )
                                      #width=4
                                      )),
                                        fluidRow(column(4,aligh="left",
                                                        textInput("filename3","Name",value="File3")),
                                                 column(7,align = "center",
                                                        fileInput(
                                                          'file3',
                                                          label = "Input file3",
                                                          multiple = FALSE
                                                        )
                                        #width=4
                                        )),
                                          fluidRow(column(4,aligh="left",
                                                          textInput("filename4","Name",value="File4")),
                                                   column(7,align = "center",
                                                          fileInput(
                                                            'file4',
                                                            label = "Input file4",
                                                            multiple = FALSE
                                                          ))
                                          ),
                                            fluidRow(column(4,aligh="left",
                                                            textInput("filename5","Name",value="File5")),
                                                     column(7,align = "center",
                                                            fileInput(
                                                              'file5',
                                                              label = "Input file5",
                                                              multiple = FALSE
                                                            )
                                            )),
                                              fluidRow(column(4,aligh="left",
                                                              textInput("filename6","Name",value="File6")),
                                                       column(7,align = "center",
                                                              fileInput(
                                                                'file6',
                                                                label = "Input file6",
                                                                multiple = FALSE
                                                              ))
                                              ),
                                   fluidRow(column(10,radioButtons(
                                     'sep',
                                     label = 'Separator',
                                     choices = c(
                                       Comma = ',',
                                       Tab = '\t'
                                     ),inline = TRUE,
                                     selected = '\t'
                                   ))),
                                    fluidRow(tags$button(id="plotx", 
                                                         type="button", 
                                                         class="btn action-button btn-large btn-primary", 
                                                         HTML('<i class="icon-star"></i>Plot!'))
                                    ),
                                   width= 4

),
mainPanel(uiOutput('mytabs')
          )
)
)),
              tabPanel("Manual input",
                       column(12,
                              sidebarLayout(
                                fluidRow(
                                  mainPanel(
                                    tags$h3(HTML("<u>Instructions</u>")),
                                    p(
                                      "To see how the list format works copy and paste each list of letters into their respective input boxes."
                                    ),
                                    #h3(textOutput("text")),
                                    p("List 1: A, B, C, D, E, F, G, H"),
                                    p("List 2: A, B, D, F, I, J, K, L"),
                                    p("List 3: D, E, J, M, N, O, P, Q"),
                                    p("List 4: D, B, P, R, S, T, U","V"),
                                    width = 12
                                    )
                                  ),
                                fluidRow(
                                  sidebarPanel(tags$style(type = "text/css", "textarea {width:100%}"),
                                               fluidRow(
                                                 column(6,
                                                        tags$textarea(
                                                          id = 'name1',
                                                          placeholder = "List 1",
                                                          rows = 1
                                                        ),
                                                        tags$style(type = "text/css", "textarea {width:100%}"),
                                                        tags$textarea(
                                                          id = 'list1',
                                                          placeholder = '',
                                                          rows = 3
                                                        ),
                                                        tags$textarea(
                                                          id = 'name3',
                                                          placeholder = "List 3",
                                                          rows = 1
                                                        ),
                                                        tags$style(type = "text/css", "textarea {width:100%}"),
                                                        tags$textarea(
                                                          id = 'list3',
                                                          placeholder = '',
                                                          rows = 3
                                                        ),
                                                        tags$textarea(
                                                          id = 'name5',
                                                          placeholder = "List 5",
                                                          rows = 1
                                                        ),
                                                        tags$style(type = "text/css", "textarea {width:100%}"),
                                                        tags$textarea(
                                                          id = 'list5',
                                                          placeholder = '',
                                                          rows = 3
                                                        )),
                                                 column(6,
                                                        tags$textarea(
                                                          id = 'name2',
                                                          placeholder = "List 2",
                                                          rows = 1
                                                        ),
                                                        tags$style(type = "text/css", "textarea {width:100%}"),
                                                        tags$textarea(
                                                          id = 'list2',
                                                          placeholder = '',
                                                          rows = 3
                                                        ),
                                                        tags$textarea(
                                                          id = 'name4',
                                                          placeholder = "List 4",
                                                          rows = 1
                                                        ),
                                                        tags$style(type = "text/css", "textarea {width:100%}"),
                                                        tags$textarea(
                                                          id = 'list4',
                                                          placeholder = '',
                                                          rows = 3
                                                        ),
                                                        tags$textarea(
                                                          id = 'name6',
                                                          placeholder = "List 6",
                                                          rows = 1
                                                        ),
                                                        tags$style(type = "text/css", "textarea {width:100%}"),
                                                        tags$textarea(
                                                          id = 'list6',
                                                          placeholder = '',
                                                          rows = 3
                                                        )
                                                 )),
                                               
                                               fluidRow(tags$button(id="ploty", 
                                                                    type="button", 
                                                                    class="btn action-button btn-large btn-primary", 
                                                                    HTML('<i class="icon-star"></i>Plot!'))
                                               ),
                                               width= 10)
                                )))),
              tabPanel("Plot",
                       column(12,
                              sidebarLayout(
                                sidebarPanel(
                                  fluidRow(
                                    uiOutput("select"),
                                    width=4
                                  ),
                                  fluidRow(selectInput("type",label="Display type",choices = c("venn","vennpie","upset"),selected = "venn"),width=4),
                                  fluidRow(
                                    uiOutput("dyna"),
                                    width=4
                                  ),
                                  fluidRow(column(6,offset = 0,style='padding:0px;',
                                                  textInput("height","Height",value="8")),
                                           column(6,offset=0,style='padding:0px;',
                                                  textInput("width","Width",value="10"))
                                  ),
                                  fluidRow(
                                    textInput("plotfile","Filename: ",value = ""),width=4
                                  ),
                                
                                  fluidRow(radioButtons(
                                    'ftype',
                                    label = 'Figure type',
                                    choices = c(
                                      pdf = 'pdf',
                                      tiff="tiff",
                                      png = 'png',
                                      jpeg='jpeg'
                                    ),inline = TRUE,
                                    selected = 'pdf'
                                  ),width=4),
                                  br(),
                                  downloadButton('Download_plot', label = "Download plot")

                  ),
                                #mainPanel(plotOutput("plot"))
                                mainPanel(uiOutput("plot"))
                              )
            )),
tabPanel("Detail",
         column(12,
                sidebarLayout(
                  sidebarPanel(
                    fluidRow(
                      uiOutput("chose"),
                      width=4
                    ),
                   # fluidRow(textInput("delim",label="Group separate",value = "_")),
                    br(),
                   fluidRow(textInput("filename","Filename: ",value = ""),width=4),
                   fluidRow(radioButtons(
                     'fftype',
                     label = 'File type',
                     choices = c(
                       txt = 'txt',
                       csv='csv'),inline = TRUE,
                     selected = 'txt'
                   )),
                    downloadButton('Download_data', label = "Download Data"),
                   # fluidRow(,
                    width=4
                  ),
                 
                  #mainPanel(uiOutput('mytabs'))
                  mainPanel(DT::dataTableOutput("table"))
                )
         ))
)
))

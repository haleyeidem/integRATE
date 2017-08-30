# dependencies #################################################################

library(shinythemes)
library(shinydashboard)
library(DT)
library(dplyr)
library(plyr)
library(data.table)
library(shinyFiles)

# server #######################################################################

server <- function(input, output) {

  meta <- reactive({
    inFile <- input$files
    if (is.null(inFile)) {
      return(NULL)
    }
    inFile
  })

  output$metadata <- DT::renderDataTable(meta(),
                                         extensions = list('FixedHeader' = NULL
                                                           ),
                                         options = list(scrollY = 200,
                                                        autoWidth=TRUE,
                                                        dom = "t"
                                                        ),
                                         rownames = FALSE)

  dat <- reactive({
    inFile <- input$files
    if (is.null(inFile)) {
      return(NULL)
    }
    file_names <- as.list(inFile$datapath)
    files <- lapply(file_names, fread, header=TRUE)
    # can access individual files with files[[i]]
    df <- join_all(files, by="Gene", type="full")
    df
  })

  output$data <- DT::renderDataTable(dat(),
                                     extensions = list('FixedColumns' = NULL,
                                                       'ColReorder' = NULL,
                                                       'FixedHeader' = NULL,
                                                       'Scroller' = NULL
                                                       ),
                                     options = list(dom = "iflrtpRCB",
                                                    fixedColumns =
                                                      list(leftColumns = 1),
                                                    scrollX = TRUE,
                                                    scrollY = 500,
                                                    scroller = TRUE,
                                                    deferRender = TRUE,
                                                    autoWidth=TRUE,
                                                    colReorder = TRUE
                                                    ),
                                     rownames = FALSE)

  indiv <- reactive({
    inFile <- input$files
    if (is.null(inFile)) {
      return(NULL)
    }
    num <- ncol(dat())-1
    variables <- c(colnames(dat()[,-1]))
    df <- data.frame('Data' = variables,
                     'Function' = numeric(num),
                     'Cuts' = numeric(num),
                     'Scale' = numeric(num),
                     'Min' = numeric(num),
                     'Max' = numeric(num))
    df
  })

  output$individual <- DT::renderDataTable(indiv(),
                                           extensions = list('FixedHeader' = NULL
                                           ),
                                           options = list(scrollY = 200,
                                                          autoWidth=TRUE,
                                                          dom = "t"
                                           ),
                                           rownames = FALSE)

  over <- reactive({
    inFile <- input$files
    if (is.null(inFile)) {
      return(NULL)
    }
    names <- c(inFile$name)
    num <- length(names)
    df <- data.frame('Data' = names,
                     'Weights' = numeric(num)
                     )
    df
  })

  output$overall <- DT::renderDataTable(over(),
                                           extensions = list('FixedHeader' = NULL
                                           ),
                                           options = list(scrollY = 200,
                                                          autoWidth=TRUE,
                                                          dom = "t"
                                           ),
                                           rownames = FALSE)

}

# ui ###########################################################################

ui <- dashboardPage(skin = "black",

  dashboardHeader(title = tags$img(src='integRATE_image.pdf',
                                   align = "left",
                                   width = '100%')),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",
               tabName = "home",
               icon = icon("home")),
      menuItem("Add Data",
               tabName = "data",
               icon = icon("upload")),
      menuItem("Analyze",
               tabName = "analyze",
               icon = icon("bar-chart")),
      menuItem("FAQ",
               tabName = "faq",
               icon = icon("question-circle"))
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(width = 12,
                    solidHeader = TRUE,
                    title = "Welcome to integRATE!",
                    tags$ul(
                      tags$li("integRATE is a tool based on desirability designed for the integration of heterogeneous 'omics data and prioritization of candidate genes"),
                      tags$li("Start by adding data for integration"),
                      tags$li("Then analyze that data with custom desirability functions"),
                      tags$li("Use the ranked gene list output to guide further research and functional validation")
                    )
                )
              )
      ),

      tabItem(tabName = "data",

              fluidRow(
                box(width = 12,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Add data",
                    fileInput("files",
                              label = "",
                              multiple = TRUE
                    ),
                    DT::dataTableOutput('metadata')
                )
              ),

              fluidRow(
                box(width = 12,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    title = "Data summary",
                    DT::dataTableOutput('data')
                )
              )
      ),

      tabItem(tabName = "analyze",

              fluidRow(
                box(width = 12,
                    solidHeader = TRUE,
                    title = "Customize desirability function analysis",
                    tabsetPanel(
                      tabPanel(
                        title = "Individual parameters",
                        DT::dataTableOutput('individual'),
                        br(),
                        actionButton("analyze_individual", "Analyze")
                      ),
                      tabPanel(
                        title = "Overall parameters",
                        DT::dataTableOutput('overall'),
                        br(),
                        actionButton("analyze_overall", "Analyze")
                      )
                    )
                )
              ),

              fluidRow(
                box(width = 12,
                    solidHeader = TRUE,
                    title = "Plot",
                    tabsetPanel(
                      tabPanel(
                        title = "Overall desirability"
                      ),
                      tabPanel(
                        title = "Top candidates"
                      ),
                      tabPanel(
                        title = "Clustered candidates"
                      )
                    )
                )
              )
      ),

      tabItem(tabName = "faq",

              fluidRow(
                box(width = 12,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    title = "What is integRATE?"
                )
              ),

              fluidRow(
                box(width = 12,
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = TRUE,
                    title = "How do I use integRATE?"
                )
              )

      )

    )
  )

)

shinyApp(ui = ui, server = server)

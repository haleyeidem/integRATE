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
                                         options = list(pageLength = 5,
                                                        dom = "tip"
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
                                                    fixedColumns = list(leftColumns = 1),
                                                    scrollX = TRUE,
                                                    scrollY = 1000,
                                                    scroller = TRUE,
                                                    deferRender = TRUE,
                                                    autoWidth=TRUE,
                                                    colReorder = TRUE
                                                    ),
                                     rownames = FALSE)

}

# ui ###########################################################################

ui <- dashboardPage(skin = "black",

  dashboardHeader(title = tags$strong("integRATE")),

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
                box(width = 12
                )
              )
      ),

      tabItem(tabName = "data",

              fluidRow(
                box(width = 12,
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
                    title = "Data summary",
                    DT::dataTableOutput('data')
                )
              )
      ),

      tabItem(tabName = "analyze",

              fluidRow(
                box(width = 12,
                    title = "Desirability Function Parameters")
              ),

              fluidRow(
                box(width = 12,
                    title = "Plots")
              ),

              fluidRow(
                box(width = 12,
                    title = "Data Download")
              )
      ),

      tabItem(tabName = "faq",

              fluidRow(
                box(width = 12,
                    title = "What is integRATE?")
              ),

              fluidRow(
                box(width = 12,
                    title = "How do I use integRATE?")
              )

      )

    )
  )

)

shinyApp(ui = ui, server = server)

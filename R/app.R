# dependencies #################################################################

library(shinythemes)
library(shinydashboard)
library(DT)
library(dplyr)
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
                                                        dom = "tip"),
                                         rownames = FALSE)

  dat <- reactive({
    inFile <- input$files
    if (is.null(inFile)) {
      return(NULL)
    }
    file_names <- as.list(inFile$datapath)
    files <- lapply(file_names, read.csv, header=TRUE)
    # can access individual files with files[[i]]
    df <- join_all(files, by="Gene", type="full")
    df
  })

  output$data <- DT::renderDataTable(dat())

  output$contents <- renderTable({
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, it will be a data frame with 'name',
    # 'size', 'type', and 'datapath' columns. The 'datapath'
    # column will contain the local filenames where the data can
    # be found.
    inFile <- input$file1

    if (is.null(inFile))
      return(NULL)

    read.csv(inFile$datapath, header = input$header)
  })

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
                    tabsetPanel(
                      tabPanel("Data summary",
                               DT::dataTableOutput('metadata'),
                               tableOutput('contents')
                      ),
                      tabPanel("Add pre-uploaded data",
                               DT::dataTableOutput('data')
                      ),
                      tabPanel("Add new data",
                               fileInput('file1', 'Choose CSV File',multiple = TRUE,
                                         accept=c('text/csv',
                                                  'text/comma-separated-values,text/plain',
                                                  '.csv'))
                      )
                    )
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

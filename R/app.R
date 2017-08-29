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
                                                    fixedColumns =
                                                      list(leftColumns = 1),
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

  dashboardHeader(title = tags$img(src='integRATE_colored.pdf',
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

    tags$head(tags$style(HTML('
      .skin-black .main-sidebar {
                              background-color: #556370;
                              }
                              .skin-black .sidebar-menu>li.active>a, .skin-black .sidebar-menu>li:hover>a {
                              background-color: #556370;
                              }
                              '))),

    tags$style(HTML("



                    .box.box-solid.box-primary>.box-header {
                    color:#ffffff;
                    background:#556370
                    }

                    .box.box-solid.box-success>.box-header {
                    color:#ffffff;
                    background:#4ecdc4
                    }

                    .box.box-solid.box-info>.box-header {
                    color:#ffffff;
                    background:#c7f465
                    }

                    .box.box-solid.box-warning>.box-header {
                    color:#ffffff;
                    background:#ff6b6b
                    }

                    .box.box-solid.box-danger>.box-header {
                    color:#ffffff;
                    background:#c54d57
                    }

                    ")),

    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                box(width = 12,
                    solidHeader = TRUE
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
                    title = "Desirability Function Parameters")
              ),

              fluidRow(
                box(width = 12,
                    solidHeader = TRUE,
                    title = "Plots")
              ),

              fluidRow(
                box(width = 12,
                    solidHeader = TRUE,
                    title = "Data Download")
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

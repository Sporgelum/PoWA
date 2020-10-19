library(BiocManager)
#BiocManager::install("DT")
#BiocManager::install("readxl")
library(readxl)
library(plotly)
library(shiny)
library(DT)
library(tidyverse)

# --- shiny ui
ui <- fluidPage(
  
  titlePanel(title = "Power Analysis"),
  
  
  tabsetPanel(
    tabPanel(title = "Data Visualization",
             plotOutput("plot1")),
    tabPanel(title = "Stats",
             verbatimTextOutput("stats")),
    tabPanel(title = "Uploading Files",
             # Sidebar layout with input and output definitions ----
             sidebarLayout(
               # Sidebar panel for inputs ----
               sidebarPanel(width = 3,
                            
                            # Input: Select a file ----
                            fileInput(inputId = "file1",label = "Choose CSV File",
                                      multiple = FALSE,
                                      accept = c("text/csv",
                                                 "text/comma-separated-values,text/plain",
                                                 ".csv",
                                                 "xlsx")
                                      ),
                            # Horizontal line ----
                            tags$hr(),
                            # File type selector ----
                            actionButton(inputId = "excel", "Excel",
                                         style = "background-color:#27CF2F;
                                         color:#0A0A0A;
                                         border-color:#27CF2F;
                                         border-style:dotted;
                                         border-width:5px;
                                         border-radius:70%;
                                         font-size:18px;"),
                            actionButton(inputId = "text",label = "Text",
                                         style = "background-color:#D12777;
                                         color:#0A0A0A;
                                         border-color:#D12777;
                                         border-style:dotted;
                                         border-width:5px;
                                         border-radius:0%;
                                         font-size:18px;"),
                            #radioButtons(inputId = "fileTypeInput",
                            #             label = h4("Choose File Type"),
                            #             choices = list(".CSV/TXT" = "signs",
                            #                            ".XLSX" = "excel")),
                            # Horizontal line ----
                            tags$hr(),
                            
                            # Input: Checkbox if file has header ----
                            checkboxInput(inputId = "header", label = "Header", TRUE),
                            
                            # Horizontal line ----
                            tags$hr(),
                            
                            # Input: Select separator ----
                            radioButtons(inputId = "sep", label = "Separator",
                                         choices = c(Comma = ",",
                                                     Semicolon = ";",
                                                     Tab = "\t"),
                                         selected = ","),
                            # Horizontal line ----
                            tags$hr(),
                            # Input: Select quotes ----
                            radioButtons(inputId = "quote", label = "Quote",
                                         choices = c(None = "",
                                                     "Double Quote" = '"',
                                                     "Single Quote" = "'"),
                                         selected = ''),
                            # Horizontal line ----
                            tags$hr(),
                            #Input: Select number of rows to display ----
                            radioButtons(inputId = "disp", label = "Display",
                                         choices = c(Head = "head",
                                                     All = "all"),
                                         selected = "head")
                            ),
               # Main panel for displaying outputs ----
               mainPanel(
                 # Output: Data file ----
                 DT::dataTableOutput('ex1')
                 )
               )
             )
    )
  )

# --- shiny server 
server <- function(input,output){
  
  #very imporatant inside of reactive values or any function use to rename or assign the =
  rv <- reactiveValues(data = mtcars)
  
  
  #  text = read.table(input$file1$datapath,
  #                    header = input$header,
  #                    sep = input$sep,
  #                    quote = input$quote),
  #  excel= read_excel(input$file1$datapath,
  #                    col_names = input$header,
  #                    sheet = 1))
  observeEvent(input$text, { rv$data <- read.table(input$file1$datapath, header = input$header, sep = input$sep, quote = input$quote) })
  observeEvent(input$excel, {rv$data <- read_excel(input$file1$datapath, col_names = input$header, sheet = 1) })

  output$ex1 <- DT::renderDataTable(
    DT::datatable(rv$data, options = list(pageLength = 25))
    )
  
  output$plot1 <- renderPlotly({
    
    df_plot <- mtcars
    plot_macro <- ggplot(df_plot, aes(x = mpg,
                                      y = disp,
                                      color = gear)) +
      geom_point()
    
    ggplotly(plot_macro)
    })
  }


  
  
# --- shiny app
shinyApp(ui = ui, server = server)
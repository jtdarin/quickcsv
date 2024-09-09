library(tidyverse)
library(data.table)
library(shiny)
library(shinyjs)

ui <- navbarPage(
  title = "Dataset Constructor Tool",
  tabPanel(
    title = "Construct New Dataset",
    
    useShinyjs(),
    
    # Instructions text
    fluidRow(
      column(12, textOutput("text1"))
    ),
    
    # Horizontal line
    tags$hr(),
    
    # Create two columns layout for left and right file inputs
    fluidRow(
      # Left side
      column(6,
             h3("Upload Left CSV File(s)"),
             fileInput(
               inputId = "left",
               label = "Upload Left Data CSV File(s). If uploading multiple files, select all files at once in the pop-up window.",
               multiple = TRUE,
               accept = c(
                 "text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv"
               )
             ),
             # Table to show uploaded left file details
             tableOutput("leftfiles"),
             
             # Preview of the first uploaded left CSV file
             tableOutput("leftpreview")
      ),
      
      # Right side
      column(6,
             h3("Upload Right CSV File(s)"),
             fileInput(
               inputId = "right",
               label = "If uploading multiple files, select all files at once in the pop-up window.",
               multiple = TRUE,
               accept = c(
                 "text/csv",
                 "text/comma-separated-values,text/plain",
                 ".csv"
               )
             ),
             # Table to show uploaded right file details
             tableOutput("rightfiles"),
             
             # Preview of the first uploaded right CSV file
             tableOutput("rightpreview")
      )
    ),
    
    # Horizontal line
    tags$hr(),
    
    # Center the selectInput for columns to left-join on
    fluidRow(
      column(12, align = "center",
             tags$div(style = "text-align: center;",
                      uiOutput("column_selector")
             )
      )
    ),
    
    # Horizontal line
    tags$hr(),
    
    # Centered download button
    fluidRow(
      column(12, align = "center",
             tags$div(style = "text-align: center;",
                      downloadButton(
                        outputId = "download",
                        label = "Download Dataset"
                      )
             )
      )
    ),
    
    # Horizontal line
    tags$hr(),
    
    # My Name
    fluidRow(
      column(12, align = "center",
             textOutput("text0")
      )
    ),
    
    # Github link
    fluidRow(
      column(12, align = "center",
             tags$div(style = "text-align: center;",
                      tags$a(
                        href = "https://github.com/jtdarin/quickcsv",  
                        "Github",            
                        target = "_blank"                   
                      )
             )
      )
    )
  )
)

server <- function(input, output) {
  # Set max file upload size to 100 MB
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)
  
  # text 
  output$text0 <- renderText("Developed by J.T. Darin.")
  output$text1 <- renderText("This tool allows you to easily join two files into one dataset using an inner-join. 
                             An inner-join keeps values in both the left and right files which have a matching key in both files.
                             The key on which to join the two files can be selected after uploading your left and right files.
                             The input buttons allow for multiple file uploads, which is useful if you have identically formatted files
                             with different data that you would like to combine.")
  
  # render left files table (show file name, size, and type)
  output$leftfiles <- renderTable({
    req(input$left)
    data.frame(
      Name = input$left$name,
      Size = input$left$size,
      Type = input$left$type
    )
  })
  
  # render right files table (show file name, size, and type)
  output$rightfiles <- renderTable({
    req(input$right)
    data.frame(
      Name = input$right$name,
      Size = input$right$size,
      Type = input$right$type
    )
  })
  
  ### CSV Preview logic ###
  
  # Preview the first left CSV file (first few rows)
  output$leftpreview <- renderTable({
    req(input$left)
    # Read the first file only
    leftFilePath <- input$left$datapath[1]
    # Preview the first few rows
    read_csv(leftFilePath) %>%
      head(5)
  })
  
  # Preview the first right CSV file (first few rows)
  output$rightpreview <- renderTable({
    req(input$right)
    # Read the first file only
    rightFilePath <- input$right$datapath[1]
    # Preview the first few rows
    read_csv(rightFilePath) %>%
      head(5)
  })
  
  ### reactives ###
  
  # bind left dataset
  leftData <- reactive({
    req(input$left)
    rbindlist(lapply(input$left$datapath, read_csv), use.names = TRUE, fill = TRUE)
  })
  
  # bind right dataset
  rightData <- reactive({
    req(input$right)
    rbindlist(lapply(input$right$datapath, read_csv), use.names = TRUE, fill = TRUE)
  })
  
  # column choice for left join
  columnChoice <- reactive({
    req(leftData(), rightData())
    intersect(names(leftData()), names(rightData()))
  })
  
  output$column_selector <- renderUI({
    req(leftData(), rightData())
    
    if (length(columnChoice()) == 0) {
      showNotification("Error: The left and right files do not have any matching column names.", type = "error", duration = NULL)
      shinyjs::disable("download")  # Disable the download button
      return(NULL)
    } else {
      shinyjs::enable("download")  # Enable the download button when columns match
      selectInput(
        inputId = "column",
        label = "Select column to perform inner-join",
        choices = columnChoice(),
        multiple = FALSE
      )
    }
  })
  
  # join datasets together
  fullData <- reactive({
    if (length(columnChoice()) == 0) {
      return(NULL)  # No common columns, return NULL
    } else {
      leftData() %>%
        inner_join(rightData(), by = input$column) %>%
        # Remove columns containing only NA values
        select_if(~sum(!is.na(.)) > 0)
    }
  })
  
  # download handler
  output$download <- downloadHandler(
    filename = function() {
      paste0("file_", format(Sys.time(),"%Y-%m-%d %H:%M:%S"), ".csv", sep = "")
    },
    content = function(file) {
      validate(need(fullData(), "Cannot download the file. Please check your inputs."))
      write.csv(fullData(), file)
    }
  )
}

shinyApp(ui = ui, server = server)

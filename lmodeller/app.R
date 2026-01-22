library(shiny)
library(bslib)

ui <- page_fillable(
  
  layout_columns(
    card(
      fileInput(
        "file", "Upload a .csv file", accept = ".csv"
      ),
    card(
      varSelectInput("preds", "Select predictors",
                     data = NULL, multiple = TRUE)
    )
    )
  ),
  
  card(
    textOutput(outputId = "selectedPreds"),
    card_header("Selected Predictors")
  )
) #close of page_fillable


server <- function(input, output){
  
  filedata <- reactive({
    req(input$file)
    
    data <- read.csv(input$file$datapath)
    
  })
  
  observeEvent(
    filedata(), {
      updateVarSelectInput(
        inputId = "preds",
        data = filedata()
      )
    }
  )
  
  output$selectedPreds <- renderPrint({
    req(input$preds)
    print(input$preds)
  })
}

shinyApp(ui = ui, server = server)
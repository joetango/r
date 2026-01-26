library(shiny)
library(bslib)

ui <- page_fillable(
  
  layout_columns(
    card(
      fileInput(
        "file", "Upload a .csv file", accept = ".csv"
      )
    ),
    card(
      varSelectInput("resp", "Select a response",
                     data = NULL, multiple = FALSE,
                     selected = NULL),
      varSelectInput("preds", "Select predictors",
                     data = NULL, multiple = TRUE,
                     selected = NULL),
      actionButton("submit", "Submit")
    )
  ),
  
  card(
    verbatimTextOutput(outputId = "selectedResp"),
    verbatimTextOutput(outputId = "selectedPreds")
  ),
  
  card(
    verbatimTextOutput(outputId = "lmodsum")
  ),
  
  padding = c("1vw", "2vw")
) #close page_fillable


server <- function(input, output){
  
  filedata <- reactive({
    req(input$file)
    
    read.csv(input$file$datapath)
    
  })
  
  observeEvent(
    filedata(), {
      updateVarSelectInput(
        inputId = "preds",
        data = filedata()
      )
    }
  )
  
  observeEvent(
    filedata(), {
      updateVarSelectInput(
        inputId = "resp",
        data = filedata()
      )
    }
  )
  
  # output$selectedPreds <- renderPrint({
  #   req(input$preds)
  #   print(input$preds)
  # })
  
  predictors <- reactive({
    req(input$preds)
    input$preds

  })
  
  response <- reactive({
    req(input$resp)
    input$resp

  })
  
  output$selectedResp <- renderPrint({
    paste(response())
  })
  
  output$selectedPreds <- renderPrint({
    paste(predictors())
  })
  
  lmod <- eventReactive(input$submit, {
    req(response(), predictors())
    lm(as.character(response()) ~ as.character(predictors()), 
       data = filedata()
    )
  
    
  })
  
  output$lmodsum <- renderPrint({
    print(summary(lmod()))
  })
  
  
    
  
  
}

shinyApp(ui = ui, server = server)
library(shiny)
library(shinydashboard)

## Global Functions

## DC Check

difficulty_check <- function(dc1, mod1=0){
  roll <- sample(1:20, 1)
  if((roll + mod1) >= dc1){
    paste("Roll:", roll, "+ Modifer:", mod1, "=", (roll + mod1), "...Success!")
  } 
  else {
    paste("Roll:", roll, "+ Modifer:", mod1, "=", (roll + mod1), "...Failure")
  }
}


ui <- dashboardPage(
  
  dashboardHeader("Dungeon Master's Multitool"),
  dashboardSidebar(sidebarMenu(id = "menu", sidebarMenuOutput("menu"))),
  dashboardBody(tabItems(
    tabItem(
      tabName = "DC")
    )
  ))
  
  sidebarLayout(
  
    sidebarPanel(
      h5("Difficulty Check"),
      selectInput("dc", "Select Difficulty Class", choices = c(1:20)),
      numericInput("mod", "Enter Modifier:", value = 0),
      actionButton("dc_roll", "Roll")

    ), #difficultyCheck tabPanel
    
  mainPanel(
    textOutput("dc_result")
  )
) #navbarPage


server <- function(input, output) {
  
  dc_result <- eventReactive(input$dc_roll, {
    difficulty_check(as.numeric(input$dc), input$mod)
  })

  output$dc_result <- renderText({
    dc_result()
  })
    
} #server


shinyApp(ui = ui, server = server)

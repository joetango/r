################
### Packages ###
################

library(shiny)
library(ggplot2)
library(bslib)

###########################
### Initialize Function ###
###########################

fnc <- function(n){
  vec <- numeric(n)
  
  for(i in 1:n){
    vec[i] <- sum(sample(1:6, 8, replace = TRUE))
  }
  return(as.data.frame(vec))
}

##########
### UI ###
##########

ui <- fluidPage(

    # Application title
    titlePanel("Central Limit Theorem Illustration"),
    

    # Sidebar 
    sidebarLayout(
        sidebarPanel(
            radioButtons("rolls",
                        "Number of Rolls:",
                        choices = c(
                          10, 25, 100, 1000, 10000
                        )
            ),
            actionButton(
              "runplot", "Generate Plot"
            )
        ),

        # Plot
        mainPanel(
           plotOutput("plot")
        ),
    )
)



##############
### Server ###
##############

server <- function(input, output) {
  
  data <- eventReactive(input$runplot, {
      fnc(as.numeric(input$rolls))
  })

    output$plot <- renderPlot({
      ggplot(data = data(), aes(x = vec)) +
        geom_bar(fill = "lightblue", size = .5) +
        stat_density(geom = "line", aes(y = after_stat(density) * n), 
                     color = "orange", linewidth = .8) +
        scale_x_continuous(breaks = seq(min(data()$vec), max(data()$vec), by = 1)) +
        theme_minimal() +
        labs(title = paste("8d6 Roll Frequency With",
                           length(data()$vec), "Rolls"),
             x = "Roll Value",
             y = "Number of Rolls") +
        theme(plot.title = element_text(hjust = .5))
    })
}

###########
### Run ###
###########

shinyApp(ui = ui, server = server)

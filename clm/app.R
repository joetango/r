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

ui <- page_fillable(

  titlePanel("Central Limit Theorem Illustration"),
    
  layout_columns(
    card(
      radioButtons("rolls",
                    "Number of Rolls:",
                    choices = c(
                      10, 25, 100, 500, 10000
                    )
        ),
        actionButton(
          "runplot", "Generate Plot"
        )
      ),
    
      
    card(
        print(
          "The Central Limit Theoremstates that an increase in sample 
          size will result in a normal distribution of the sample mean. 
          This app seeks to illustrate this theorem in play using six-sided 
          dice. Select a number of rolls, and observe the distribution of 
          values."
        ),
        a("Central Limit Theorem",
          href = "https://www.geeksforgeeks.org/maths/central-limit-theorem/"),
      ),
    col_widths = c(4, 8)
    ),
    
  card(
    plotOutput("plot")
  ),
  
  padding = c("1vw", "10vw")
  
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
        # geom_histogram(aes(y = after_stat(density)),
        #                fill = "lightblue", binwidth = 1,
        #                outline.type = "both") +
        # geom_density(alpha = .2, fill = "orange") +
        geom_bar(fill = "lightblue", size = 0.8) +
        stat_density(geom = "line", aes(y = after_stat(density) * n),
                     color = "orange", linewidth = .8) +
        scale_x_continuous(breaks = seq(min(data()$vec), max(data()$vec), by = 1)) +
        theme_minimal() +
        labs(title = paste("8d6 Roll Frequency With",
                           length(data()$vec), "Rolls"),
             x = "Roll Value",
             y = "Number of Rolls Per Sum") +
        theme(plot.title = element_text(hjust = .5))
    })
}

###########
### Run ###
###########

shinyApp(ui = ui, server = server)

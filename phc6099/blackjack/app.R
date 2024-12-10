library(shiny)

ui <- fluidPage(

    titlePanel("The Dungeon Master's Multi-tool"),

     
    sidebarLayout(
        sidebarPanel(
            actionButton("run_d20", "Roll d20"),
            textOutput("d20"),
            actionButton("run_d12", "Roll d12"),
            textOutput("d12")
        ),

        
        mainPanel(
          actionButton("run_tavern", "Generate Tavern Name"),
          textOutput("tavern"),
          
           
        ) # sidebar
    ) # main panel
) # fluid page

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  tavern <- reactiveVal(NULL)
  d20 <- reactiveVal(NULL)
  d12 <- reactiveVal(NULL)
  
  observeEvent(input$run_tavern, {
    
    tavern_adj <- c("Stout", "Bloody", "Slow", "Dull", "Soaked", "Drunken",
                    "Crooked", "Dark", "Fabulous", "Noble", "Soft", "Red",
                    "Green", "Whtie", "Black", "Yellow", "Blue", "Burning",
                    "Broken", "Shattered", "Mighty", "Strong", "Lonely",
                    "Poor", "Old", "Generous", "Lanky", "Hapless", "Tall",
                    "Remarkable", "Frugal", "Prudent", "Foul", "Evil", "Good",
                    "Rotten", "Shining", "Fragile", "Hungry", "Tired",
                    "Patient", "Merciful", "Immortal", "Faithful", "Friendly",
                    "Forlorn", "Adoring", "Brittle", "Floating", "Sharp",
                    "Worn", "Cursed", "Beautiful", "Beleoved", "Quiet",
                    "Happy", "Courageous", "Wounded", "Blind", "Clairvoyant",
                    "Blushing", "Calm", "Wary", "Cheerful", "Wise", "Clumsy",
                    "Boorish", "Boastful", "Sly", "Daring", "Rebellious",
                    "Diligent", "Disguised", "Ominous", "Determined", "Reliable",
                    "Loyal", "Raging", "Excited", "Shy", "Magical", "Trecherous",
                    "False", "Foolhardy", "Golden", "Frozen", "Gracious", "Hairy",
                    "Hidden", "Hoarse", "Honest", "Humble", "Limping",
                    "Lively", "Lucky", "Lean", "Nefarious", "Ogling",
                    "Subtle", "Crazy")
    
    tavern_noun <- c("Rooster", "Raven", "Crow", "Toad", "Hound", "Fox",
                     "Bull", "Boar", "Clam", "Hawk", "Eagle", "Mouse",
                     "Rat", "Frog", "Elk", "Cat", "Guardian", "Hunter",
                     "Barbarian", "Witch", "Troll", "Sword", "Shield",
                     "Bow", "Dagger", "Hammer", "Helm", "Acrobat", "Lion",
                     "Ghoul", "Druid", "Master", "King", "Queen", "Prince",
                     "Princess", "Shrub", "Tree", "Bear", "Smile", "Eye",
                     "Tounge", "Flounder", "Whale", "Steer", "Stallion",
                     "Mare", "Wish", "Hoof", "Goat", "Tower", "Fist",
                     "Monk", "Sleep", "Fool", "Knight", "Poet", "Thrush",
                     "Diamond", "Ruby", "Emerald", "Lute", "Drum", "Flute",
                     "Farmer", "Songbird", "Mother", "Father", "Solider",
                     "Sailor", "Brewer", "Hornet", "Donkey", "Hare",
                     "Twig", "Barrel", "Boot", "Fang", "Skull", "Snail",
                     "Beetle")
    
    result <- paste("The", sample(tavern_adj, 1), sample(tavern_noun, 1))
    
    tavern(result)
  })
  
  
  ## Observe Events
  
  observeEvent(input$run_d20, {
    
    d20(sample(1:20, 1))
  })
  
  observeEvent(input$run_d12, {
    
    d12(sample(1:12, 1))
  })

  
  ## Outputs
  
    output$tavern <- renderText({
      if (!is.null(tavern())) {
        paste(tavern())
      }
      else {
        "[click button to generate name]"
      }
      
    })
    
    output$d20 <- renderText ({
      paste("d20 result:", d20())
    })
    
    output$d12 <- renderText ({
      paste("d12 result:", d12())
    })
}

# Run
shinyApp(ui = ui, server = server)

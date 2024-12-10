library(shiny)

## Global

difficulty_check <- function(dc, mod){
  roll <- sample(1:20, 1)
  if((roll + mod) >= dc){
    paste("Roll:", roll, "+ Modifer:", mod, "=", (roll + mod), "...Success!")
  }
  else {
    paste("Roll:", roll, "+ Modifer:", mod, "=", (roll + mod), "...Failure")
  }
}

ui <- fluidPage(

    titlePanel("The Dungeon Master's Multi-tool"),

     
    sidebarLayout(
        sidebarPanel(
          h3("Dice Rollers"),
          actionButton("run_d20", "Roll d20"),
          textOutput("d20"),
          h5(""),
          actionButton("run_d12", "Roll d12"),
          textOutput("d12"),
          h5(""),
          actionButton("run_d10", "Roll d10"),
          textOutput("d10"),
          h5(""),
          actionButton("run_d8", "Roll d8"),
          textOutput("d8"),
          h5(""),
          actionButton("run_d6", "Roll d6"),
          textOutput("d6"),
          h5(""),
          actionButton("run_d4", "Roll d4"),
          textOutput("d4"),
          h5(""),
          actionButton("run_d100", "Roll d100"),
          textOutput("d100")
        ),

        
        mainPanel(
          h4("A selection of handy tools for any dungeon masters in a pinch"),
          
          h4("-------------------------------------------------------------"),
          
          h4("Difficulty Check"),
          selectInput("dc", "Select Difficulty Class", choices = c(1:20)),
          numericInput("mod", "Enter Modifier:", value = 0),
          actionButton("dc_roll", "Roll"),
          textOutput("dc_result"),
          
          h4("-------------------------------------------------------------"),
          
          actionButton("generate_npc", "Generate NPC Stats"),
          textOutput("npc_stats"),
          
          h4("-------------------------------------------------------------"),
          
          actionButton("run_tavern", "Generate Tavern Name"),
          textOutput("tavern")
          
        ) # sidebar
    ) # main panel
) # fluid page


server <- function(input, output) {
  
  npc_stats <- reactiveVal(NULL)
  tavern <- reactiveVal(NULL)
  d20 <- reactiveVal(NULL)
  d12 <- reactiveVal(NULL)
  d10 <- reactiveVal(NULL)
  d8 <- reactiveVal(NULL)
  d6 <- reactiveVal(NULL)
  d4 <- reactiveVal(NULL)
  d100 <- reactiveVal(NULL)
  
  dc_result <- eventReactive(input$dc_roll, {
    difficulty_check(as.numeric(input$dc), input$mod)
  })
  
  
  observeEvent(input$generate_npc, {
    stat_gen <- function() {
      stat <- c(sample(1:6, 4, replace = TRUE))
      stat <- stat[stat != min(stat)]
      sum(stat)
    }
    
    for(i in 1:6) {
      temp <- paste0("stat", i)
      
      assign(temp, stat_gen())
    }
    
    npc <- paste("Strength:", stat1,
                 "Dexterity:", stat2,
                 "Constitution:", stat3,
                 "Intelligence:", stat4,
                 "Wisdom:", stat5,
                 "Charisma:", stat6)
    
    npc_stats(npc)
  })
  
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
  
  
  ## Rollers
  
  observeEvent(input$run_d20, {
    
    d20(sample(1:20, 1))
  })
  
  observeEvent(input$run_d12, {
    
    d12(sample(1:12, 1))
  })
  
  observeEvent(input$run_d10, {
    
    d10(sample(1:10, 1))
  })
  
  observeEvent(input$run_d8, {
    
    d8(sample(1:8, 1))
  })
  
  observeEvent(input$run_d6, {
    
    d6(sample(1:6, 1))
  })
  
  observeEvent(input$run_d4, {
    
    d4(sample(1:4, 1))
  })
  
  observeEvent(input$run_d100, {
    
    d100(sample(1:100, 1))
  })

  
  ## Outputs
  
  output$dc_result <- renderText({
    dc_result()
  })
  
    output$npc_stats <- renderText({
      if (!is.null(npc_stats())) {
        paste(npc_stats())
      }
      else {
        "[click button to generate NPC stats]"
      }
      
    })
  
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
    
    output$d10 <- renderText ({
      paste("d10 result:", d10())
    })
    
    output$d8 <- renderText ({
      paste("d8 result:", d8())
    })
    
    output$d6 <- renderText ({
      paste("d6 result:", d6())
    })
    
    output$d4 <- renderText ({
      paste("d4 result:", d4())
    })
    
    output$d100 <- renderText ({
      paste("d100 result:", d100())
    })
}

# Run
shinyApp(ui = ui, server = server)

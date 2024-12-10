library(shiny)
library(shinydashboard)

  ##################################
  ######### Global Functions #######
  ##################################

difficulty_check <- function(dc, mod){
  roll <- sample(1:20, 1)
  if((roll + mod) >= dc){
    paste(" d20 roll:", roll, "+ Modifer:", mod, "=", (roll + mod), "...Success!")
  }
  else {
    paste("d20 roll:", roll, "+ Modifer:", mod, "=", (roll + mod), "...Failure")
  }
}

mult_dice <- function(dice_type, dice_quant){
  rolls <- c(sample(1:dice_type, dice_quant, replace = TRUE))
  sum_rolls <- sum(rolls)
  paste("Result:", paste(rolls, collapse = " + "), " = ", sum_rolls)
}

hp_down <- function(x){
  return(x + 1)
}

hp_up <- function(x){
  return(x - 1)
}

  ##############################
  ####### User Interface #######
  ##############################

ui <- dashboardPage(
  
  dashboardHeader(title = "DM Toolkit"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home"),
      menuItem("Dice Roller", tabName = "roller"),
      menuItem("Difficulty Class Check", tabName = "dc_check"),
      menuItem("Worldbuilding Generators", tabName = "gens"),
      menuItem("Combat Tracker", tabName = "combat")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              h2("The Dungeon Master's Multitool"),
              h2(""),
              h4("An application to provide quick help to a dungeon master in need.
                 Use the sidebar to navigate the various tools."),
              img(src = "agoblin1.png", align = "center")),
              
      
      tabItem(tabName = "roller",
              h2("Dice Roller"),
              h2(""),
              
              fluidRow(
                column(6, 
                       actionButton("run_d20", "Roll d20"),
                       textOutput("d20", inline = TRUE),
                       h5(""),
                       actionButton("run_d12", "Roll d12"),
                       textOutput("d12", inline = TRUE),
                       h5(""),
                       actionButton("run_d10", "Roll d10"),
                       textOutput("d10", inline = TRUE)
                ),
                column(6, 
                       actionButton("run_d8", "Roll d8"),
                       textOutput("d8", inline = TRUE),
                       h5(""),
                       actionButton("run_d6", "Roll d6"),
                       textOutput("d6", inline = TRUE),
                       h5(""),
                       actionButton("run_d4", "Roll d4"),
                       textOutput("d4", inline = TRUE),
                       h5(""),
                       actionButton("run_d100", "Roll d100"),
                       textOutput("d100", inline = TRUE)
                )
              ),
              
              h4("Roll Multiple Dice:"),
              numericInput("dice_quant", "Quantity of Dice", value = 1),
              selectInput("die_type", "Sides of the Dice", choices = c(20,
                                                                 12, 10,
                                                                 8, 6,
                                                                 4, 100)),
              actionButton("mult_roll", "Roll"),
              textOutput("mult_result")
      ),
      
      tabItem(tabName = "dc_check",
              h2("Difficulty Class Check"),
              h2(""),
              h4("Instructions:"),
              p("Set the difficulty class to an appropriate level given
                 the obstacle (for example, climbing a tree may have a
                 difficulty class of 5, while jumping over a crevasse
                 may have a DC of 15). Then, add an appropriate modifer
                 based on the player character's stats (acrobatics +2 will
                 add +2 to the roll for jumping over the crevasse). The
                 program will do the rest for you."),
              h2(""),
              selectInput("dc", "Set Difficulty Class", choices = c(5, 10, 15, 20,
                                                                    25, 30)),
              numericInput("mod", "Enter Modifier:", value = 0),
              actionButton("dc_roll", "Roll"),
              textOutput("dc_result")),
      
      tabItem(tabName = "gens",
              h2("On-the-fly Worldbuilding Generators"),
              h2(""),
              p("All dungeon masters face the challenge of improvisation,
                 and sometimes the improvisation juices are not flowing
                 to their full potential. Below are some helpful tools
                 for generating information pertaining to world-buidling."),
              h2(""),
              actionButton("generate_npc", "Generate NPC Stats"),
              textOutput("npc_stats"),
              h1(""),
              actionButton("run_tavern", "Generate Tavern Name"),
              textOutput("tavern"),
              h1(""),
              actionButton("run_potion", "Generate Potion"),
              textOutput("potion")),
      
      tabItem(tabName = "combat",
              h2("Combat Tracker"),
              h2(""),
              p("Enter the amount of enemies and a table will appear to help keep track of
                an instance of combat. First, enter a number of enemies to keep track of.
                Then, use the arrows on the input field or your keyboard to change the enemy HP and AC.
                A d20 is included on this page for convenience."),
              numericInput("enemy_quant", "How many enemies?", value = 0, min = 0,
                           max = 10),
              uiOutput("enemy_inputs"),
              tableOutput("combat_stats"),
              actionButton("run_d20_2", "Roll d20"),
              textOutput("d20_2", inline = TRUE))
    )
  )
)  

  ######################
  ####### Server #######
  ######################

server <- function(input, output){
  
  ##############################
  ####### Combat Tracker #######
  ##############################
  
  d20_2 <- reactiveVal(NULL)
  
  enemies <- reactiveVal(data.frame(Enemy = character(0), HP = numeric(0),
                                    AC = numeric(0)))
  
  observeEvent(input$run_d20_2, {
    d20_2(sample(1:20, 1))
  })
  
  output$d20_2 <- renderText ({
    paste(d20_2())
  })
  
  output$enemy_inputs <- renderUI({
    n <- input$enemy_quant
    
    req(n >= 0)
    
    if(n==0){
      return(NULL)
    }
    
    lapply(1:n, function(i){
      fluidRow(
        column(6, textInput(paste0("enemy_name_", i), label = paste("Enemy", i, "Name"), value = paste("Enemy", i))),
        column(3, numericInput(paste0("enemy_hp_", i), label = paste("HP", i), value = 20, min = 1, max = 1000)),
        column(3, numericInput(paste0("enemy_ac_", i), label = paste("AC", i), value = 12, min = 1, max = 30))
      )
    })
  })
  
  observeEvent(input$hp_up_btn, {
    n <- input$enemy_quant
    if (n > 0) {
      for (i in 1:n) {
        
        enemy_hp <- input[[paste0("enemy_hp_", i)]]
        new_hp <- hp_up(enemy_hp)
        updateNumericInput(session, paste0("enemy_hp_", i), value = new_hp)
      }
    }
  })
  
  observeEvent(input$hp_down_btn, {
    n <- input$enemy_quant
    if (n > 0) {
      for (i in 1:n) {
        
        enemy_hp <- input[[paste0("enemy_hp_", i)]]
        new_hp <- hp_down(enemy_hp)
        updateNumericInput(session, paste0("enemy_hp_", i), value = new_hp)
      }
    }
  })
  
  observe({
    n <- input$enemy_quant
    
    req(n > 0)
    
    if (n > 0) {
      
      enemy_data <- data.frame(Enemy = character(0), HP = numeric(0), AC = numeric(0))
      for (i in 1:n) {
        enemy_data <- rbind(enemy_data, data.frame(
          Enemy = input[[paste0("enemy_name_", i)]],
          HP = input[[paste0("enemy_hp_", i)]],
          AC = input[[paste0("enemy_ac_", i)]]
        ))
      }
      enemies(enemy_data)
    }
  })
  
  
  output$combat_stats <- renderTable({
    enemies()
  })

  
  ################################
  ####### Potion Generator #######
  ################################
  
  potion <- reactiveVal(NULL)

  observeEvent(input$run_potion, {

    container <- c("A glass statuette of a God", "A thin glass vial", "A leather pouch",
                   "A drinking horn", "A metal flask", "A hallowed out crystal",
                   "The sealed bone of an ogre", "A short, fat, glass jar",
                   "A wooden jar with a cork", "An ornate iron flagon", "A syringe",
                   "A clay urn", "A glass skull", "A shot-glass sized container",
                   "A fist-sized barrel", "A sealed, ornate tankard",
                   "A hallowed out dragon tooth", "A glass sphere with a cork",
                   "A repurpsed scorpion tail", "A small bronze container")

    color <- c("white", "lime green", "bronze", "purple", "ruby", "pale blue",
               "forest green", "lavender", "orange", "plum", "silver", "black",
               "jade", "peach", "red", "yellow", "azure", "olive", "sky blue", "tan")

    effect <- c("instantly restores some health (2d6 + 2)",
                "gives the strength of a giant for ten minutes",
                "makes the drinker double in size",
                "cures an illness",
                "doubles speed for one hour",
                "ages the drinker by five years",
                "temporarily turns the drinker's teeth into stone",
                "puts the drinker into a coma for 6 hours",
                "gives resistence to acid damage for one hour",
                "increases acrobatics for one hour",
                "instantly grows a beard on the drinker",
                "turns the drinker invisible for 10 minutes",
                "grants levitation for two minutes",
                "forces the drinker to tell the truth for one hour",
                "grants a temporary increase in max health until the health is lost",
                "grants the drinker underwater breathing for twenty minutes",
                "gives the user a single vision of the future",
                "makes the drinker lose sight for one hour",
                "lets the drinker ask their deity one question",
                "gives the drinker a slight experience boost"
    )

    result <- paste(sample(container, 1), "containing a liquid of", sample(color, 1),
                    "color that", sample(effect, 1))

    potion(result)
  })

  output$potion <- renderText({
    if (!is.null(potion())) {
      paste(potion())
    }
    else {
      "[click button to generate a potion]"
    }

  })
  
  
  ####################################
  ####### Multiple Dice Roller #######
  ####################################
  
  mult_result <- eventReactive(input$mult_roll, {
    mult_dice(as.numeric(input$die_type), input$dice_quant)
  })
  
  output$mult_result <- renderText({
    mult_result()
  })
  
  ######################################
  ####### Difficulty Class Check #######
  ######################################
  
  dc_result <- eventReactive(input$dc_roll, {
    difficulty_check(as.numeric(input$dc), input$mod)
  })
  
  output$dc_result <- renderText({
    dc_result()
  })
  
  ##################################
  ####### NPC Stat Generator #######
  ##################################
  
  npc_stats <- reactiveVal(NULL)
  
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
  
  output$npc_stats <- renderText({
    if (!is.null(npc_stats())) {
      paste(npc_stats())
    }
    else {
      "[click button to generate NPC stats]"
    }
    
  })
  
  #####################################
  ####### Tavern Name Generator #######
  #####################################
  
  tavern <- reactiveVal(NULL)
  
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
                    "Worn", "Cursed", "Beautiful", "Beloved", "Quiet",
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
  
  output$tavern <- renderText({
    if (!is.null(tavern())) {
      paste(tavern())
    }
    else {
      "[click button to generate name]"
    }
    
  })
  
  
  ############################
  ####### Dice Rollers #######
  ############################
  
  d20 <- reactiveVal(NULL)
  d12 <- reactiveVal(NULL)
  d10 <- reactiveVal(NULL)
  d8 <- reactiveVal(NULL)
  d6 <- reactiveVal(NULL)
  d4 <- reactiveVal(NULL)
  d100 <- reactiveVal(NULL)
  
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
  
  ##############################
  
  output$d20 <- renderText ({
    paste(d20())
  })
  
  output$d12 <- renderText ({
    paste(d12())
  })
  
  output$d10 <- renderText ({
    paste(d10())
  })
  
  output$d8 <- renderText ({
    paste(d8())
  })
  
  output$d6 <- renderText ({
    paste(d6())
  })
  
  output$d4 <- renderText ({
    paste(d4())
  })
  
  output$d100 <- renderText ({
    paste(d100())
  })
  
}



shinyApp(ui = ui, server = server)
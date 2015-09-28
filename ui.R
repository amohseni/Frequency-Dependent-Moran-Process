library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Title
  titlePanel("The Moran Process for 2x2 Symmetric Games"),
  
  # Sidebar for Parameter Input
  sidebarLayout(
      
      sidebarPanel(
    
        selectInput("interaction_structure", "Game Type",
            c("Degenerate Game", "Dominating Strategy Game", "Coordination Game", "Anticoordination Game", "Custom (to manually change payoffs)"),
            selected = "Anticoordination Game"
          ),
        
        uiOutput("ui"),
        
        # 2X2 Game Payoff Input Fields
        strong("Payoff / Fitness Matrix:"),
        
          # 1st Row: Types
          fluidRow(
            column(2),
            column(5,
                   p("Type A", align="center")
                   ),
            column(5,
                   p("Type B", align="center")
            )),

          # 2nd Row: Payoffs to Type A
          fluidRow(
            column(2,
                   p("Type A", align="center")
                   ),
            column(5,
                   
                   numericInput("a", 
                                label = NULL, 
                                value = 1,
                                min = 1)
          ),
          column(5,
                 numericInput("b", 
                              label = NULL, 
                              value = 3,
                              min = 1)
          )),
        
          # 3rd Row: Payoffs to Type B
          fluidRow(
            column(2,
                   p("Type B", align="center")
                  ),
            column(5,      
                   numericInput("c", 
                                label = NULL, 
                                value = 2,
                                min = 1)
            ), 
            column(5,
                   numericInput("d", 
                                label = NULL, 
                                value = 1,
                                min = 1)
            )),
        
        # Population Size Slider
        sliderInput("populationSize",
                    "Population Size:",
                    min = 2,
                    max = 200,
                    value = 100),
        
        # Mutation Rate Slider
        sliderInput("mutationRate",
                    "Mutation Rate:",
                    min = 0,
                    max = .1,
                    value = .02),
        
        # Intensity of Selection Slider        
        sliderInput("intensityOfSelection",
                    "Intensity of Selection:",
                    min = 0,
                    max = 1,
                    value = .1),

        # Display Max Probability (Y-Axis)                         
        sliderInput("probabilityMax",
                    "(Y-Axis) Display Max Probability:",
                    min = 0,
                    max = 1,
                    value = .05),
        
        tags$head(tags$script(src = "message-handler.js")),
        p(actionButton("simulateSinglePopulation", "Simulate Single Population"), align = "center"),
        
        # Simulation Time Slider
        sliderInput("simulationTime",
                    "Simulation Time:",
                    min = 1,
                    max = 10000,
                    value = 1000)
      ),
  
  # Main Panel with Stationary Distribution + Simulation & Stats Panels
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Stationary (Limit) Distribution & Simulation", 
                           plotOutput("stationaryDistribution"), 
                           plotOutput("singlePopulationSimulation")),
                  tabPanel("Replacement Probabilities & Invasion Dynamics", verbatimTextOutput("summary")) 
      )
    )
  )
))
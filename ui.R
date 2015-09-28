library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Title
  titlePanel("The Moran Process for 2x2 Symmetric Games"),
  
  # Sidebar for Parameter Input
  sidebarLayout(
      
      sidebarPanel(
    
        selectInput("interaction_structure", "Game Type:`",
            c("Degenerate Game", "Dominating Strategy Game", "Coordination Game", "Anticoordination Game", "Custom (manually change payoffs)"),
            selected = "Anticoordination Game"
          ),
        
        # 2X2 Game Payoff Input Fields
        strong("Payoff Matrix:"),
        
          # 1st Row: Types
          fluidRow(
            column(4),
            column(4,
                   p("Type A", align="center", style="margin-top:7px")
                   ),
            column(4,
                   p("Type B", align="center", style="margin-top:7px")
            )),

          # 2nd Row: Payoffs to Type A
          fluidRow(
            column(4,
                   p("Type A", align="right", style="margin-top:7px;")
                   ),
            column(4,
                  numericInput("a", 
                                label = NULL, 
                                value = 1,
                                min = 1)
            ),
            column(4,
                   numericInput("b", 
                                label = NULL, 
                                value = 3,
                                min = 1)
            )),
        
          # 3rd Row: Payoffs to Type B
          fluidRow(
            column(4,
                   p("Type B", align="right", style="margin-top:7px;")
                  ),
            column(4,      
                   numericInput("c", 
                                label = NULL, 
                                value = 2,
                                min = 1)
            ), 
            column(4,
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
                  tabPanel("Invasion Dynamics & Replacement Probabilities", 
                           h4("Invasion Dynamics"),
                             p("Invasion dynamics for A:"),
                               textOutput("InvDynA1"),
                               textOutput("InvDynA2"),
                             p("Invasion dynamics for B:"),
                               textOutput("InvDynB1"),
                               textOutput("InvDynB2"),   
                           h4("Replacement Probabilities")
                           ) 
      )
    )
  )
))
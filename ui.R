# FREQUENCY-DEPENDENT MORAN PROCESS
# FOR 2x2 SYMMETRIC GAMES
# << UI >>
# by Aydin Mohseni


# Load the shiny GUI library
library(shiny)
library(ggplot2)

# Set encoding for special characters
Sys.setlocale("LC_ALL", "fr_FR.UTF-8")

# Define UI for application
shinyUI(fluidPage(
  
  # CSS for visual
  includeCSS("www/style.css"),
  
  # Title
  titlePanel("Frequency-Dependent Moran Process for 2x2 Symmetric Games"),
  
  # Load MathJax 
  withMathJax(),
  
  fluidRow(
    style = "background-color:#F2F2F2; margin-top: 30px; margin-bottom: 30px; padding: 10px", 
    column(
      width = 3,
      # Introduction text:
      p(
        tags$b("Description:"),
        "The frequency-dependent Moran process is a standard stochastic model of biological and cultural evolution [Moran, 1962; Nowak 2006]. 
        Here, we model the evolution of a population whose interactions are described by 2x2 symmetric games."
      )
    ),
    column(
      width = 3,
      # Introduction text:
      p(
        "In the top plot, you can see the stationary distribution of the process.
        this describes the frequency of time the process will spend in each population state,
        in the long run."),
      p(
        "In the second plot, you can see the trajectory of evolution
        for a single population."
      )
      ),
    column(
      width = 3,
      p(
        tags$b("Instructions:"),
        "To see the changes in the long run behavior or evolution 
        (top plot), change the parameters in the left panel
        and the plot will automatically update to relfect these changes."
      )
      ),
    column(
      width = 3,
      # Instructions text
      p(
        "To simulate evolutionary trajectories
        (bottom plot), 
        click 'RUN EVOLUTION' at the bottom of the left panel."
      ),
      p(
        "To see various predictions of the outcome of evolution,
        select the 'Invasion, Replacement & Equilibria' tab."
      )
      )
    ),
  
  # Sidebar for Parameter Input
  sidebarLayout(
      
      sidebarPanel(
    
        selectInput("interaction_structure", "Game Type:",
            c("Neutral Game", "Dominating Strategy Game", "Coordination Game", "Anti-Coordination Game", "Custom"),
            selected = "Custom"
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
                    value = 80),
        
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
                    value = .25),
        
        # Simulate Single Population Button
        tags$head(tags$script(src = "message-handler.js")),
        p(actionButton("simulateSinglePopulation", "RUN EVOLUTION"), align = "center"),
        
        # Simulation Time Slider
        sliderInput("simulationTime",
                    "Simulation Time:",
                    min = 1,
                    max = 10000,
                    value = 4000)
      ),
  
  # Main Panel with Stationary Distribution + Simulation & Stats Panels
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Stationary Distribution & Simulation", 
                           plotOutput("stationaryDistribution"), 
                           plotOutput("singlePopulationSimulation")),
                  tabPanel("Invasion, Replacement & Equilibria", 
                           fluidRow(
                             column(
                               6,
                               h4("Invasion dynamics for A:", style = "margin-top:25px;"),
                               verbatimTextOutput("InvDynA1"),
                               verbatimTextOutput("InvDynA2"),
                               br(),
                               h4("Replacement Probabilities for A:"),
                               verbatimTextOutput("RepProbA1"),
                               verbatimTextOutput("RepProbA2"),
                               br()
                             ),
                             column(
                               6,
                               h4("Invasion dynamics for B:", style ="margin-top:25px;"),
                               verbatimTextOutput("InvDynB1"),
                               verbatimTextOutput("InvDynB2"),
                               br(),
                               h4("Replacement Probabilities for B:"),
                               verbatimTextOutput("RepProbB1"),
                               verbatimTextOutput("RepProbB2")
                             )),
                           fluidRow(
                             column(
                               4,
                               h4("Fixed Points of the Replicator"), 
                               verbatimTextOutput("FixedPoints")
                             ),
                             column(
                               4,
                               h4("Symmetric Nash Equilibria"),
                               verbatimTextOutput("Nash")
                             ),                             
                             column(
                               4,
                               h4("Evolutionarily Stable States"),
                               verbatimTextOutput("ESS")
                             )                             
                           )
                  )
      )
    )
  )
))
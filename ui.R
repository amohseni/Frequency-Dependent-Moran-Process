library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("The Moran Process for 2x2 Symmetric Games"),
  
  # Sidebar with a slider input for Population Size
  sidebarLayout(
    sidebarPanel(
      
    strong("Payoff / Fitness Matrix:"),
      fluidRow(
        column(6,
               numericInput("a", 
                   label = NULL, 
                   value = 1,
                   min = 0)
        ),
        column(6,
               numericInput("b", 
                   label = NULL, 
                   value = 3,
                   min = 0)
        )),

      fluidRow(
        column(6,      
              numericInput("c", 
                   label = NULL, 
                   value = 2,
                   min = 0)
        ), 
        column(6,
              numericInput("d", 
                   label = NULL, 
                   value = 1,
                   min = 0)
        )),
      
      sliderInput("populationSize",
                  "Population Size:",
                  min = 10,
                  max = 100,
                  value = 100),

        sliderInput("mutationRate",
                    "Mutation Rate:",
                    min = 0,
                    max = .1,
                    value = .05),

        sliderInput("intensityOfSelection",
                      "Intensity of Selection:",
                      min = 0,
                      max = 1,
                      value = .1),
        
        sliderInput("probabilityMax",
                    "Display Max Probability:",
                    min = 0,
                    max = 1,
                    value = .05),
    
        tags$head(tags$script(src = "message-handler.js")),
        p(actionButton("simulateSinglePopulation", "Simulate Single Population"), align = "center"),
    
        sliderInput("simulationTime",
                    "Simulation Time:",
                    min = 1,
                    max = 10000,
                    value = 1000)
    
        ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("stationaryDistribution"),
      plotOutput("singlePopulationSimulation")
    )
  )
))
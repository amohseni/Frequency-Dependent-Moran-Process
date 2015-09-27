# COMPUTING THE MORAN PROCESS
# For 2x2 Symmetric Games

# Load the shiny GUI library, 
# and the matrix exponential package.
library(shiny)
library(expm)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # The expression that generates our histogram is wrapped in a call to renderPlot to indicate that:
  #  1) It is "reactive" and therefore should re-execute automatically when inputs change
  #  2) Its output type is a plot
  output$stationaryDistribution <- renderPlot({
    
# FINDING THE STATIONARY DISTRIBUION OF THE MORAN PROCESS   
    # Let G be a 2x2 symmetric game 
    # with two players,
    # pure strategies A and B,
    # and payoff matrix M = matrix(c(a,b,c, d), nrow=2, ncol=2, byrow=TRUE).

    # Then the payoff to each combination of actions would be given by
    # U(A,A)=a, U(A,B)=b, U(B,A)=c, U(B,B)=d. 
    
    # The payoff values a, b, c, d are input from the user interface
    a <- as.numeric(input$a)
    b <- as.numeric(input$b)
    c <- as.numeric(input$c)
    d <- as.numeric(input$d)    

    # The five traditional game types are provided as defaults:
    # The Degenerate Game (0)
    #assignVec(c('a','b', 'c', 'd'), c(1, 1, 1, 1), envir = .GlobalEnv)
    # Dominating Strategy Game [e.g., Prisoner's Dilemma]
    # assignVec(c('a','b', 'c', 'd'), c(2, 0, 3, 1), envir = .GlobalEnv)
    # Coordination Game [e.g., Stag Hunt]
    # assignVec(c('a','b', 'c', 'd'), c(3, 0, 2, 2), envir = .GlobalEnv)  
    # Anticoordination Game [e.g., Hawk-Dove]
    # assignVec(c('a','b', 'c', 'd'), c(0, 3, 1, 2), envir = .GlobalEnv)
    #assignVec(c('a','b', 'c', 'd'), c(1, 3, 2, 1), envir = .GlobalEnv)
        
    # Population Size (we are interested in 'Large Populations' where N -> ∞)
    N <- as.numeric(input$populationSize)
    
    # i = number of A-types
    # N-i = number of B-types
    
    # Pr(A-type interacts with A-type) = (i-1) / (N-1)
    # Pr(A-type interacts with B-type) = (N-i) / (N-1)  
    # Pr(B-type interacts with A-type) = (i) / (N-1)	
    # Pr(B-type interacts with B-type) = (N-i-1) / (N-1)
    
    # Expected Payoff to Each Strategy
    FA <- function(i) (a*(i-1) + b*(N-i)) / (N-1)
    FB <- function(i) (c*(i) + d*(N-i-1)) / (N-1)
    
    # Intensity of Selection (we are interested in in 'Weak Selection' where w -> 0)
    w <- as.numeric(input$intensityOfSelection)
    
    # Fitness (Reproductive/Imitative Success) of Each Strategy
    fA <- function(i) (1 - w + w * FA(i))
    fB <- function(i) (1 - w + w * FB(i))

    # Mutation Term
    µ <- as.numeric(input$mutationRate)
    
    #Transition Proabilities for Moran Process with Mutation
    # Pr(j -> j+1)
    PforwardM <- function(i) (1-µ)*((i * fA(i) / P(i)) * ((N-i) / N)) + µ*(((N-i) * fB(i) / P(i)) * ((N-i) / N))
    # Pr(j -> j-1)		
    PbackM <- function(i) (1-µ)*(((N-i) * fB(i) / P(i)) * (i / N)) + µ*((i * fA(i) / P(i)) * (i / N))
    # Pr(j -> j)		
    PstayM <- function(i) 1 - (PforwardM(i) + PbackM(i))
    
    # The MPM Transition Matrix :: (MPM)
    MPM <- matrix(nrow = N+1, ncol = N+1, byrow = TRUE)

    # We relabel the row and columns to correspond to the number of A-types in the N-size population
    rownames(MPM) = c(0:N)
    colnames(MPM) = c(0:N)
    
    MPM <- outer(0:N, 0:N, FUN=function(r,c) 
      ifelse(c == r-1, PbackM(r),
             ifelse(c == r, PstayM(r), 
                    ifelse(c == r+1, PforwardM(r),
                           0))))

    # Denote MPM^∞ := MPMlim
    MPMlim <- MPM %^% 10000000

    # We print the title of the stationary distribution
    print("The Moran Process with Mutation Under the 2x2 Symmetric Game M")
    # then print the stationary disribution π for the MPM (and round the entries to the N/2 digit):
    π <- MPMlim[round(N/2),]
    print("Stationary Distribution π of the MPM")
    print(π)
    
    # Get the max (y-axis) probability value from the interace
    probMax <- as.numeric(input$probabilityMax)

    # Draw the Barplot
    barplot(π, col=rgb(82, 140, 202, max = 255), main="Stationary Distribution", ylab=expression('Probability µ'[i]), xlab="# of A-types in the population (i)", ylim=c(0,probMax), space=0)
    # Establish the interval between x-axis marks as a function of N.
    xInterval <- round(N/10, digits = 0)
    axis(side=1, at=seq(0,N, by = xInterval))
    box()
  })

  

  
# SIMULATING THE MORAN PROCESS FOR A SINGLE POPULATION

  # We create the function that simulates the population from a random initial condition

  output$singlePopulationSimulation <- renderPlot({
    simulationResetVariable<-input$simulateSinglePopulation
    # Inputs: 
    # Duration of simulation/number of generations of birth
    t <- as.numeric(input$simulationTime)
    # Population Size
    N <- as.numeric(input$populationSize)
    # Mutation Rate
    µ <- as.numeric(input$mutationRate)    
    # Intensity of Selection
    w <- as.numeric(input$intensityOfSelection)    
    
    #Transition Proabilities for Moran Process with Mutation
    # Pr(j -> j+1)
    PforwardM <- function(i) (1-µ)*((i * fA(i) / P(i)) * ((N-i) / N)) + µ*(((N-i) * fB(i) / P(i)) * ((N-i) / N))
    # Pr(j -> j-1)		
    PbackM <- function(i) (1-µ)*(((N-i) * fB(i) / P(i)) * (i / N)) + µ*((i * fA(i) / P(i)) * (i / N))
    # Pr(j -> j)		
    PstayM <- function(i) 1 - (PforwardM(i) + PbackM(i))
        
    # The MPM Transition Matrix :: (MPM)
    MPM <- matrix(nrow = N+1, ncol = N+1, byrow = TRUE)

    # We relabel the row and columns to correspond to the number of A-types in the N-size population
    rownames(MPM) = c(0:N)
    colnames(MPM) = c(0:N)
    
    MPM <- outer(0:N, 0:N, FUN=function(r,c) 
      ifelse(c == r-1, PbackM(r),
             ifelse(c == r, PstayM(r), 
                    ifelse(c == r+1, PforwardM(r),
                           0))))
    
    # Output: A vector of length t
    MPM.sim <- function(t, MPM, πNought) {
      sim <- as.numeric(t)
      if (missing(πNought)) { # If the initial state is not specified
        sim[1] <- (sample(1:(N+1),1) - 1) # Then take a random initial state 
      }else{sim[1]<-πNought} # If the initial state is specified
      for(i in 2:t){ # For each step of the simulation after the initial state
        newstate <- sample(1:(N+1),1,prob=MPM[sim[i-1],]) # The transition to the next state is given by the transition matrix MPM
        sim[i] <- newstate
      } 
      sim 
    }
    
    # Plot the simulation
    plot(MPM.sim(t,MPM), type="l", main="Single Population Simulation", xlab="Time (t)", ylab="# of A-types in the population (i)", ylim=c(0,N), col=rgb(82, 140, 202, max = 255))
  }  
  )
  
})
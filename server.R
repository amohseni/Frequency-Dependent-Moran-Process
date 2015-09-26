# COMPUTING THE MORAN PROCESS
# For 2x2 Symmetric Games
# ***Recall: Load the 'expm' package for Matrix exponentiation.*** 

# Make minor changes
# Call the shiny GUI library
library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # The expression that generates our histogram is wrapped in a call to renderPlot to indicate that:
  #  1) It is "reactive" and therefore should re-execute automatically when inputs change
  #  2) Its output type is a plot
  
  output$stationaryDistribution <- renderPlot({
    
    # Let G be a 2x2 symmetric game 
    # with two players
    # pure strategies A, B
    # and payoff matrix M = matrix(c(a,b,c, d), nrow=2, ncol=2, byrow=TRUE).
    # Then the payoff to each combination of actions would be given by
    # U(A,A)=a, U(A,B)=b, U(B,A)=c, U(B,B)=d. 
    
    # We assign test payoff values for different games types as follows:
    # The Degenerate Game (0)
    #assignVec(c('a','b', 'c', 'd'), c(1, 1, 1, 1), envir = .GlobalEnv)
    # Prisoner's Dilemma (PD)
    # assignVec(c('a','b', 'c', 'd'), c(2, 0, 3, 1), envir = .GlobalEnv)
    # Stag Hunt (SH)
    # assignVec(c('a','b', 'c', 'd'), c(3, 0, 2, 2), envir = .GlobalEnv)  
    # Hawk-Dove (HD)
    # assignVec(c('a','b', 'c', 'd'), c(0, 3, 1, 2), envir = .GlobalEnv)
    # Anticoordination Game (HD)
    #assignVec(c('a','b', 'c', 'd'), c(1, 3, 2, 1), envir = .GlobalEnv)
    a <- as.numeric(input$a)
    b <- as.numeric(input$b)
    c <- as.numeric(input$c)
    d <- as.numeric(input$d)    
    
    # Population Size (we are interested in 'Large Populations' where N -> ∞)
    N <- as.numeric(input$populationSize)
    
    # i = number of type A
    # N-i = number of type B
    
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
    
    #Transition Proabilities for Moran Process with Mutation
    # Mutation Term
    µ <- as.numeric(input$mutationRate)
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
    # We label the matrix, and format P∞ to round numbers off at the 3rd digit 
    print("The Moran Process with Mutation Under the 2x2 Symmetric Game M")
    
    # Finally, we print the stationary disribution π for the MPM:
    π <- MPMlim[round(N/2),]
    print("Stationary Distribution π of the MPM")
    print(π)
    
    # Draw the Barplot
    probMax <- as.numeric(input$probabilityMax)
    barplot(π, col = "gray", main="Stationary Distribution", ylab=expression('Probability µ'[i]), xlab="# of A-types in the population (i)", ylim=c(0,probMax), space=0)
    # Establish the interval between x-axis marks as a function of N.
    xInterval <- round(N/10, digits = 0)
    axis(side=1, at=seq(0,N, by = xInterval))
    box()
#    hist(x, breaks = bins, col = 'skyblue', border = 'white')
  })

  
  
  
  
# SIMULATING THE MORAN PROCESS
# For 2x2 Symmetric Games    

  randomVals <- eventReactive(input$simulateSinglePopulation, {})
    
  output$singlePopulationSimulation <- renderPlot({
    simulationResetVariable<-input$simulateSinglePopulation
    # Function name: MP.sim 
    # Input: t (number of steps/length of simulation), P (transition matrix), π0 (initial state)
    t <- as.numeric(input$simulationTime)
    # Population Size (we are interested in 'Large Populations' where N -> ∞)
    N <- as.numeric(input$populationSize)
    # Mutation Term
    µ <- as.numeric(input$mutationRate)    
    # Intensity of Selection (we are interested in in 'Weak Selection' where w -> 0)
    w <- as.numeric(input$intensityOfSelection)    
    
    #Transition Proabilities for Moran Process with Mutation
    # Mutation Term
    µ <- as.numeric(input$mutationRate)
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
    
    # Output: a vector of length t
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
    plot(MPM.sim(t,MPM), type="l", main="Single Population Simulation", xlab="Time (t)", ylab="# of A-types in the population (i)", ylim=c(0,N))
  }  
  )
  
})
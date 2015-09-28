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
    # Let Γ be a 2x2 symmetric game 
    # with two players,
    # pure strategies A and B,
    # and payoff matrix M = matrix(c(a,b,c, d), nrow=2, ncol=2, byrow=TRUE).

    # Then the payoff to each combination of actions would be given by
    # U(A,A)=a, U(A,B)=b, U(B,A)=c, U(B,B)=d. 

    # The five traditional game types and a customizable option are provided as default payoff settings:
    if (input$interaction_structure=='Anticoordination Game') {
      updateNumericInput(session, "a", value =1)
      updateNumericInput(session, "b", value =3)
      updateNumericInput(session, "c", value =2)
      updateNumericInput(session, "d", value =1)
    }
    if (input$interaction_structure=='Coordination Game') {
      updateNumericInput(session, "a", value = 3)
      updateNumericInput(session, "b", value = 0)
      updateNumericInput(session, "c", value = 1)
      updateNumericInput(session, "d", value = 2)
    }
    if (input$interaction_structure=='Dominating Strategy Game') {
      updateNumericInput(session, "a", value = 3)
      updateNumericInput(session, "b", value = 1)
      updateNumericInput(session, "c", value = 2)
      updateNumericInput(session, "d", value = 0)
    }
    if (input$interaction_structure=='Degenerate Game') {
      updateNumericInput(session, "a", value = 1)
      updateNumericInput(session, "b", value = 1)
      updateNumericInput(session, "c", value = 1)
      updateNumericInput(session, "d", value = 1)
    }
    if (input$interaction_structure=='Custom') {
    }
    
    # The payoff values a, b, c, d are input from the user interface
    a <- as.numeric(input$a)
    b <- as.numeric(input$b)
    c <- as.numeric(input$c)
    d <- as.numeric(input$d)    
        
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
    # Population Mean Fitness
    P <- function(i) (i * fA(i) + (N-i) * fB(i))
    
    # Mutation Rate
    η <- as.numeric(input$mutationRate)
    
    #Transition Proabilities for Moran Process with Mutation
    # Pr(j -> j+1)
    PforwardM <- function(i) (1-η)*((i * fA(i) / P(i)) * ((N-i) / N)) + η*(((N-i) * fB(i) / P(i)) * ((N-i) / N))
    # Pr(j -> j-1)		
    PbackM <- function(i) (1-η)*(((N-i) * fB(i) / P(i)) * (i / N)) + η*((i * fA(i) / P(i)) * (i / N))
    # Pr(j -> j)		
    PstayM <- function(i) 1 - (PforwardM(i) + PbackM(i))
    
    # The MPM Transition Matrix :: (MPM)
    MPM <- matrix(nrow = N+1, ncol = N+1, byrow = TRUE)

    # We relabel the row and columns to correspond to the number of A-types in the N-size population
    rownames(MPM) = c(0:N)
    colnames(MPM) = c(0:N)
    
    # We compute the transition matrix by recursive applications of the transition probabilities
    MPM <- outer(0:N, 0:N, FUN=function(r,c) 
      ifelse(c == r-1, PbackM(r),
             ifelse(c == r, PstayM(r), 
                    ifelse(c == r+1, PforwardM(r),
                           0))))

    # We approximate MPM^∞ in order to estimate the Stationary Distribution µ
    # which will be the vector composing every row of MPM^∞
    
    # Note: We know that the stationary distribution µ exists because the addition of mutation makes the Markov process Ergodic
    # That is: Where the Moran Process is already finite, and aperiodic, 
    # it is now irreducible (has only one recursive class) as every state communicates with every other (is in a single class), 
    # and every state will be visited an infinite number of times in the limit.
    # In the absence of mutation, there are were two (singleton) recursive classes
    # corresponding to the two absorbing states where the population is all A-type or B-type.
    # It follows from being ergodic that the limit distribution π is independent of any initial distribution π(0).
    
    # Denote MPM^∞ := MPMlim
    MPMlim <- MPM %^% 10000000
    # Select any row from the MPM^∞ matrix to get the stationary disribution µ for the MPM
    µ <- MPMlim[round(N/2),]
    # For testing purposes, print the stationary distribution µ
    print("Stationary Distribution µ of the MPM")
    print(µ)
    
    # Get the max (y-axis) probability value from the interace
    probMax <- as.numeric(input$probabilityMax)

    # Draw the barplot
    barplot(µ, col=rgb(82, 140, 202, max = 255), main="Stationary (Limit) Distribution", ylab=expression("Probability (µ'[i])"), xlab="# of A-types in the population (i)", ylim=c(0,probMax), space=0)
    # The interval between ticks on the x-axis shiould be a function of N.
    xInterval <- if (N>9) { round(N/10, digits = 0) } else { round(1+N/10, digits = 0) } # the else is to prevent the intervals from going to zero at small population sizes, and outputting an error
    axis(side=1, at=seq(0,N, by = xInterval))
    # Remove the box around the barplot
    box() 
   
     
# COMPUTING THE INVASION PROBABILITIES AND FIXATION DYNAMICS
    # We compute the invasion probability for each strategy.
    # We compare the fitness of each strategy, fA(i)  and fB(i), to see which strategy is favored by selection given that the population is in a state with i A-types. We define this as follows:
    h <- function(i) (fA(i)-fB(i))
    # Invasion dynamics then can be determined by evaluating the sign of h(1) and h(N-1), 
    # where h(1) is the difference in fitness between A-types and B-types where there is a single A-type mutant in the population, 
    # and h(N-1) is the difference in fitness when there is a single B-type mutant in the population.
    # If the fitness of a single mutant is less than the fitness of the resident population, then selection opposes invasion. 
    # Conversely, a strategy favored by selection for invading is likely to find a foothold in the population. 
    print("Invasion dynamics for A:")
    print(h(1))
    output$InvDynA1 <- renderText({ toString(h(1)) })
    if (h(1)>0) {print("Selection favors A invading B")} else if (h(1)<0) {print("Selection opposes A invading B")} else {print("Invasion is Selection regarding invasion by A")}
    output$InvDynA1 <- renderText({ toString(h(1)) })
    
    print("Invasion dynamics for B:")
     print(h(N-1))
    if (h(N-1)>0) {print("Selection opposes B invading A")} else if (h(N-1)<0) {print("Selection favors B invading A")} else {print("Selection is neutral regarding invasion by B")}
    
    # Next, we compute the fixation probabilities, ρAB and ρBA, 
    # corresponding to the probability that the process transitions from a single A-type mutant to an all-A population 
    # and that the process transitions from a single B-type mutant to an all-B population, respectively. 
    # (To do this we creat a vector of the ratios of the fixation probabilities fB(i)/fA(i),
    # and proceed to compute the culative sum of the cumulative products of transitions over all population states.)
    X <- c(1:N)
    HBA <- function(i) (fB(i)/fA(i))
    HBAvector <-sapply(X, HBA)
    HBAproductsVector <- cumprod(HBAvector)
    HBAsumsOfProductsVector <- cumsum(HBAproductsVector)
    ρAB <- 1/(1 + HBAsumsOfProductsVector[N-1])
    
    Y <- c(1:N)
    HAB <- function(i) (fA(i)/fB(i))
    HABvector <-sapply(Y, HAB)
    HABproductsVector <- cumprod(HABvector)
    HABsumsOfProductsVector <- cumsum(HABproductsVector)
    ρBA <- 1/(1 + HABsumsOfProductsVector[N-1])
    
    # We compare the fixation probabilities of each strategy to that of a neutral mutant
    # (whose fixation probability is 1/N). 
    # If the fixation probability of the mutant is less than that of a neutral mutant, 
    # then we say selection opposes fixation. If the fixation probability is greater than that of a neutral mutant, then we say selection favors fixation.
    print("Replacement dynamics for A:")
    print(ρAB)
    if (ρAB > (1/N)) {print("Selection favors A replacing B")} else if (ρAB < (1/N)) {print("Selection opposes A replacing B")} else {print("Selection is neutral regarding replacement by A")}
    print("Replacement dynamics for B:")
    print(ρBA)
    if (ρBA > (1/N)) {print("Selection favors B replacing A")} else if (ρBA < (1/N)) {print("Selection opposes B replacing A")} else {print("Selection is neutral regarding replacement by B")}
        
  })

  
# SIMULATING THE MORAN PROCESS FOR A SINGLE POPULATION
  # We create the function that simulates the population from a random initial population composition
  output$singlePopulationSimulation <- renderPlot({
    simulationResetVariable<-input$simulateSinglePopulation

    # INPUTS
      # The payoffs a, b, c, d, from the game
      a <- as.numeric(input$a)
      b <- as.numeric(input$b)
      c <- as.numeric(input$c)
      d <- as.numeric(input$d) 
      
      # Duration of simulation/number of generations of birth & death
      t <- as.numeric(input$simulationTime)
      
      # Population Size
      N <- as.numeric(input$populationSize)
      # Intensity of Selection
      w <- as.numeric(input$intensityOfSelection)
      # Mutation Rate
      η <- as.numeric(input$mutationRate)

      # The MPM Transition Matrix :: (MPM)
      MPM <- matrix(nrow = N+1, ncol = N+1, byrow = TRUE)
      
      # We relabel the row and columns to correspond to the number of A-types in the N-size population
      rownames(MPM) = c(0:N)
      colnames(MPM) = c(0:N)
      
      # We compute the transition matrix by recursive applications of the transition probabilities
      MPM <- outer(0:N, 0:N, FUN=function(r,c) 
        ifelse(c == r-1, PbackM(r),
               ifelse(c == r, PstayM(r), 
                      ifelse(c == r+1, PforwardM(r),
                             0))))
      
    # Simulation Output: A vector of length t
    MPM.sim <- function(t, MPM, πNought) { # The simulation function takes as arguments: duration, the MPM, and some initial condition πNought
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
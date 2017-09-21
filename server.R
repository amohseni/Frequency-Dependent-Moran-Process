# FREQUENCY-DEPENDENT MORAN PROCESS
# FOR 2x2 SYMMETRIC GAMES
# << SERVER >>
# by Aydin Mohseni


# Load packages
library(shiny, expm, ggplot2)

# Define server logic
shinyServer(function(input, output, session) {
  
  # Let Γ be a 2x2 symmetric game
  # with two players,
  # pure strategies A and B,
  # and payoff matrix M = matrix(c(a,b,c, d), nrow=2, ncol=2, byrow=TRUE).
  
  # Then the payoff to each combination of actions would be given by
  # U(A,A)=a, U(A,B)=b, U(B,A)=c, U(B,B)=d.
  
  # Four traditional game types and a fifth customizable option are set as our possible payoffs:
  # 1. Anticoordination Game (Polymorphic case)
  observe({
    if (input$interaction_structure == 'Anti-Coordination Game') {
      updateNumericInput(session, "a", value = 1)
      updateNumericInput(session, "b", value = 3)
      updateNumericInput(session, "c", value = 2)
      updateNumericInput(session, "d", value = 1)
    }
    # 2. Coordination Game (Bistable case)
    if (input$interaction_structure == 'Coordination Game') {
      updateNumericInput(session, "a", value = 3)
      updateNumericInput(session, "b", value = 1)
      updateNumericInput(session, "c", value = 1)
      updateNumericInput(session, "d", value = 2)
    }
    # 3. Dominating Strategy Game (A dominates B)
    if (input$interaction_structure == 'Dominating Strategy Game') {
      updateNumericInput(session, "a", value = 3)
      updateNumericInput(session, "b", value = 2)
      updateNumericInput(session, "c", value = 2)
      updateNumericInput(session, "d", value = 1)
    }
    # 4. Degenerate Game (Neutral case)
    if (input$interaction_structure == 'Neutral Game') {
      updateNumericInput(session, "a", value = 1)
      updateNumericInput(session, "b", value = 1)
      updateNumericInput(session, "c", value = 1)
      updateNumericInput(session, "d", value = 1)
    }
    # 5. Custom Game
    if (input$interaction_structure == 'Custom') {
    }
    
  })
  
  # Compute the MORAN PROCESS transition matrix
  # as well as invasion dynamics, and replacement probabilities
  computeDynamics <- reactive({
    
    # Import payoff values a, b, c, d 
    a <- as.numeric(input$a)
    b <- as.numeric(input$b)
    c <- as.numeric(input$c)
    d <- as.numeric(input$d)
    
    # Population Size 
    N <- as.numeric(input$populationSize)
    
    # i = number of A-types
    # N-i = number of B-types
    
    # Pr(A-type interacts with A-type) = (i-1) / (N-1)
    # Pr(A-type interacts with B-type) = (N-i) / (N-1)
    # Pr(B-type interacts with A-type) = (i) / (N-1)
    # Pr(B-type interacts with B-type) = (N-i-1) / (N-1)
    
    # Expected Payoff to Each Strategy
    FA <- function(i)
      (a * (i - 1) + b * (N - i)) / (N - 1)
    FB <- function(i)
      (c * (i) + d * (N - i - 1)) / (N - 1)
    
    # Intensity of Selection (Contribution of the Game to an Individual's Net Fitness)
    w <- as.numeric(input$intensityOfSelection)
    
    # Fitness (Reproductive/Imitative Success) of Each Strategy
    fA <- function(i)
      (1 - w + w * FA(i))
    fB <- function(i)
      (1 - w + w * FB(i))
    # Population Mean Fitness
    P <- function(i)
      (i * fA(i) + (N - i) * fB(i))
    
    # Mutation Rate
    η   <- as.numeric(input$mutationRate)
    
    #Transition Proabilities for Moran Process with Mutation
    # Pr(j -> j+1)
    PforwardM <-
      function(i)
        (1 -  η) * ((i * fA(i) / P(i)) * ((N - i) / N)) +   η  * (((N - i) * fB(i) / P(i)) * ((N -
                                                                                                 i) / N))
    # Pr(j -> j-1)
    PbackM <-
      function(i)
        (1 -  η) * (((N - i) * fB(i) / P(i)) * (i / N)) +   η  * ((i * fA(i) / P(i)) * (i / N))
    # Pr(j -> j)
    PstayM <- function(i)
      1 - (PforwardM(i) + PbackM(i))
    
    # The Moran Process with Mutation (MPM) Transition Matrix 
    MPM <- matrix(nrow = N + 1,
                  ncol = N + 1,
                  byrow = TRUE)
    
    # We relabel the row and columns to correspond to the number of A-types in the N-size population
    rownames(MPM) = c(0:N)
    colnames(MPM) = c(0:N)
    
    # TRANSITION MATRIX
    # We compute the transition matrix by recursive applications of the transition probabilities
    MPM <- outer(
      0:N,
      0:N,
      FUN = function(r, c)
        ifelse(c == r - 1, PbackM(r),
               ifelse(
                 c == r, PstayM(r),
                 ifelse(c == r + 1, PforwardM(r),
                        0)
               ))
    )
    
    # STATIONARY DISTRIBUTION  
    
    # Note: We know that the stationary distribution µ exists because the addition of mutation makes the Markov process Ergodic
    # That is: Where the Moran Process is already finite, and aperiodic,
    # it is now irreducible (has only one recursive class) as every state communicates with every other (is in a single class),
    # and every state will be visited an infinite number of times in the limit.
    # In the absence of mutation, there are were two (singleton) recursive classes
    # corresponding to the two absorbing states where the population is all A-type or B-type.
    # It follows from being ergodic that the limit distribution π is independent of any initial distribution π(0).
    
    # We compute the stationary distribution of the process
    # First, we calculate µ_0 = ( Σ^{N}_{k=0} Π^{k}_{i=1} PforwardM_{i-1} / PbackM_{i} ) ^ -1
    Mu0Vector <- c() # Empty vector, in which to store each product
    # Empty matrix, in which to store each element of the products
    Mu0Matrix <- matrix(data = 1,
                        nrow = N + 1,
                        ncol = N + 1)
    for (k in 2:(N + 1)) {
      for (i in 2:k) {
        Mu0Matrix[k, i - 1] <- MPM[i - 1, i] / MPM[i, i - 1]
      }
    }
    # Take the product of the rows of Mu0Matrix
    for (i in 1:(N + 1)) {
      Mu0Vector[i] <- prod(Mu0Matrix[i,])
    }
    # Compute µ_0
    Mu0 <- sum(Mu0Vector) ^ -1
    
    # Now, we calculate µ_k = µ_0 * Π^{k}_{i=1} PforwardM_{i-1} / PbackM_{i} )
    MuVector <- c()
    # Empty matrix, in which to store each element of the products
    MuMatrix <- matrix(data = 1,
                       nrow = N + 1,
                       ncol = N + 1)
    for (k in 2:(N + 1)) {
      for (i in 2:k) {
        MuMatrix[k, i - 1] <- MPM[i - 1, i] / MPM[i, i - 1]
      }
    }
    for (i in 1:(N + 1)) {
      MuVector[i] <- Mu0 * prod(MuMatrix[i,])
    }
    
    
    # INVASION DYNAMICS and REPLACEMENT PROBABILITIES
    # We compute the invasion probability for each strategy.
    # We compare the fitness of each strategy, fA(i)  and fB(i), to see which strategy is favored by selection given that the population is in a state with i A-types. We define this as follows:
    h <- function(i)
      (fA(i) - fB(i))
    # Invasion dynamics then can be determined by evaluating the sign of h(1) and h(N-1),
    # where h(1) is the difference in fitness between A-types and B-types where there is a single A-type mutant in the population,
    # and h(N-1) is the difference in fitness when there is a single B-type mutant in the population.
    # If the fitness of a single mutant is less than the fitness of the resident population, then selection opposes invasion.
    # Conversely, a strategy favored by selection for invading is likely to find a foothold in the population.
    
    # Invasion dynamics for A:
    if (h(1) > 0) {
      InvDynA1 <- "Selection favors A invading B"
    }
    else if (h(1) < 0) {
      InvDynA1 <- "Selection opposes A invading B"
    }
    else {
      InvDynA1 <- "Invasion is neutral regarding invasion by A"
    }
    InvDynA2 <- paste("h(1)", round(h(1), digits = 4), sep = " = ")
    
    # Invasion dynamics for B:
    if (h(N - 1) > 0) {
      InvDynB1 <- "Selection opposes B invading A"
    }
    else if (h(N - 1) < 0) {
      InvDynB1 <- "Selection favors B invading A"
    }
    else {
      InvDynB1 <- "Selection is neutral regarding invasion by B"
    }
    InvDynB2 <- paste("h(N-1)", round(h(N - 1), digits = 4), sep = " = ")
    
    # Next, we compute the fixation probabilities, ρAB and ρBA,
    # corresponding to the probability that the process transitions from a single A-type mutant to an all-A population
    # and that the process transitions from a single B-type mutant to an all-B population, respectively.
    # (To do this we creat a vector of the ratios of the fixation probabilities fB(i)/fA(i),
    # and proceed to compute the culative sum of the cumulative products of transitions over all population states.)
    X <- c(1:N)
    
    HBA <- function(i)
      (fB(i) / fA(i))
    HBAvector <- sapply(X, HBA)
    HBAproductsVector <- cumprod(HBAvector)
    HBAsumsOfProductsVector <- cumsum(HBAproductsVector)
    ρAB <- 1 / (1 + HBAsumsOfProductsVector[N - 1])
    
    HAB <- function(i)
      (fA(i) / fB(i))
    HABvector <- sapply(X, HAB)
    HABproductsVector <- cumprod(HABvector)
    HABsumsOfProductsVector <- cumsum(HABproductsVector)
    ρBA <- 1 / (1 + HABsumsOfProductsVector[N - 1])
    
    # We compare the fixation probabilities of each strategy to that of a neutral mutant
    # (whose fixation probability is 1/N).
    # If the fixation probability of the mutant is less than that of a neutral mutant,
    # then we say selection opposes fixation. If the fixation probability is greater than that of a neutral mutant, then we say selection favors fixation.
    
    # Replacement dynamics for A:
    if (ρAB > (1 / N)) {
      RepProbA1 <- "Selection favors A replacing B"
    }
    else if (ρAB < (1 / N)) {
      RepProbA1 <- "Selection opposes A replacing B"
    }
    else {
      RepProbA1 <- "Selection is neutral regarding replacement by A"
    }
    RepProbA2 <- paste("ρAB", round(ρAB, digits = 4), sep = " = ")
    
    # "Replacement dynamics for B:
    if (ρBA > (1 / N)) {
      RepProbB1 <- "Selection favors B replacing A"
    }
    else if (ρBA < (1 / N)) {
      RepProbB1 <- "Selection opposes B replacing A"
    }
    else {
      RepProbB1 <- "Selection is neutral regarding replacement by B"
    }
    RepProbB2 <- paste("ρBA", round(ρBA, digits = 4), sep = " = ")
    
    
    # OUTPUT the results to be accessed by other reactive contexts
    return(
      list(
        MPM,
        MuVector,
        InvDynA1,
        InvDynA2,
        InvDynB1,
        InvDynB2,
        RepProbA1,
        RepProbA2,
        RepProbB1,
        RepProbB2
      )
    )
    
  })
  
  output$InvDynA1 <- renderText(computeDynamics()[[3]])
  output$InvDynA2 <- renderText(computeDynamics()[[4]])
  output$InvDynB1 <- renderText(computeDynamics()[[5]])
  output$InvDynB2 <- renderText(computeDynamics()[[6]])
  output$RepProbA1 <- renderText(computeDynamics()[[7]])
  output$RepProbA2 <- renderText(computeDynamics()[[8]])
  output$RepProbB1 <- renderText(computeDynamics()[[9]])
  output$RepProbB2 <- renderText(computeDynamics()[[10]])
  
    # FIXED POINTS, NASH EQUILIBRIA and EVOLUTIONARILY STABLE STRATEGIES
    # We solve for fixed points of the replicator dynamics
    solveFixedPoints <- function(a, b, c, d) 
      if (a>c && b>=d ) { "{0, 1}"
      } else if (a>c && b<d ) { paste("{0,", round((d-b)/(d+a-b-c), digits = 4), ",1}")
      } else if (a==c && b==d ) { "[0, 1]"
      } else if (a==c && b>d ) { "{0, 1}"
      } else if (a==c && b<d ) { "{0, 1}"
      } else if (a<c && b>d ) { paste("{0,", round((b-d)/(b+c-a-d), digits = 4), ",1}") 
      } else if (a<c && b<=d ) { "{0, 1}"
      } else "ERROR"
    # And output the results as text
    output$FixedPoints <- renderText({
      solveFixedPoints(input$a, input$b, input$c, input$d)
    })
    
    # We solve for all symmetric Nash equilibria
    solveNash <- function(a, b, c, d) 
      if (a>c && b>=d ) { "{1}"
      } else if (a>c && b<d ) { paste("{0, ", round((d-b)/(d+a-b-c), digits = 4), ",1}")
      } else if (a==c && b==d ) { "[0, 1]"
      } else if (a==c && b>d ) { "{1}"
      } else if (a==c && b<d ) { "{0}"
      } else if (a<c && b>d ) { paste("{", round((b-d)/(b+c-a-d), digits = 4), "}") 
      } else if (a<c && b<=d ) { "{0}"
      } else "ERROR"
    # And output the results as text
    output$Nash <- renderText(
      { solveNash(input$a, input$b, input$c, input$d) }
    )
    
    # We solve for ESS
    solveESS <- function(a, b, c, d) 
      if (a>c && b>=d ) { "{1}"
      } else if (a>c && b<d ) { "{0, 1}"
      } else if (a==c && b==d ) { "∅"
      } else if (a==c && b>d ) { "{1}"
      } else if (a==c && b<d ) { "{0}"
      } else if (a<c && b>d ) { paste("{", round((b-d)/(b+c-a-d), digits = 4), "}") 
      } else if (a<c && b<=d ) { "{0}"
      } else "ERROR"
    # And output the results as text
    output$ESS <- renderText(
      { solveESS(input$a, input$b, input$c, input$d) }
    )
    
  # PLOT STATIONARY DISTRIBUTION  
  output$stationaryDistribution <- renderPlot({
    
    # Import relevant variables
    N <- as.numeric(input$populationSize)
    MuVector <- computeDynamics()[[2]]
    
    # Finally, plot the stationary distribution
    MuDF <- data.frame(N = c(0:N), Probability = MuVector)
    p <- ggplot(data = MuDF, aes(x = N, y = Probability)) +
      geom_bar(
        stat = "identity",
        width = 1,
        fill = "#4277BE",
        colour = "black",
        size = 0.1
      ) +
      ggtitle("Stationary Distribution") +
      labs(x = "Number of A-types in the Population i", y = bquote('Probability ' * mu[i])) +
      # ylim(c(0, 1)) +
      theme_light() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          margin = margin(t = 30, b = 20, unit = "pt"),
          lineheight = 1.15
        ),
        axis.title.x =  element_text(margin = margin(t = 15, unit = "pt")),
        axis.title.y =  element_text(margin = margin(r = 15, unit = "pt")),
        text = element_text(size = 16)
      ) +
      geom_vline(
        xintercept = which.max(MuVector) - 1,
        colour = "#4FA7EF",
        size = 1
      )
    print(p)
    
  })

  
# SIMULATING the MORAN PROCESS for a SINGLE POPULATION
  # We create the function that simulates the population from a random initial population composition
  output$singlePopulationSimulation <- renderPlot({
    
    # Activate the simulation when RUN EVOLUTION button is pressed
    simulationResetVariable <- input$simulateSinglePopulation

    # Import relevant variables
    N <- as.numeric(input$populationSize) # Population size
    t <- as.numeric(input$simulationTime) # Duration of simulation
    MPM <- computeDynamics()[[1]] # Transition matrix
    MuVector <- computeDynamics()[[2]] # Stationary distribution

    # Simulation Output: A vector of length t
    MPM.sim <- function(t, MPM) {
      # The simulation function takes as arguments: simulation duration, and the transition matrix MPM
      sim <- c()
      sim[1] <- sample(1:(N + 1), size = 1) # then take a random initial state.
      for (i in 2:(t + 1)) {
        # For each step of the simulation after the initial state,
        newstate <- sample(1:(N + 1),
                           size = 1,
                           replace = TRUE,
                           prob = MPM[sim[i - 1],]) # the transition to the next state is given by the transition probabilities of the previous state by the transition matrix MPM.
        sim[i] <- newstate
      }
      return(sim)
    }
    
    # Plot the simulation
    q <- ggplot(data = data.frame(time = 0:t, state = MPM.sim(t, MPM)), aes(x = time, y = state)) +
      geom_hline(yintercept = which.max(MuVector) - 1, colour = "#4FA7EF", size = 1) +
      geom_line(colour = "#4277BE") +
      ggtitle("Simulation of the Evolution of a Single Population") +
      labs(x = "Time t", y = "Number of A-types in the Population i") +
      ylim(c(0, N)) +
      theme_light() +
      theme(
        plot.title = element_text(
          hjust = 0.5,
          margin = margin(t = 30, b = 20, unit = "pt"),
          lineheight = 1.15
        ),
        axis.title.x =  element_text(margin = margin(t = 15, unit = "pt")),
        axis.title.y =  element_text(margin = margin(r = 15, unit = "pt")),
        legend.position = "bottom",
        legend.title = element_blank(),
        legend.text = element_text(size = 14),
        text = element_text(size = 16)
      )
    print(q)
    
    }  
  )

})

### EOD ###
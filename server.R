# FREQUENCY-DEPENDENT MORAN PROCESS
# FOR 2x2 SYMMETRIC GAMES
# << SERVER >>
# by Aydin Mohseni


# Load matrix exponential package
library(shiny)
library(expm)

# Define server logic required to draw our plot
shinyServer(function(input, output, session) {
  output$stationaryDistribution <- renderPlot({
    
        
# FINDING THE STATIONARY DISTRIBUION OF THE MORAN PROCESS   
    # Let Γ be a 2x2 symmetric game 
    # with two players,
    # pure strategies A and B,
    # and payoff matrix M = matrix(c(a,b,c, d), nrow=2, ncol=2, byrow=TRUE).

    # Then the payoff to each combination of actions would be given by
    # U(A,A)=a, U(A,B)=b, U(B,A)=c, U(B,B)=d. 

    # Four traditional game types and a fifth customizable option are set as our possible payoffs:
    # 1. Anticoordination Game (Polymorphic case)
    if (input$interaction_structure=='Anticoordination Game') {
      updateNumericInput(session, "a", value = 1)
      updateNumericInput(session, "b", value = 3)
      updateNumericInput(session, "c", value = 2)
      updateNumericInput(session, "d", value = 1)
    }
    # 2. Coordination Game (Bistable case)
    if (input$interaction_structure=='Coordination Game') {
      updateNumericInput(session, "a", value = 3)
      updateNumericInput(session, "b", value = 0)
      updateNumericInput(session, "c", value = 1)
      updateNumericInput(session, "d", value = 2)
    }
    # 3. Dominating Strategy Game (A dominates B)
    if (input$interaction_structure=='Dominating Strategy Game') { 
      updateNumericInput(session, "a", value = 3)
      updateNumericInput(session, "b", value = 1)
      updateNumericInput(session, "c", value = 2)
      updateNumericInput(session, "d", value = 0)
    }
    # 4. Degenerate Game (Neutral case)
    if (input$interaction_structure=='Degenerate Game') { 
      updateNumericInput(session, "a", value = 1)
      updateNumericInput(session, "b", value = 1)
      updateNumericInput(session, "c", value = 1)
      updateNumericInput(session, "d", value = 1)
    }
    # 5. Custom Game
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
    MPMlim <- MPM %^% 1000000
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
    xInterval <- if (N>9) { round(N/10, digits = 0) } else { 1 } # the else is to prevent the intervals from going to zero at small population sizes, and outputting an error
    axis(side=1, at=seq(0, N, by = xInterval))
    # Remove the box around the barplot
    box() 
    
    
    
    # COMPUTING THE INVASION DYNAMICS AND REPLACEMENT PROBABILITIES
    # We compute the invasion probability for each strategy.
    # We compare the fitness of each strategy, fA(i)  and fB(i), to see which strategy is favored by selection given that the population is in a state with i A-types. We define this as follows:
    h <- function(i) (fA(i)-fB(i))
    # Invasion dynamics then can be determined by evaluating the sign of h(1) and h(N-1), 
    # where h(1) is the difference in fitness between A-types and B-types where there is a single A-type mutant in the population, 
    # and h(N-1) is the difference in fitness when there is a single B-type mutant in the population.
    # If the fitness of a single mutant is less than the fitness of the resident population, then selection opposes invasion. 
    # Conversely, a strategy favored by selection for invading is likely to find a foothold in the population. 
    
    # Invasion dynamics for A:
    output$InvDynA1 <- if (h(1)>0) { renderText({"Selection favors A invading B"}) }
    else if (h(1)<0) { renderText({"Selection opposes A invading B"}) }
    else { renderText({"Invasion is neutral regarding invasion by A"}) }
    output$InvDynA2 <- renderText({ paste("h(1)", round(h(1), digits=4), sep=" = ") })
    
    # Invasion dynamics for B:
    output$InvDynB1 <- if (h(N-1)>0) { renderText({"Selection opposes B invading A"}) } 
    else if (h(N-1)<0) { renderText({"Selection favors B invading A"}) } 
    else { renderText({"Selection is neutral regarding invasion by B"}) }
    output$InvDynB2 <- renderText({ paste("h(N-1)", round(h(N-1), digits=4), sep=" = ") })

    # Next, we compute the fixation probabilities, ρAB and ρBA, 
    # corresponding to the probability that the process transitions from a single A-type mutant to an all-A population 
    # and that the process transitions from a single B-type mutant to an all-B population, respectively. 
    # (To do this we creat a vector of the ratios of the fixation probabilities fB(i)/fA(i),
    # and proceed to compute the culative sum of the cumulative products of transitions over all population states.)
    X <- c(1:N)
    HBA <- function(i) (fB(i)/fA(i))
    HBAvector <- sapply(X, HBA)
    HBAproductsVector <- cumprod(HBAvector)
    HBAsumsOfProductsVector <- cumsum(HBAproductsVector)
    ρAB <- 1/(1 + HBAsumsOfProductsVector[N-1])
    
    Y <- c(1:N)
    HAB <- function(i) (fA(i)/fB(i))
    HABvector <- sapply(Y, HAB)
    HABproductsVector <- cumprod(HABvector)
    HABsumsOfProductsVector <- cumsum(HABproductsVector)
    ρBA <- 1/(1 + HABsumsOfProductsVector[N-1])
    
    # We compare the fixation probabilities of each strategy to that of a neutral mutant
    # (whose fixation probability is 1/N). 
    # If the fixation probability of the mutant is less than that of a neutral mutant, 
    # then we say selection opposes fixation. If the fixation probability is greater than that of a neutral mutant, then we say selection favors fixation.
    
    # Replacement dynamics for A:
    output$RepProbA2 <- renderText({ paste("ρAB", round(ρAB, digits=4), sep=" = ") })  
    output$RepProbA1 <- if (ρAB > (1/N)) { renderText({"Selection favors A replacing B"}) }
    else if (ρAB < (1/N)) { renderText({"Selection opposes A replacing B"}) } 
    else { renderText({"Selection is neutral regarding replacement by A"}) }

    # "Replacement dynamics for B:
    output$RepProbB2 <- renderText({ paste("ρBA", round(ρBA, digits=4), sep=" = ") })
    output$RepProbB1 <- if (ρBA > (1/N)) { renderText({"Selection favors B replacing A"}) } 
    else if (ρBA < (1/N)) { renderText({"Selection opposes B replacing A"}) } 
    else { renderText({"Selection is neutral regarding replacement by B"}) }    
    
  })
    
  
  
# SIMULATING THE MORAN PROCESS FOR A SINGLE POPULATION
  # We create the function that simulates the population from a random initial population composition
  output$singlePopulationSimulation <- renderPlot({
    simulationResetVariable <- input$simulateSinglePopulation

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

      # Expected Payoff to Each Strategy
      FA <- function(i) (a*(i-1) + b*(N-i)) / (N-1)
      FB <- function(i) (c*(i) + d*(N-i-1)) / (N-1)
      
      # Fitness (Reproductive/Imitative Success) of Each Strategy
      fA <- function(i) (1 - w + w * FA(i))
      fB <- function(i) (1 - w + w * FB(i))
      # Population Mean Fitness
      P <- function(i) (i * fA(i) + (N-i) * fB(i))
     
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
      
      # Denote MPM^∞ := MPMlim
      MPMlim <- MPM %^% 1000000
      # Select any row from the MPM^∞ matrix to get the stationary disribution µ for the MPM
      µ <- MPMlim[round(N/2),]
      
    # Simulation Output: A vector of length t
    MPM.sim <- function(t, MPM, πNought) { # The simulation function takes as arguments: duration, the transition matrix MPM, and some initial condition πNought.
      sim <- as.numeric(t)
      if (missing(t))
        stop("Need to specify t.")
      if (missing(MPM))
        stop("Need to specify MPM.")
      if (missing(πNought)) { # If the initial state is NOT specified,
        # sim[1] <- (sample(1:(N+1),1) - 1) # then take a random initial state.
        sim[1] <- sample(1:(N+1), size=1) # then take a random initial state.
      } else { sim[1] <- πNought } # If the initial state is specified, use it.
      for (i in 2:t) { # For each step of the simulation after the initial state,
        newstate <- sample(1:(N+1), size = 1, replace = TRUE, prob = MPM[sim[i-1],]) # the transition to the next state is given by the transition probabilities of the previous state by the transition matrix MPM.
        sim[i] <- newstate
      } 
      sim 
    }
    
    # Plot the simulation
    plot(MPM.sim(t,MPM)-1, # we subtract one to correct for counting starting from zero.
         type="l",
         main="Single Population Simulation", 
         xlab="Time (t)", ylab="# of A-types in the population (i)", 
         ylim=c(0,N),
         col=rgb(82, 140, 202, max = 255))
    abline(h=which.max(µ), col=rgb(82, 140, 202, max = 255))
    }  
  )


# CALCULATING THE FIXED POINTS, NASH ANDD ESS
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
  # And reactive output
  output$FixedPoints <- renderText(
    { solveFixedPoints(input$a, input$b, input$c, input$d) }
  )
  
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
    # And reactive output
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
     # And reactive output
     output$ESS <- renderText(
       { solveESS(input$a, input$b, input$c, input$d) }
     )

})

### EOD ###
The Moran Process is the standard stochastic model of evolution [Moran, 1962; Nowak 2006]. 
The Moran process is a Markov process in which, for each time step, two individuals are chosen: 
one for reproduction and the other for elimination. 
The process is defined over the state space by the designation of transition probabilities given by a transition matrix between adjacent states. 
Here we use the Moran process to model the evolution of a population with two types.

The parameters determining the process are: 
the interaction structure of the population (given by a 2X2 symmetric game), 
population size, mutation rate, 
and intensity of selection (the contribution of the game to an organism’s net fitness). 

First, we can calculate the stationary distribution µ of the Moran process. The stationary distribution captures the process' long run behavior of the  in two senses: It is both the process’s limiting distribution and its limiting empirical distribution. 
That is, it captures both where we may expect to locate the process in the limit of time, 
and the proportion of time it will spend at each of the states in the state space.

Next, we can simulate the trajectory of a single population evolving as described by the Moran process. 
Starting from a random initial population, we sample from the transition probabilities given by the Moran process, 
and move to the resulting population state. We proceed in this way for t generations of the birth and death process. 

In the simulation, we can observe that over sufficiently long periods of time, 
the behavior of the process will accord with the predictions of the stationary distribution. 
That is, the population will spend most of its time near the peak(s) of the distribution.
#Workshop 2 (Biodiversity under Pressure) - 26/09/2024


#1. Important concepts for programming in R

my.function <- function(x=0,y=0){
  z <- x*y
  return(z)
}  #Creates a function that returns z, which is the product of x and y. x=y=0 by default, but we can include whichever values we want and the function will multiply them

my.value <- my.function(x=2,y=3) #It returns the number 6 as that is the result of multiplying 2 by 3

my.value <- my.function(x=2) #In this case it returns 0 because we did assign the value 2 to the x variable, but y stays with its default value, which is 0 (and 2*0=0)

my.function <- function(x,y){
  z <- x*y
  return(z)
}   #In this case, we don't assign a default value to x and y

my.value <- my.function(x=2,y=3) #The outcome here is the same...

my.value <- my.function(x=2) #...but in this case, instead of returning the value 0, R returns an error as we haven't assigned a default value to neither x or y. It can't execute the function as it's missing a value

#Packages in R

install.packages("vegan")
library("vegan")

#Variable types in R

#Vectors
x <- c(1,3,5)
y <- c("a", "b", "c")
z <- 1:5

#Matrix and Array
X <- matrix(0,nrow=2,ncol=3) #How do we input other data different of 0?
X <- array(0,dim=c(2,3,5)) #I don't really understand this
#In a matrix, all elements must be of the same type (numerical, character, logic...)

#Data frames
#Similar to a matrix, but each column is a separate variable and therefore different columns can be of different types. IMPORTANT
my.data <- data.frame(x=1:5,y=c("a", "b", "c", "d", "e"))

#Lists. Sequence of elements that can be completely different
my.list <- list(x=x,X=X,my.data=my.data)

#For loops and If conditions
#A for loop is a way to iterate through a suite of elements (typically numbers, but they can 
#be anything, including a series of words).
x <- 0
for(i in 1:10){
  x <- x + i
}

y <- c("a", "b", "c")
z <- character(3) ##we create a vector of characters of size 3 but with no values in it
ii <- 0
for(i in y){
  ii <- ii+1
  z[ii] <- paste(i,i)
}

x <- list("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
list <- list()
for (i in 1:10){
  list[i] = x[i]
}  #My own for loop! <3


#An if condition is a function that will lead to execute some lines of codes only if a condition 
#is met.
x <- 0
for(i in 1:10){
  x <- x + i
  if(x==10){
    print(x)
  }
}


#apply, lapply, sapply

m1 <- matrix(1:10,nrow=5, ncol=6)
m1
a.m1 <- apply(m1, 2, sum) #Why 2 in limit?
a.m1

species <- c("DOG","CAT","EAGLE","ADER")
species_lower <-lapply(species, tolower)
str(species_lower)
unlist(species_lower)

dt <- cars ## this is a dataset that is included with base R
lmn_cars <- lapply(dt, min)
smn_cars <- sapply(dt, min)
dt
lmn_cars
smn_cars


#2. Introduction to modelling with R

#Now, we'll make our first simple population models. 
#We will use a density-dependent logistic growth, using discrete and continuous models.

#Firstly, we will run some lines of code to understand the logic behind

P <- numeric(length = 500)  #first, we create a vector of size 500 using numeric()

P[1] <- 10 #here, we change the first value of the vector to be 10 (our population size at t)

r <- 0.1 #we set the growth rate of our population at 0.1...

K <- 100 #...and its carrying capacity at 100

#Now, we'll try to predict population size at t=2, at t=3 and at t=4 using the discrete logistic growth equation:

P[2] <- r*(1-(P[1]/K))*P[1]+P[1]

P[3] <- r*(1-(P[2]/K))*P[2]+P[2]

P[4] <- r*(1-(P[3]/K))*P[3]+P[3]

#Instead of doing this until we fill our vector, we use for loops.

for (t in 2:500) {
  P[t] <- r*(1-(P[t-1]/K))*P[t-1]+P[t-1]
}
  
plot(P, xlab = "Time (t)", ylab = "Population size (P)", main = "Population size at time t")

plot(P, xlab = "Time (t)", ylab = "Population size (P)", log = "y", main = "Log of population size at time t")

plot(P, xlab = "Time (t)", ylab = "Population size (P)", main = "Population size at time t", type = "l")

plot(P, xlab = "Time (t)", ylab = "Population size (P)", log = "y", main = "Log of population size at time t", type = "l")

plot(P, xlab = "Time (t)", ylab = "Population size (P)", main = "Population size at time t")
lines(P, type = "l", col = "red")
legend <- c("Discrete points", "Continuous line")
legend("bottomright", legend, col = c("black", "red"), lty =c(6,1)) #this legend is not right, but I wanted to practice code

#Let's try plotting P(t+1) vs P(t):

plot(P[1:499], P[2:500], xlab = "P(t)", ylab = "P(t+1)", main = "P(t+1) vs. P(t)") #here we can see how data points are more are more frequent as they close in to the carrying capacity (K=100)

plot(P[1:499], P[2:500], xlab = "P(t)", ylab = "P(t+1)", main = "P(t+1) vs. P(t)", type = "l") #here, however, since we plot it as a continuous line, we don't really appreciate that.

#Now, let's try to plot a curve showing the whole possible set of [P(t),P(t+1)] combinations. 
#We cannot exactly show the whole possible set of [P(t),P(t+1)] combinations, but we can compute a large number of these combinations and plot the output.

Pt <- seq(0,150,0.1) #that creates a vector from 0 to 150 with an increment of 0.1
Ptt <- r*(1-Pt/K)*Pt+Pt #that applies the discrete logistic growth equation to all positions of the vector Pt and stores the values in the object "Ptt"
lines(Pt, Ptt, col = "blue", xlim = c(1,150))
lines(c(1,150), c(1,150), col = "red") # that draws the diagonal, as we saw in the lecture, to show the equilibrium

#Now, let's change the values of P(0):

#P(0)=20, r=0.5, K=100 
P[1] <- 20
r <- 0.5
K <- 100
for (t in 2:500) {
  P[t] <- r*(1-(P[t-1]/K))*P[t-1]+P[t-1]
}
plot(P[1:499], P[2:500], xlab = "P(t)", ylab = "P(t+1)", main = "P(t+1) vs. P(t) when P(0)=20, r=0.5 and K=100") 
lines(Pt, Ptt, col = "blue", xlim = c(1,150))
lines(c(1,150), c(1,150), col = "red") # that draws the diagonal, as we saw in the lecture, to show the equilibrium


#P(0)=10, r=1, K=100 
P[1] <- 10
r <- 1
K <- 100
for (t in 2:500) {
  P[t] <- r*(1-(P[t-1]/K))*P[t-1]+P[t-1]
}
plot(P[1:499], P[2:500], xlab = "P(t)", ylab = "P(t+1)", xlim = c(0,100), type = "l", ylim = c(0,100), main = "P(t+1) vs. P(t) when P(0)=10, r=1 and K=100") 
lines(Pt, Ptt, col = "blue", xlim = c(1,150))
lines(c(1,150), c(1,150), col = "red") # that draws the diagonal, as we saw in the lecture, to show the equilibrium


#P(0)=10, r=1.3, K=100 
P[1] <- 10
r <- 1.3
K <- 100
for (t in 2:500) {
  P[t] <- r*(1-(P[t-1]/K))*P[t-1]+P[t-1]
}
plot(P, xlab = "Time (t)", ylab = "Population size (P)", type = "l", main = "Population size at time t when P(0)=10, r=1.3 and K = 100")
plot(P[1:499], P[2:500], xlab = "P(t)", ylab = "P(t+1)", xlim = c(0,100), type = "l", ylim = c(0,100), main = "P(t+1) vs. P(t) when P(0)=10, r=1.3 and K=100") 
lines(Pt, Ptt, col = "blue", xlim = c(1,150))
lines(c(1,150), c(1,150), col = "red") # that draws the diagonal, as we saw in the lecture, to show the equilibrium


#P(0)=10, r=1.7, K=100 
P[1] <- 10
r <- 1.7
K <- 100
for (t in 2:500) {
  P[t] <- r*(1-(P[t-1]/K))*P[t-1]+P[t-1]
}
plot(P, xlab = "Time (t)", ylab = "Population size (P)", type = "l", xlim = c(0,300), main = "Population size at time t when P(0)=10, r=1.7 and K = 100")
plot(P[1:499], P[2:500], xlab = "P(t)", ylab = "P(t+1)", xlim = c(0,100), type = "l", ylim = c(0,100), main = "P(t+1) vs. P(t) when P(0)=10, r=1.7 and K=100") 
lines(Pt, Ptt, col = "blue", xlim = c(1,150))
lines(c(1,150), c(1,150), col = "red") # that draws the diagonal, as we saw in the lecture, to show the equilibrium


#P(0)=10, r=2, K=100 
P[1] <- 10
r <- 2
K <- 100
for (t in 2:500) {
  P[t] <- r*(1-(P[t-1]/K))*P[t-1]+P[t-1]
}
plot(P, xlab = "Time (t)", ylab = "Population size (P)", type = "l", xlim = c(0,200), main = "Population size at time t when P(0)=10, r=2 and K = 100") #between r=1.7 and r=2, population size starts to oscilate in cicles. The population overshoots
plot(P[1:499], P[2:500], xlab = "P(t)", ylab = "P(t+1)", type = "l", main = "P(t+1) vs. P(t) when P(0)=10, r=2 and K=100")  
lines(Pt, Ptt, col = "blue", xlim = c(1,150))
lines(c(1,150), c(1,150), col = "red") # that draws the diagonal, as we saw in the lecture, to show the equilibrium


#P(0)=10, r=2.5, K=100 
P[1] <- 10
r <- 2.5
K <- 100
for (t in 2:500) {
  P[t] <- r*(1-(P[t-1]/K))*P[t-1]+P[t-1]
}
plot(P, xlab = "Time (t)", ylab = "Population size (P)", type = "l", xlim = c(0,200), main = "Population size at time t when P(0)=10, r=2.5 and K = 100") #when r=2.5, we can see double cycles, but still no chaotic behaviour
plot(P[1:499], P[2:500], xlab = "P(t)", ylab = "P(t+1)", type = "l", main = "P(t+1) vs. P(t) when P(0)=10, r=2.5 and K=100")  
lines(Pt, Ptt, col = "blue", xlim = c(1,150))
lines(c(1,150), c(1,150), col = "red") # population goes below equilibrium


#P(0)=10, r=3, K=100 
P[1] <- 10
r <- 3
K <- 100
for (t in 2:500) {
  P[t] <- r*(1-(P[t-1]/K))*P[t-1]+P[t-1]
}
plot(P, xlab = "Time (t)", ylab = "Population size (P)", type = "l", xlim = c(0,200), main = "Population size at time t when P(0)=10, r=3 and K = 100") #now we start seeing chaotic behaviour.
plot(P[1:499], P[2:500], xlab = "P(t)", ylab = "P(t+1)", type = "l", main = "P(t+1) vs. P(t) when P(0)=10, r=3 and K=100")  
lines(Pt, Ptt, col = "blue", xlim = c(1,150))
lines(c(1,150), c(1,150), col = "red") # that draws the diagonal, as we saw in the lecture, to show the equilibrium

#A different way to plot P(t+1) vs. P(t):
plot(c(P[1],rep(P[2:99],each=2)), y = c(rep(P[2:99],each=2),P[100]), main = "P(t+1) vs. P(t) (P(t)=10, r=3, K=200", type = "l")
lines(Pt, Ptt, col = "blue")
lines(c(1,300), c(1,300), col = "red") 

#what happens if our population size starts over K?
P[1] <- 130
r <- 3
K <- 100
for (t in 2:500) {
  P[t] <- r*(1-(P[t-1]/K))*P[t-1]+P[t-1]
}
plot(P, xlab = "Time (t)", ylab = "Population size (P)", type = "l", xlim = c(0,200), main = "Population size at time t when P(0)=130, r=3 and K = 100") #we still see chaotic behaviour.
plot(P[1:499], P[2:500], xlab = "P(t)", ylab = "P(t+1)", type = "l", main = "P(t+1) vs. P(t) when P(0)=130, r=3 and K=100")  
lines(Pt, Ptt, col = "blue", xlim = c(1,150))
lines(c(1,150), c(1,150), col = "red") # that draws the diagonal, as we saw in the lecture, to show the equilibrium


#P(0)=130, r=1.5, K=100
P[1] <- 130
r <- 1.5
K <- 100
for (t in 2:500) {
  P[t] <- r*(1-(P[t-1]/K))*P[t-1]+P[t-1]
}
plot(P, xlab = "Time (t)", ylab = "Population size (P)", type = "l", xlim = c(0,200), main = "Population size at time t when P(0)=130, r=1.5 and K = 100") 
plot(P[1:499], P[2:500], xlab = "P(t)", ylab = "P(t+1)", type = "l", main = "P(t+1) vs. P(t) when P(0)=130, r=1.5 and K=100")  
lines(Pt, Ptt, col = "blue", xlim = c(1,150))
lines(c(1,150), c(1,150), col = "red") 


#Let's move on to the Continuous Logistic Growth Model now.

#In order to work with the continuous model, we're gonna need to download several packages:

install.packages("deSolve") #deSolve is a package that contains the function ode() for solving the differential equation
install.packages("tidyverse") #tidyverse contains some useful functions to manipulate data.
install.packages("ggplot2") #ggplot2 makes beautiful graphs

library(deSolve)
library(tidyverse)
library(ggplot2)

#Conflict problems
install.packages("devtools")
devtools::install_github("r-lib/conflicted")

library(conflicted)
library(dplyr)

#Moving on

#To use the ode() function in deSolve we need to define four parameters: y, times, func and parms.
##y: the initial (state) values for the ODE system, a vector. If y has a name attribute, the names will be used to label the output matrix.
##The initial (state) value for our specific ODE system is P(0), therefore we define:
  state <- c(P=10)
##times: time sequence for which output is wanted; the first value of times must be the initial time.
##As we did for the Discrete Logistic Growth model, let’s simulate 500 time steps, and get values for every 0.1 interval:
  times <- seq(0,100,by=0.01) #but the vector is longer than 500...?
##parms: parameters passed to func
##In our case, these parameters are r and K:
  parameters <- c(r=0.1, K=100)
##func: either an R-function that computes the values of the derivatives in the ODE system (the model definition) at time t, or a character string giving the name of a compiled function in a dynamically loaded shared library.
##func is a bit more complicated to define, but basically it is just an R function that implements the differential equation above:
##LG will be our func:
  LG <- function(t,state,parameters) { ##logistic grown function, that takes a set of parameter values, initial conditions and a time sequence
    with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
      dP <- r*(1-P/K)*P ##this is our logistic equation governing the rate of change of P
      return(list(dP)) ## return the rate of change - it needs to be a list
    }) # end with(as.list ...
  }

#Now, we just need to input all parameters into ode():
out <- ode(y = state, times = times, func = LG, parms = parameters) 

#out is a deSolve object, which R can have trouble interpreting. We'll change it into a data frame:

out.df <- as.data.frame(out)

plot(out.df, type = "l")
abline(h=100, col = "red")

#We can also use ggplot2 to get a better result:
ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=P),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  geom_abline(intercept = 100, slope = 0, col = "red") +
  labs(x = "Time", y = "P")

#Let's change r to r=3, which is when we started seeing chaotic behaviour for the Discrete Logistic Growth Model:
parameters <- c(r=3, K=100)
LG <- function(t,state,parameters) { ##logistic grown function, that takes a set of parameter values, initial conditions and a time sequence
  with(as.list(c(state, parameters)),{ ##"with" is a function that allows us to use the variable names directly - it looks for r, K and P in state and parameters
    dP <- r*(1-P/K)*P ##this is our logistic equation governing the rate of change of P
    return(list(dP)) ## return the rate of change - it needs to be a list
  }) # end with(as.list ...
}
out <- ode(y = state, times = times, func = LG, parms = parameters) 
out.df <- as.data.frame(out)
ggplot(data = out.df)+
  geom_line(mapping=aes(x=time,y=P),color="blue") +
  geom_hline(yintercept=0,color="darkgrey") +
  geom_vline(xintercept=0,color="darkgrey") +
  geom_abline(intercept = 100, slope = 0, col = "red") +
  labs(x = "Time", y = "P")
#what we see is that the population reaches the equilibrium much faster, but without showing the chaotic behaviour that we saw in the discrete model


#Model calibration

#We could use our model to figure out the K and r parameters out of real data from a population. This is called model calibration.
#Let's try and do this, but using generated data for which we actually know the values of r and K.
#More specifically, this is how the data was generated:
tmax <- 300
x <- numeric(tmax+1)
for(i in 2:(tmax+1)){
  x[i] <- r*x[i-1]*(1-x[i-1]/K)+x[i-1]+rnorm(1,0,x[i-1]/100)
}
#We're adding random values from a Normal distribution?

#Let's load our data
data1 <- read.csv("data/pop_LG_simul_noise_small.csv")
View(data1)

#Let's use this data to define the parameters of our differential equation
state <- data1[1,2] #This will be our initial state

plot(data1, type = "l")
K <- mean(data1[250,2]:data1[301,2]) 

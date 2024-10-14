#Workshop 2 (Biodiversity under Pressure) - 26/09/2024

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


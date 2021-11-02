# Essential data
{
# Number of distinct values, integer at least 1
n <- 13

# Number of suits, integer at least 1
m <- 4

# Number of cards held by each player, integer at least 1
k <- 4

# Number of players, integer at least 1
t <- 4
}

# Starter functions
{
# Starting deck of cards, m suits, n distinct values
mainor <- rep(m,n)

# Random sampler of k values from int vector x
samplor <- function(x,k){
  samp <- sample(expandor(x),k,replace=FALSE)
  return(contractor(samp,length(x)))
}

# x is the integer vector you'd like to expand
expandor <- function(x){
  return(rep(1:length(x),x))
}

# x is the integer vector you'd like to contract into n categories
contractor <- function(x,n=max(x)){
  y <- numeric(n)
  for(i in 1:length(x)){
    y[x[i]] <- y[x[i]]+1
  }
  return(y)
}
}

# Strategies
{
# Single unit in smallest category of x, the strategy function
posminor_m <- function(x){
  if(length(x[x!=0])==0){
    return(x)
  }
  t <- which(x %in% min(x[x!=0]))[1]
  y <- numeric(length(x))
  y[t] <- 1
  return(y)
}

# Randomly pick single unit out of possible smallest
# categories, second strategy function
posminor_r <- function(x){
  if(length(x[x!=0])==0){
    return(x)
  }
  v <- which(x %in% min(x[x!=0]))
  if(length(v)==1){
    t <- v
  }else{
    t <- sample(which(x %in% min(x[x!=0])),1)
  }
  y <- numeric(length(x))
  y[t] <- 1
  return(y)
}

# For discarding hopeless doubles
dblcheqor <- function(x){
  v<-x[x!=0]
  if((min(v)==k/2)&(max(v)==k/2)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}
}

# Simulation code
{
# The player matrix
gamestator <- function(){
  return(matrix(c(mainor,numeric(n*t)),t+1,n,byrow = TRUE))
}

# The dealer function
dealor <- function(x,k){
  for(i in 2:nrow(x)){
    x[i,]<-x[i,]+samplor(x[1,],k)
    x[1,]<-x[1,]-x[i,]
  }
  return(x)
}

# One cycle of play
cyclor_m <- function(x){
  r <- nrow(x)
  p <- samplor(x[1,],1)
  x[1,] <- x[1,] - p ; x[2,] <- x[2,] + p
  for(i in 2:r){
    p <- posminor_m(x[i,])
    x[i,] <- x[i,]-p
    f<-dblcheqor(x[i,])
    if(f==FALSE){
      x[(i%%r)+1,] <- x[(i%%r)+1,]+p
    }
    if(f==TRUE){
      f<- posminor_m(x[i,])
      x[i,] <- x[i,]-f+p ; x[(i%%r)+1,] <- x[(i%%r)+1,]+f
    }
  }
  return(x)
}

# Other strat
cyclor_r <- function(x){
  r <- nrow(x)
  p <- samplor(x[1,],1)
  x[1,] <- x[1,] - p ; x[2,] <- x[2,] + p
  for(i in 2:r){
    p <- posminor_r(x[i,])
    x[i,] <- x[i,]-p
    f<-dblcheqor(x[i,])
    if(f==FALSE){
      x[(i%%r)+1,] <- x[(i%%r)+1,]+p
    }
    if(f==TRUE){
      f<- posminor_r(x[i,])
      x[i,] <- x[i,]-f+p ; x[(i%%r)+1,] <- x[(i%%r)+1,]+f
    }
  }
  return(x)
}

checkor <- function(x){
  if(k %in% x[-1,]){
    return(FALSE)
  }
  return(TRUE)
}

detector <- function(x){
  for(i in 2:(t+1)){
    if(k %in% x[i,]){
      return(i-1)
    }
  }
}
}



# Run simulation
simulator <- function(N){
  total=numeric(N)
  for(i in 1:N){
    g<-gamestator()
    g<-dealor(g,k)
    while(checkor(g)){
      g<-cyclor_r(g)
    }
    total[i]<-detector(g)
  }
  return(total)
}


# Create a graph
library('tidyverse')

d<-tibble(player=as.factor(c(1,2,3,4)),wins=c(24459,24079,24969,26493))
ggplot(d)+
  geom_bar(mapping=aes(x=player,y=wins), stat='identity')+
  geom_hline(aes(yintercept = 25000))

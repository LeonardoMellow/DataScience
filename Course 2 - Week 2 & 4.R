# tem uma list, pode transformar pra vector com unclass, com as.numeric, com as.character, etc.

# Course 2 - Week 2 - Lectures

# # # # # # # # # # # # # # # # # # 

y <- 10
f <- function(x){
  y <- 2
  y^2 + g(x)
}
g <- function(x){
  x*y
}
f(3)

# # # # # # # # # # # # # # # # # # 

p <- as.POSIXlt(Sys.time())
names(unclass(p))
p$yday

# # # # # # # # # # # # # # # # # # 

# Course 2 - Week 2 - Assignment
setwd("~/Downloads")
pollutantmean <- function(directory, pollutant, id = 1:332) {
  x <- dir(path = directory)
  df <- data.frame()
  
  for (i in id) {
    foo <- read.csv(paste0(directory, "/", x[i]))
    df <- rbind(df, foo)
  }
  
  mean(df[[pollutant]], na.rm = TRUE)
}
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)

complete <- function(directory, id = 1:332) {
  x <- dir(path = directory)
  df <- data.frame()
  
  for (i in id) {
    foo <- read.csv(paste0(directory, "/", x[i]))
    cc <- complete.cases(foo)
    df <- rbind(df, data.frame(id = i, nrow = sum(cc)))
  }
  df
}

corr <- function(directory, threshold = 0) {
  x <- dir(path = directory)
  df <- data.frame()
  vec <- numeric()
  
  for (file in x) {
    foo <- read.csv(paste0(directory, "/", file))
    cc <- complete.cases(foo)
    
    if(sum(cc) > threshold) {
      bar <- foo[cc,]
      vec <- c(vec, cor(bar$sulfate, bar$nitrate))
    }
  }
  vec
}

# Profiler: useful to understand why something is taking too long to run and fix it

# 1st Question: is your program running slowly?
# In general, you shouldn't think of optimizing your code at first.
  # First thing: How to make the code run and how to make it readable

## If you know where the problem is:
  system.time()
  # Ex.: system.time({loooooong expression})
  # Returns an object of class proc_time. If there's an error: time until error
  # user time: time charged to CPU for this expression
  # elapsed time: "wall clock" time

  # elapsed time > user time => CPU spends time waiting for things that may not
    # be related to the code
  # user time > elapsed time => if machine is using multiple cores (parallel or
    # using libraries that use multicore, like BLAS - linear algebra operations)


## If you don't know where the problem is: 
  ####  (do not use both alternatives together) ####
  Rprof() # R profiler
    # keeps track of function call stacks every 0.02 secs and prints it out
  summaryRprof() # summarizes the output of R profiler and makes it readable
    # $by.self , $sampling.time
  # If your function takes less than 0.02 secs to run, R profiler is useless


# how much space the dataset is occupying in memory:
  object.size(plants)

# Generate 5 random values from a Poisson distribution with mean 10 and perform 
# this operation 100 times  (creates a matrix)
  replicate(100, rpois(5,10))



# Assignment Week 4
setwd("~/Downloads/rprog%2Fdata%2FProgAssignment3-data/")  
library(dplyr); library(stringr)
foo <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
bar <- fread("hospital-data.csv")

foo[, 11] <- as.numeric(foo[, 11])
hist(foo[, 11])

best <- function(state, outcome) {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Change name of outcome to match columns name pattern
  outcome <- paste0("Hospital.30.Day.Death..Mortality..Rates.from.",
                    gsub(" ", ".", str_to_title(outcome)))
  
  ## Check if state and outcome are valid and print message if don't
  if ( !(state %in% data$State) ) {
    stop("invalid state")
  }
  if ( !(outcome %in% names(data)) ) {
    stop("invalid outcome")
  }
  
  ## Coerce the column to numeric
  suppressWarnings(data[,outcome] <- as.numeric(data[,outcome]))
  
  ## Return hospital name in that state with the lowest 30-day death rate
  data <- data[data$State == state,]
  # library(data.table)
  # data <- as.data.table(data)
  # data <- data[State == state,]
  data[which(data[outcome] == min(data[outcome], na.rm = T)), 2]
  # data[get(outcome) == min(get(outcome), na.rm = T), Hospital.Name]
}

best("TX", "heart attack")
best("TX", "heart failure")
best("MD", "heart attack")
best("MD", "pneumonia")
best("BB", "heart attack")
best("NY", "hert attack")

rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Change name of outcome to match columns name pattern
  outcome <- paste0("Hospital.30.Day.Death..Mortality..Rates.from.",
                    gsub(" ", ".", str_to_title(outcome)))
  
  ## Check if state and outcome are valid and print message if don't
  if ( !(state %in% data$State) ) {
    stop("invalid state")
  }
  if ( !(outcome %in% names(data)) ) {
    stop("invalid outcome")
  }
  
  ## Coerce the column to numeric
  suppressWarnings(data[,outcome] <- as.numeric(data[,outcome]))
  
  ## Return hospital name in that state with the lowest 30-day death rate
  library(data.table)
  data <- as.data.table(data[data$State == state,])
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  data <- data[,.(Hospital.Name, Rate = get(outcome))][order(data[[outcome]], 
                                                             Hospital.Name)]
  data <- data[!is.na(Rate)]
  
  if (num == "best") {
    num <- 1
  }
  
  if (num == "worst") {
    num <- nrow(data)
  }
  
  data[num, Hospital.Name]
}

rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
rankhospital("MN", "heart attack", 5000)


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


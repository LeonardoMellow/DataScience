# tem uma list, pode transformar pra vector com unclass, 
# com as.numeric, com as.character, etc.

# Course 3 - Week 1 - Quiz

# # # # # # # # # # # # # # # # # # 
library(data.table); library(dplyr)
library(xlsx) 
# install.packages("xlsx")
#   No terminal: sudo R CMD javareconf
#                sudo ln -f -s $(/usr/libexec/java_home)/jre/lib/server/libjvm.dylib /usr/local/lib
library(XML)

foo <- read.csv("Downloads/getdata%2Fdata%2Fss06hid.csv")
foo$VAL %>% table

dat <- read.xlsx("Downloads/getdata%2Fdata%2FDATA.gov_NGAP.xlsx")
sum(dat$Zip*dat$Ext,na.rm=T)

xmL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
#### Removendo o "s" do "https"
xmL <- "http://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
bar <- xmlTreeParse(xmL, useInternalNodes = TRUE, )
node <- xmlRoot(bar)
sum(xpathSApply(node, "//zipcode", function(x) xmlValue(x) == "21231"))

DT <- fread("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv")
mean(DT$pwgtp15,by=DT$SEX) # wrong result
DT[,mean(pwgtp15),by=SEX] # wrong result  maybe yes, that's the one (LOL)
sapply(split(DT$pwgtp15,DT$SEX),mean) # could be NO
rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2] # error NO
mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15) # could be NO
tapply(DT$pwgtp15,DT$SEX,mean) # could be NO 


race <- 100
Frodo <- replicate(race, system.time(sapply(split(DT$pwgtp15,DT$SEX),mean))[1])
Pippin <- replicate(race, system.time(tapply(DT$pwgtp15,DT$SEX,mean))[1])
Merry <- replicate(race, system.time(mean(DT[DT$SEX==1,]$pwgtp15)) + 
                     system.time(mean(DT[DT$SEX==2,]$pwgtp15))[1])

Frodo_av = cumsum(Frodo) / seq_along(Frodo)
Pippin_av = cumsum(Pippin) / seq_along(Pippin)
Merry_av = cumsum(Merry) / seq_along(Merry)

topY = max(Frodo_av, Pippin_av, Merry_av) 
#making sure the y axis is the right height

lowY = min(Frodo_av, Pippin_av, Merry_av)
#making sure the y axis is the right height

plot(Frodo_av, type = "l", col = "green", 
     ylim = c(lowY, topY), xlab = "distance", ylab = "average time")
lines(Pippin_av, col = "red")
lines(Merry_av, col = "blue")


# Week 2 - LECTURES

con <- dbConnect(MySQL(), user = "genome", 
                 host = "genome-mysql.cse.ucsc.edu")
result <- dbGetQuery(con, "show databases;") # show databases; is an SQL command
dbDisconnect(con)

con <- dbConnect(MySQL(), user = "genome", db = "hg19",
                 host = "genome-mysql.cse.ucsc.edu")
allTables <- dbListTables(con)
length(allTables) # How many tables/dataframes are in this Database?

# What are the fields/columns/variables in "affyU133Plus2" table/dataframe?
dbListFields(con,"affyU133Plus2")

# number of records/rows in hg19 database
dbGetQuery(con, "select count(*) from affyU133Plus2")

# Convert "affyU133Plus2" to dataframe 
affyData <- dbReadTable(con, "affyU133Plus2")
head(affyData)

# Selecting a subset of the data
query <- dbSendQuery(con, "select * from affyU133Plus2 where misMatches between 1 and 3")
affyMis <- fetch(query)
quantile(affyMis$misMatches)

affyMisSmall <- fetch(query, n = 10) # restricting to top 10 records/rows

dbClearResult(query) # clearing the query/result

dim(affyMisSmall)
dbDisconnect(con)

## list of sql commands: http://www.pantz.org/software/mysql/mysqlcommands.html
## other mysql commands: http://www.r-bloggers.com/mysql-and-r/



# Course 3 - WEEK 2 - QUIZ #

## Question 1

### To Register an application with the Github API, go to:
### https://github.com/settings/developers
### Use any URL for the homepage URL (http://github.com is fine) 
### and http://localhost:1410 as the callback url

### Take notes of Client ID (c4c43e4a31b47d2a444f) and
### Client Secret (cdf9dbb6c6ab0d0c1e7b4e0d7a4eabcc9407e257)

### Follow tutorial:https://github.com/r-lib/httr/blob/master/demo/oauth2-github.r
library(httr)
oauth_endpoints("github")

myapp <- oauth_app("github",
                   key = "c4c43e4a31b47d2a444f",
                   secret = "cdf9dbb6c6ab0d0c1e7b4e0d7a4eabcc9407e257")

# 3. Get OAuth credentials
github_token <- oauth2.0_token(oauth_endpoints("github"), myapp)

url <- "https://api.github.com/users/jtleek/repos"
con <- GET(url); con
json1 <- content(con); json1
library(jsonlite)
json2 <- fromJSON(toJSON(json1)); json2

library(data.table)
json2 <- json2 %>% as.data.table()
json2[, .(name, created_at)]

## Question 2
install.packages("sqldf"); library(sqldf)
setwd("/Users/LeonardoMelo/Downloads/")
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv", 
              destfile = "/Users/LeonardoMelo/Downloads/foo.csv")

acs <- fread("foo.csv")
sqldf("select * from acs where AGEP < 50 and pwgtp1") %>% head
sqldf("select pwgtp1 from acs where AGEP < 50") %>% head
acs[AGEP < 50, pwgtp1] %>% head

## Question 3
sqldf("select distinct AGEP from acs") %>% head
unique(acs$AGEP) %>% head 

## Question 4
# library(XML)
# library(httr)
url <- "http://biostat.jhsph.edu/~jleek/contact.html"
# html <- GET(url)
# content <- content(html, as = "text")
# parsed <- XML::htmlParse(content, asText = T)
a <- readLines(url)
b <- c(a[10],a[20],a[30],a[100])
unlist(lapply(b, nchar))

## Question 5
foo <- read.fwf("foo.for", skip=4, widths=c(12, 7, 4, 9, 4, 9, 4, 9, 4)) %>%
  as.data.table
foo[, sum(V4)] #32426.7



# Week 2 - LECTURES

## which                         ## order
## dplyr::arrange(df,var)        ## dplyr::arrange(df,desc(var))

## summary with 
  ## summary(variable)           ## summary( DATA ) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## and str()

## quantile(variable, na.rm = T)
## quantile(variable, na.rm = T, probs = c(0.5, 0.7, 0.9))

## table(variable, useNA = "ifany") # adds number of missing values !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## table(variable1, variable2) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

## table(variable %in% c(whatever))
## DATA[ variable %in% c(whatever), ]

## check for missing values with
  ## sum(is.na(variable)) # returns number of missing values
  ## any(is.na(variable)) # returns TRUE/FALSE
  ## !!!! colSums(is.na( DATA )) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ## all(colSums(is.na( DATA )))
## all(variable > number)

## checking size
  ## print(object.size( DATA ), units = "Mb")





## check directory existence
#if(!file.exists("./DirectoryName")) { dir.create("./data") }

## sometimes when downloading a file you need to use download.file(..., method = "curl")

## AGGREGATING IN GROUPS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#cut(variable, breaks = list)
## EASIER CUTTING !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#library(Hmisc)
#cut2(variable, g=4) #g stands for group
## cutting produces factors

## factor(..., levels = c(factor1, factor2, etc.))

## dplyr::mutate

## meh –.–'
# library(reshape2); mtcars$carname <- rownames(mtcars)
# carMelt <- melt(mtcars, id = c("carname", "gear", "cyl"),
#                 measure.vars = c("mpg", "hp"))
# cylData <- dcast(carMelt, cyl ~ variable); cylData
# cylData <- dcast(carMelt, cyl ~ variable, mean); cylData
# tapply(InsectSprays$count, InsectSprays$spray, sum)
#   or 
# spIns <- split(InsectSprays$count, InsectSprays$spray)
# unlist(lapply(spIns, sum))
#   or 
# sapply(spIns, sum)


## !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
## plyr::ddply(InsectSprays, .(spray), summarise, sum = ave(count, FUN = sum))
## data.table way:
##   ins <- InsectSprays %>% as.data.table
##   ins[, .(sum=ave(count,FUN=sum)), by = .(spray)]
## repeates the new column for every instance
##   spray  sum
##     A    174
##     A    174
##     A    174
##     A    174
##     B    121
##     B    121
##     B    121
##       ...



## DF1 <- DF1 %>% select(col2:col5)
## DT[, col2:col5]

## DF1 <- DF1 %>% select(-(col2:col5))
## DT[, -(col2:col5)]

## DF <- rename(DF, tidy.name = weird.name)
## DF <- DF %>% rename(tidy.name = weird.name)

## mutate...
system.time(replicate(100,mutate(InsectSprays, var = factor(1 * (count > 20), labels = c("big","small")))))
system.time(replicate(100,mutate(InsectSprays, var = as.factor(ifelse(count > 20, "small", "big")))))
## mutate(df, var = as.factor( ifelse (cond, "ble", "bla"))) is usually faster
## than mutate(df, var = factor(1 * (cond), labels = c("bla", "ble")))


# Course 3 # Week 3 # Quiz

## Question 1
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
path <- "/Users/LeonardoMelo/Downloads/foo.csv"
download.file(url, destfile = path)
foo <- fread(path)
agricultureLogical <- foo$ACR == 3 & foo$AGS == 6
which(agricultureLogical) %>% head(3)

## Question 2
library(jpeg)
# url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
bar <- readJPEG("/Users/LeonardoMelo/Downloads/jpeg.jpg", native = TRUE)
quantile(bar, probs = c(0.3, 0.8))

## Question 3
url1 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
path1 <- "/Users/LeonardoMelo/Downloads/gdp.csv"
download.file(url1, destfile = path1)
gdp <- fread(path1, sep = ",")
gdp2 <- gdp[6:220,.(V1, V2, V4, V5)]
gdp2 <- gdp2[ !(V1 == ""), ]

url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
path2 <- "/Users/LeonardoMelo/Downloads/data.csv"
download.file(url2, destfile = path2)
data <- fread(path2)

# sum(!(is.na(match(gdp2$V1, data$CountryCode))))

#189
arrange(gdp2, -as.integer(V2))[13,3]


## Question 4
colnames(gdp2)[1] <- "CountryCode"
mg <- left_join(gdp2, data, by = "CountryCode") %>% as.data.table
mg[, mean(as.integer(V2), na.rm = T), by = .(`Income Group`)]

## Question 5
# 5


## Course 3 - Week 4
library(stringr)
str_trim
# All variable to lower case
# Descriptive (Diagnosis instead of Dx)
# Avoid _ . or whitespace in variable names
# Factor them (in general)
# True / False instead of 0 / 1
# Male / Female instead of 0 / 1 or M / F

## Regular Expressions can be thought of as a combination of literals and 
## metacharacters. Analogy: Literal text form the words and metacharacters
## define their grammar

## With metacharacters you can search for Clinton clinton clinto; war peace

# start of a line: ^       
#    ^i think   => match the lines:     i think we all...
#                                       i think i have...
#                                       i think this will...

# end of a line: $
#    morning$   => match the lines:     well they have something this morning
#                                       etc.

# all different versions of a word, say, Bush:
#    [Bb][Uu][Ss][Hh]    => will match {Bush, bush, bushwalking, etc}.
#    ^[Ii] am            => will match {I am, i am} at the beginning of line.
# 

# range of letters and numbers
#    ^[0-9][a-zA-Z]      => will match {1st, 2Nd, 3rD, 4AB, 5cd, etc} at the 
#                           beginning of the line.

# ^ inside [] means not
#     [^?.]$             => will match anything (because of the [ ] thing ) that
#                           ends (due to the $ thing) with something different
#                           (because of the ^ thing) than ? or .

# . means any
#      9.11              => will match anything like 9/11, 9-11, 9.11,
#                           1239.11209, 19:11:04, 012309991101923

# | means or 
#      flood|fire        => will match anything with flood or fire, like firewire...

# ^[Gg]ood|[Bb]ad        => will match Good, good, Goodbye, etc. AT THE BEGINNING
#                           OF THE LINE (^) OR (|) bad, Bad, etc. EVERYWHERE
#                           *Note that [Bb]ad does not start with ^

# ^([Gg]ood|[Bb]ad)      => the same as before, but bad, Bad, etc. at the beginning

# ? indicates the expression before is optional
# [Gg]eorge( [Ww]\.)? [Bb]ush

# \. means period, not a metacharacter

# (.*)                   => will match ANYTHING (OR EVEN NOTHING) in parenthesis

# [0-9]+ (.*)[0-9]+      => will match AT LEAST one number followed by any number
#                           of characters followed by AT LEAST another number
#                           720 MP aisdja ajds 42nd 

# { } intervals specifiers
# [Bb]ush( +[^ ]+ +){1,5} debate
#                        => will match
#                           bush, bushes, Bush, etc.   followed by
#                           at least a space   followed by
#                           at least a character that it's not a space   followed by
#                           at least a space   followed by
#                           all three above repeated from 1 to 5 times maximum, i.e., between 1 and 5 words
#                           followed by debate, debates, etc.

##    ... etc ...





## Dates
# format(date_class_variable, "string for format")
#     %d means day as number (0-31)
#     %a means abbreviated weekday
#     %A means unabbreviated weekday
#     %m means month (00-12)
#     %b means abbreviated month
#     %B means unabbreviated month
#     %y means 2-digit year
#     %Y means 4-digit year

x <- as.Date("1jan1970", "%d%b%Y")
y <- as.Date("1jan71", "%d%b%y")
y - x
as.numeric(y - x)

library(lubridate)
lubridate::ymd("20140212") #year first, than month, than day
lubridate::ymd("20140212") #year first, than day, than month
# etc.
lubridate::wday(x)
lubridate::wday(x, label = T)
lubridate::wday(x, label = T, abbr = F)

z <- x + 171; z
yday(z)



## QUIZ WEEK 4
library(data.table)
library(dplyr)

# Question 1
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
path <- "/Users/LeonardoMelo/Downloads/foo.csv"
download.file(url, destfile = path)
foo <- fread(path)

strsplit(names(foo), "wgtp")[[123]]

# Question 2
url2 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
path2 <- "/Users/LeonardoMelo/Downloads/foo2.csv"
download.file(url2, destfile = path2)
gdp <- fread(path2, sep = ",")
gdp2 <- gdp[6:220,.(V1, V2, V4, V5)]
gdp2 <- gdp2[ !(V1 == ""), ]

library(stringr)
gdp2$V5 %>% 
  str_trim %>%
  gsub(pattern = ",", replacement = "") %>%
  as.numeric() %>%
  mean(na.rm = T)

# Question 3
countryNames <- gdp2$V4 %>% unique
x <- grep("^United",countryNames); x
length(x)

# Question 4
url3 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
path3 <- "/Users/LeonardoMelo/Downloads/data.csv"
download.file(url3, destfile = path3)
data <- fread(path3)

w <- data[, `Special Notes`] 
grep("Fiscal year end: June", w) %>% length

# Question 5
install.packages("quantmod")
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)

library(lubridate)
sum(year(sampleTimes) == "2012")
sum(year(sampleTimes) == "2012" &
    lubridate::wday(sampleTimes, label = T, abbr = F) == "Monday")




## FINAL PROJECT
library(data.table)
library(dplyr)
library(lubridate)
library(stringr)
library(tidyr)
setwd("/Users/LeonardoMelo/Downloads/UCI HAR Dataset/")
dir()

activities <- fread("activity_labels.txt")
features <- fread("features.txt")

setwd("/Users/LeonardoMelo/Downloads/UCI HAR Dataset/test/Inertial Signals/")
for (i in dir()) { 
  name <- paste("test", substr(i, 1, nchar(i) - 4) , sep = "_")
  assign(name, fread(i))
}

setwd("/Users/LeonardoMelo/Downloads/UCI HAR Dataset/train/Inertial Signals/")
for (i in dir()) { 
  name <- paste("train", substr(i, 1, nchar(i) - 4) , sep = "_")
  assign(name, fread(i))
}

setwd("/Users/LeonardoMelo/Downloads/UCI HAR Dataset/test/")
for (i in dir()) { 
  if ( substr(i, nchar(i) - 2, nchar(i)) == "txt") {
    name <- paste("test", substr(i, 1, nchar(i) - 4) , sep = "_")
    assign(name, fread(i))
  }
}

setwd("/Users/LeonardoMelo/Downloads/UCI HAR Dataset/train/")
for (i in dir()) { 
  if ( substr(i, nchar(i) - 2, nchar(i)) == "txt") {
    name <- paste("train", substr(i, 1, nchar(i) - 4) , sep = "_")
    assign(name, fread(i))
  }
}

ls()
dim(activities)
dim(features)

dim(test_body_acc_x_test)
dim(test_body_acc_y_test)
dim(test_body_acc_z_test)
dim(test_body_gyro_x_test)
dim(test_body_gyro_y_test)
dim(test_body_gyro_z_test)
dim(test_subject_test)
dim(test_total_acc_x_test)
dim(test_total_acc_y_test)
dim(test_total_acc_z_test)
dim(test_y_test)
dim(test_X_test)

setwd("/Users/LeonardoMelo/Downloads/UCI HAR Dataset/train/")
for (i in dir()) { 
  if ( substr(i, nchar(i) - 2, nchar(i)) == "txt") {
    name <- paste("train", substr(i, 1, nchar(i) - 4) , sep = "_")
    assign(name, fread(i))
  }
}

data.table(
test_body_acc_x_test %>% apply(1, mean),
test_body_acc_y_test %>% apply(1, mean),
test_body_acc_z_test %>% apply(1, mean),
test_body_acc_x_test %>% apply(1, sd),
test_body_acc_y_test %>% apply(1, sd),
test_body_acc_z_test %>% apply(1, sd),
test_body_acc_x_test %>% apply(1, mad),
test_body_acc_y_test %>% apply(1, mad),
test_body_acc_z_test %>% apply(1, mad)
)

# for (i in )
  

# Create column name (modify loop to create column Test = 0/1)
# for (i in -5:-1) {
#   eval(parse(text = paste0('foo$vigor_menos', abs(i), ' <- ifelse(foo$month == i & foo$periodo == "Day", 1, 0)')))
# }

  
sma <- function(x) {
  
}
  
energy <- function(x) {
  
}

entropy <- function(x) {
  
}

arCoeff <- function(x) {
  
}

multi.fun <- function(x) {
  c(mean = mean(x), std = sd(x), mad = mad(x), max = max(x), min = min(x),
    sma = sma(x), energy = energy(x), iqr = IQR(x), entropy = entropy(x),
    arCoeff = arCoeff(x), correlation = cor()
}
x <- apply(test_body_acc_x_test, 1, multi.fun) %>% t() %>% as.data.table



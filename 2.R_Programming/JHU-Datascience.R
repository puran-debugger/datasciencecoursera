
#JHU learning package
install.packages("swirl")
packageVersion("swirl")
library(swirl)
install_from_swirl("R Programming")
swirl()

#####practice-R-week1#####

###quiz-R-week1
#Q4
x <- 4L
class(x)
#Q5
x <- c(4, "a", TRUE) 
class(x)#"character", "lowest common denominator" ,R does automatic coercion of vectors so that all elements of the vector are the same data class.
#Q6
x <- c(1,3, 5) 
y <- c(3, 2, 10)
rbind(x, y)
#Q8
x <- list(2, "a", "b", TRUE) 
x[[2]]
class(x[[2]])
#Q9
x<-1:4;y<-2;
x+y
#Q10
x<- c(3, 5, 1, 10, 12, 6)
x[x %in% 1:5] <- 0
x[x<6]==0
x

#Q11-20
getwd()
dataweek1<-read.csv("//Users/puran/Downloads/Github/datasciencecoursera/2.R_Programming/hw1_data.csv")
dataweek1[c(1,2),]
dataweek1$Ozone#Extract the first 2 rows of the data frame 
nrow(dataweek1)# compute the number of rows in a data frame
tail(dataweek1,2)#'tail()' function is an easy way to extract the last few elements of an R object.
dataweek1[47,]
Ozonesub1<-subset(dataweek1,is.na(Ozone))#is.na test missing values
nrow(Ozonesub1)
Ozonesub2<-subset(dataweek1, !is.na(Ozone),select=Ozone)
apply(Ozonesub2,2,mean)#apply 1:row 2:column
mean(Ozonesub2$Ozone)
Solar.R1<-subset(dataweek1, Ozone > 31 & Temp > 90, select = Solar.R)
mean(Solar.R1$Solar.R)
temp1=subset(dataweek1,Month==6, select=Temp)
mean(temp1$Temp)
Ozonesub3=subset(dataweek1,Month==5 &!is.na(Ozone) ,select=Ozone)
max(Ozonesub3$Ozone)

#####practice-R-week2#####

#quiz-R-week2
#Q1
cube <- function(x, n) { 
  x^3
}
cube(3)
#Q2
x <- 1:10 
if(x > 5) {
  x <- 0
}
#Q3
f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}
z <- 10
f(3) #10
#Q4
x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}
y
#Q5
h <- function(x, y = NULL, d = 3L) {
  z <- cbind(x, d)
  if(!is.null(y))
    z <- z + y
  else
    z <- z + f
  g <- x + y / z
  if(d == 3L)
    return(g)
  g <- g + 10
  g
}

#project-week2
specdata

#####practice-R-week3#####

#quiz-R-week3
#Q1
library(datasets)
data(iris) #load specified data sets
class(iris)#data.frame
Sepvir=subset(iris,Species=="virginica",select=Sepal.Length)
mean(Sepvir$Sepal.Length)
#Q2 a vector of the means of the variables 'Sepal.Length', 'Sepal.Width', 'Petal.Length', and 'Petal.Width'
apply(iris, 2, mean)
apply(iris, 1, mean)
apply(iris[, 1:4], 1, mean)
colMeans(iris)
apply(iris[, 1:4], 2, mean)
rowMeans(iris[, 1:4])
#Q3 calculate the average miles per gallon (mpg) by number of cylinders in the car (cyl)
library(datasets)
data(mtcars)
class(mtcars)
mtcars
with(mtcars, tapply(mpg, cyl, mean)); #ok
?with
?tapply #Apply a Function Over a Ragged Array 多维数组
lapply(mtcars, mean);
?lapply #Apply a Function over a List or Vector
tapply(mtcars$cyl, mtcars$mpg, mean); #变量反了
mean(mtcars$mpg, mtcars$cyl);#error
sapply(split(mtcars$mpg, mtcars$cyl), mean); #ok
?sapply#sapply is a user-friendly version and wrapper of lapply by default returning a vector, matrix or, if simplify = "array", an array if appropriate, by applying simplify2array(). sapply(x, f, simplify = FALSE, USE.NAMES = FALSE) is the same as lapply(x, f).
split(mtcars, mtcars$cyl);
?split#Divide into Groups and Reassemble
apply(mtcars, 2, mean);
sapply(mtcars, cyl, mean);#cyl not found
tapply(mtcars$mpg, mtcars$cyl, mean);#ok
#Q4
?round#round rounds the values in its first argument to the specified number of decimal places (default 0). See ‘Details’ about “round to even” when rounding off a 5.
#mtcars_DT<-as.data.frame.table(mtcars);class(mtcars_DT)
mean(mtcars[mtcars$cyl == "8",]$hp) - mean(mtcars[mtcars$cyl == "4",]$hp) #!!!

#Q5 ？？？
debug(ls) #Execution of 'ls' will suspend at the beginning of the function and you will be in the browser.
ls

#####practice-R-week3#####

#quiz-R-week4
#Q1
set.seed(1)
rpois(5, 2)
#Q2
?qnorm#gives the quantile functio
?pnorm#gives the distribution function
?dnorm# gives the density
?rnorm#generates random deviates.
#Q3
#When simulating data, why is using the set.seed() function important? Select all that apply.
#It can be used to specify which random number generating algorithm R should use, ensuring consistency and reproducibility.
#Q4
?ppois# gives the (log) 
?qpois#gives the quantile function
?dpois#the (log) density
?rpois# generates random deviates
#Q5
set.seed(10)
x <- rep(0:1, each = 5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e
y
#Q6
?dbinom#gives the density
?pbinom#gives the distribution function
?qbinom#gives the quantile function
?rbinom#generates random deviates
#Q7
#What aspect of the R runtime does the profiler keep track of when an R expression is evaluated?
?stack
#the function call stack
#Q8
library(datasets)
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)
?summaryRprof()
#When using 'by.total' normalization, the top-level function (in this case, `lm()') always takes 100% of the time.
#Q9
#When using 'system.time()', what is the user time?
#It is the time spent by the CPU evaluating an expression
#Q10
#Question 10
#If a computer has more than one available processor and R is able to take advantage of that, then which of the following is true when using 'system.time()'?
#elapsed time may be smaller than user time  

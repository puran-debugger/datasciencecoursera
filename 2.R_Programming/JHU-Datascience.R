
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
dataweek1<-read.csv("//Users/puran/Downloads/hw1_data.csv")
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
#Q2
apply(iris, 1, mean)
apply(iris[, 1:4], 2, mean)
rowMeans(iris[, 1:4])
#Q3 ???
library(datasets)
data(mtcars)
mtcars
with(mtcars, tapply(mpg, cyl, mean));?with
lapply(mtcars, mean);?lapply
tapply(mtcars$cyl, mtcars$mpg, mean);
mean(mtcars$mpg, mtcars$cyl);
sapply(split(mtcars$mpg, mtcars$cyl), mean);
split(mtcars, mtcars$cyl);
apply(mtcars, 2, mean);
sapply(mtcars, cyl, mean);
tapply(mtcars$mpg, mtcars$cyl, mean);
#Q4

#Q5 ？？？
debug(ls) #Execution of 'ls' will suspend at the beginning of the function and you will be in the browser.
ls

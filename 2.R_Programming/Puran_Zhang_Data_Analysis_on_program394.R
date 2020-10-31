##Statistical analysis on program 394, USC Marshall's Institutional Research Office##
##puran Zhang, puranzha@usc.edu##

##Preparation
#install.packages('formattable')
library(ggplot2)
library(stringr)
library(formattable)

##Get data
appl_all_origin<-read.csv(file.choose()) #Convert the xlsx to csv and read csv
appl_all<-appl_all_origin
class(appl_all) #data.frame

appl_394=subset(appl_all,degree.program==394) #subset applicants of program 394 
appl_394_ad=subset(appl_394,admission.decision=='ADMITTED')# subset admitted student from program 394
appl_394_de=subset(appl_394,admission.decision=='DENIED')# subset denied student from program 394


##Analysis

#1.gender makeup of applicants to degree program 394
summary(appl_394$gender) #gender summary
(appl_394_gender_per<-percent(table(appl_394$gender)/sum(table(appl_394$gender)))) # percent of each gender
plot(appl_394$gender)
plot(appl_394_gender_per)


#2.ethnicity makeup of the applicants to degree program 394
summary(appl_394$ethnic.code) #ethnic code summary
str(appl_394)
appl_394$ethnic.code<-as.vector(appl_394$ethnic.code) #transform factor as string
class(appl_394$ethnic.code)#check the type

#"A,C,F,I,J,K,V,O" belong to "Asian", using A to represent "Asian" 
(appl_394$ethnic.code<-gsub('A','A',appl_394$ethnic.code))
(appl_394$ethnic.code<-gsub('C','A',appl_394$ethnic.code))
(appl_394$ethnic.code<-gsub('F','A',appl_394$ethnic.code))
(appl_394$ethnic.code<-gsub('I','A',appl_394$ethnic.code))
(appl_394$ethnic.code<-gsub('J','A',appl_394$ethnic.code))
(appl_394$ethnic.code<-gsub('K','A',appl_394$ethnic.code))
(appl_394$ethnic.code<-gsub('V','A',appl_394$ethnic.code))
(appl_394$ethnic.code<-gsub('O','A',appl_394$ethnic.code))

#"B" belong to "Black", using B to represent "Black"
(appl_394$ethnic.code<-gsub('B','B',appl_394$ethnic.code))

#"N" belong to "American Indian", using N to represent"American Indian"
(appl_394$ethnic.code<-gsub('N','N',appl_394$ethnic.code))

#"H,M,E,G,Q,L,S" belong to "Hispanic", using H to represent "Hispanic" 
(appl_394$ethnic.code<-gsub('H','H',appl_394$ethnic.code))
(appl_394$ethnic.code<-gsub('M','H',appl_394$ethnic.code))
(appl_394$ethnic.code<-gsub('E','H',appl_394$ethnic.code))
(appl_394$ethnic.code<-gsub('G','H',appl_394$ethnic.code))
(appl_394$ethnic.code<-gsub('Q','H',appl_394$ethnic.code))
(appl_394$ethnic.code<-gsub('L','H',appl_394$ethnic.code))
(appl_394$ethnic.code<-gsub('S','H',appl_394$ethnic.code))

#"X" belong to "Non Resident Alien", using X to represent"Non Resident Alien"
(appl_394$ethnic.code<-gsub('X','X',appl_394$ethnic.code))

#"P" belong to "Pacific Islander", using P to represent"Pacific Islander"
(appl_394$ethnic.code<-gsub('P','P',appl_394$ethnic.code))

#"U" belong to "Unknown", using U to represent"Unknown"
(appl_394$ethnic.code<-gsub('U','U',appl_394$ethnic.code))

#"W,D,T" belong to "White", using W to represent"White"
(appl_394$ethnic.code<-gsub('W','W',appl_394$ethnic.code))
(appl_394$ethnic.code<-gsub('D','W',appl_394$ethnic.code))
(appl_394$ethnic.code<-gsub('T','W',appl_394$ethnic.code))

#remove the repeated character from the string
#(appl_394$ethnic.code<-gsub('([[:alpha:]])\\1+', '\\1',appl_394$ethnic.code))# no need

identify_fun <- function(x){
  if(grepl('U',x,fixed = T)){
    'Unknown'}
  else if (grepl('X',x,fixed = T)){
    'Nonresident.Alien'}
  else if(grepl('H',x,fixed = T)){
    'Hispanic'}
  else if((grepl('W',x,fixed = T)&grepl('A',x,fixed = T))|(grepl('W',x,fixed = T)&grepl('P',x,fixed = T))|(grepl('W',x,fixed = T)&grepl('N',x,fixed = T))|(grepl('W',x,fixed = T)&grepl('B',x,fixed = T))|(grepl('A',x,fixed = T)&grepl('P',x,fixed = T))|(grepl('A',x,fixed = T)&grepl('N',x,fixed = T))|(grepl('A',x,fixed = T)&grepl('B',x,fixed = T))|(grepl('P',x,fixed = T)&grepl('N',x,fixed = T))|(grepl('P',x,fixed = T)&grepl('B',x,fixed = T))|(grepl('N',x,fixed = T)&grepl('B',x,fixed = T))){
    'Two.or.More'
  }
  else if(grepl('W',x,fixed = T)){
    'White'
  }
  else if(grepl('A',x,fixed = T)){
    'Asian'
  }
  else if(grepl('P',x,fixed = T)){
    'Native.Hawaiian.or.Other.Pacific.Islander'
  }
  else if(grepl('B',x,fixed = T)){
    'Black.or.African.American'
  }
  else if(grepl('N',x,fixed = T)){
    'American.Indian.or.Alaska.Native'
  }
  else{
    'na'
  }
}

appl_394$ethnicity<-mapply(identify_fun,appl_394$ethnic.code)
appl_394$ethnicity<-as.factor(appl_394$ethnicity)
summary(appl_394$ethnicity)
(appl_394_ethnicity_per<-percent(table(appl_394$ethnicity)/sum(table(appl_394$ethnicity)))) # percent 
plot(appl_394$ethnicity)
plot(appl_394_ethnicity_per)

#3.acceptance rate for degree program 394 among female and male applicant
summary(appl_394_ad$gender)
summary(appl_394$gender)
#acceptance rate among Female, Male and Undentified as follows
percent(as.vector(summary(appl_394_ad$gender))/as.vector(summary(appl_394$gender)))

#4.Quality of the admitted students into degree program 394
#test NA
which(is.na(appl_394_ad$high.school.gpa))
which(is.na(appl_394_ad$sat))
which(is.na(appl_394_de$high.school.gpa)) # find one NA in denied list
appl_394_de=subset(appl_394_de,!is.na(high.school.gpa)) #omit na
which(is.na(appl_394_de$high.school.gpa)) #check, 0 NA
which(is.na(appl_394_de$sat))

#summary quality
summary(appl_394_ad)
par(mfrow=c(1,2))
boxplot(appl_394_ad$high.school.gpa, data=appl_394_ad, xlab='high.school.gpa')
boxplot(appl_394_ad$sat, data=appl_394_ad,xlab='sat')
par(mfrow=c(1,1))

#5.test if there any significant difference in quality between admitted students and those denied to degree program 394
summary(appl_394_ad)
summary(appl_394_de)

#Z-test (for the large samples, we use Z-test instead of T-test) 
(appl_394_ad_var1<-var(appl_394_ad$high.school.gpa))#var.ad.gpa
(appl_394_ad_var2<-var(appl_394_ad$sat))#var.ad.sat
(appl_394_de_var1<-var(appl_394_de$high.school.gpa))#var.de.gpa
(appl_394_de_var2<-var(appl_394_de$sat))#var.de.sat

z.test2sam= function(a, b, var.a, var.b){ #Z-test function
  n.a = length(a)
  n.b = length(b)
  zeta = (mean(a)-mean(b)) / (sqrt(var.a/n.a+var.b/n.b))
  return(zeta)
}

z.test2sam(appl_394_ad$high.school.gpa,appl_394_de$high.school.gpa, appl_394_ad_var1, appl_394_de_var1 ) #test gpa #33.00886
z.test2sam(appl_394_ad$sat, appl_394_de$sat, appl_394_ad_var2, appl_394_de_var2) #test sat #40.85195
# The value of zeta is greater than the value of the critical value zeta tabulated for alpha equal to 0.05 (z-tabulated = 1.96 for a two-tailed test)
# Then we reject the null hypothesis in favor of the alternative hypothesis
# We conclude that the two means of high.school.gpa and sat are significantly different.
# In other words, there is  significant  difference in quality (gpa & sat) between admitted students and those denied to degree program 394 





#install.packages("ggplot2")
library(ggplot2)
library(dplyr)
library(prob)
library(distr)
library(tidyverse)

#setwd
dir_path<-"C:\\Users\\user\\OneDrive\\Documents"
setwd(dir_path)

#Import our data set into R
med_ins <- read.csv("medical_insurance.csv")
#medIns

#do some descriptive analysis to better understand the data
head(med_ins)
summary(med_ins)
sapply(med_ins, summary)
mean(med_ins$age)
mean(med_ins$charges)
sd(med_ins$charges)
range(med_ins$charges)
IQR(med_ins$age)
var(med_ins$age)


#Creating histogram to understand frequency of ages
hist(med_ins$age)
ggplot(med_ins, aes(x=age))+geom_histogram(binwidth = 0.5)

#Creating histogram to understand frequency of bmi
ggplot(med_ins, aes(x=bmi))+
  geom_histogram(binwidth = 0.5, col="blue", fill="lightblue")+
  theme_minimal()

#Creating bar plots to understand frequency or 
#distribution of each category.
ggplot(med_ins, aes(x=sex, fill=sex))+geom_bar()
ggplot(med_ins, aes(x=smoker, fill=smoker))+geom_bar()
ggplot(med_ins, aes(x=region, fill=region))+geom_bar()


#Create a plot to compare smoker status vs charges
ggplot(med_ins, aes(x = smoker, y = charges, fill = smoker)) +
  geom_violin() +
  labs(x = "Smoker Status", y = "Charges") +
  theme_minimal()

#Creating Box plots visualizing the distribution of 
#numerical data across different categories. 

ggplot(med_ins, aes(x = smoker, y = charges, color = smoker))+
  geom_boxplot()

ggplot(med_ins, aes(x = region, y = charges, color = region))+
  geom_boxplot()

#Creating relationships between two continuous variables

ggplot(med_ins, aes(x = age, y = charges))+
  geom_point()+geom_smooth(method = lm, se = FALSE)


table( med_ins$sex, med_ins$children)

ggplot(med_ins, aes(x = age, y = charges, color=sex)) +
  geom_boxplot() +
  facet_grid(sex ~ smoker)

ggplot(med_ins, aes(x = age, y = charges, color=sex)) +
  geom_boxplot() +
  facet_wrap(~smoker)

#hypothesis testing
#conduct t-test comparing charges to smoking
t.test(data=med_ins, charges~smoker)
t.test(data=med_ins, age~smoker)
t.test(data=med_ins, charges~sex)

#chisq 
chisq.test(med_ins$sex, med_ins$region, correct=FALSE)
chisq.test(med_ins$age, med_ins$smoker, correct=FALSE)


# AOV analysis
AOVmed<-aov(med_ins$children~med_ins$charges + med_ins$region)
summary(AOVmed)

AOVmed2<-aov(med_ins$age~med_ins$charges + med_ins$sex)
summary(AOVmed2)

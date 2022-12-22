#Installing packages

install.packages("ggpubr")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("gtsummary")
install.packages("Hmisc")


install.packages("corrplot")
install.packages("dummies")
install.packages("fastDummies")
install.packages("devtools")
devtools::install_github("cardiomoon/ggiraphExtra")
install.packages("ggiraphExtra")
install.packages("ggplot2")

require(ggplot2)
require(ggiraph)
require(ggiraphExtra)
library(devtools)
library(dplyr)
library(corrplot)
library(gtsummary)
library(fastDummies)
library(dummies)
library(ggplot2)
library(Hmisc)
library(gtsummary)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(ggiraphExtra)

#Setting working directory & Importing csv file into R script
setwd("E:\Yash Work\BE Project\Projects\Data Projects\Heart Disease Prediction")
HeartFailure<- read.csv("heart.csv",header=TRUE, sep=",")

#To understand the various attributes of the data and to view the data set
names(HeartFailure)
View(HeartFailure)

# Statistical summary of the data set
summary(HeartFailure)
str(HeartFailure)

#visualizations scatter plot
ggplot(HeartFailure,aes(x=Age,y=Cholesterol,group=Sex))+
  geom_point(aes(color=Sex))+
  scale_color_manual(values=c('Blue','Pink'))

#creating subset of numeric variables for creating histogram.
df <- HeartFailure[,c("RestingBP", "Cholesterol", "MaxHR",
                      "Age")]

#creating hist.data.frame to temporarily redefine the hist() function and to change the colors
hist.data.frame <- function(x, ..., colors=rainbow(ncol(x))) {
  col<-1
  hist<-function(...) {
    graphics::hist(..., col=colors[col])
    col <<- col+1
  }
  f <- Hmisc:::hist.data.frame
  environment(f) <- environment()
  f(x,...)
}

#histogram for all numeric variables
hist.data.frame(df, title= "Histogram of all Numerical Variables")
summary(df)


#assigning value to heart disease column instead of yes or no
HeartFailure$HeartDisease  <- ifelse(HeartFailure$HeartDisease ==1,"Yes","No")


#scatter plot for Resting BP vs Cholesterol for people with and without heart disease
ggplot(HeartFailure) +geom_point(aes(x=RestingBP, y= Cholesterol, 
                                     colour= HeartDisease)) +ggtitle( "RestingBP vs Cholesterol")+
  theme(plot.title = element_text(hjust = 0.5))


#Jitter plot for cholesterol levels in people
ggplot(HeartFailure) + 
  labs(y="Cholesterol")+
  labs(title="Jitter plot for Cholesterol levels in people with and without Heart disease")+
  geom_jitter(aes(x=HeartDisease , y= Cholesterol , color=HeartDisease)
,position = position_jitter(0.2)) + 
  stat_summary(aes(x=HeartDisease, y= Cholesterol),fun = mean, color="blue")+
  theme(plot.title = element_text(hjust = 0.5))


#created a subset table for correlation table
HeartFailure_Cor <- subset(HeartFailure,select=c(RestingBP,Cholesterol,FastingBS,MaxHR,HeartDisease))

#changing the datatype to numeric
HeartFailure_Cor[1:5] <- lapply(HeartFailure_Cor[1:5], as.numeric)

#checking if all the variables have datatype as numeric
str(HeartFailure_Cor)

#correlation table for heart data set
cor <- cor(HeartFailure_Cor)

#correlation plot for heart disease prediction
corrplot(cor)

corrplot(cor, method = 'number') 

#Regression model 1

reg_model<-lm(MaxHR~RestingBP+Cholesterol, data=HeartFailure)
reg_model
summary(reg_model)
tbl_regression(reg_model)

#regression model 2

reg_model2<-lm(MaxHR~RestingBP+Cholesterol+Oldpeak,data=HeartFailure)
reg_model2
summary(reg_model2)
tbl_regression(reg_model2)

#regression model 3
reg_model3<-lm(RestingBP~Cholesterol+MaxHR+Oldpeak,data=HeartFailure)
reg_model3
summary(reg_model3)
tbl_regression(reg_model3)

#regression line on scatter plot
ggplot(HeartFailure_Cor, aes(x = Cholesterol, y = MaxHR)) + geom_point() +
  stat_smooth(method = lm) + labs(title = "Regression Analysis", x = "Cholesterol", y = "Max HR")


#Part 1- Regression analysis for creating dummy variables
#created subset tables for sex and Exercise Angina
Male<- HeartFailure[HeartFailure$Sex=="M",]
Female <- HeartFailure[HeartFailure$Sex=="F",]
ExerciseAngina_yes <- HeartFailure[HeartFailure$ExerciseAngina=="Y",]
ExerciseAngina_no <- HeartFailure[HeartFailure$ExerciseAngina=="N",]

#scatter plot for cholesterol vs Max Heart rate and regression analysis
#model 1 regression and plot with regression line
plot(HeartFailure$Cholesterol~HeartFailure$MaxHR)
model1 <- lm(HeartFailure$Cholesterol~HeartFailure$MaxHR)
summary(model1)
ggplot(HeartFailure,aes(y=Cholesterol,x=MaxHR,colour=Sex))+geom_point()+
  geom_smooth(method="lm")+ggtitle( "Cholesterol vs MaxHR")+
  theme(plot.title = element_text(hjust = 0.5))

#Model 2 regression and plot with regression line
plot(HeartFailure$Cholesterol~HeartFailure$RestingBP)
model2<-lm(HeartFailure$Cholesterol~HeartFailure$RestingBP)
ggplot(HeartFailure,aes(y=Cholesterol,x=RestingBP,colour=Sex))+
  geom_point()+geom_smooth(method="lm")+ggtitle( "Cholesterol vs Resting BP")+
  theme(plot.title = element_text(hjust = 0.5))

#Model 3 regression and plot with regression line
model3<-lm(Cholesterol~RestingBP+MaxHR, data = HeartFailure)
equation1=function(x){coef(model3)[2]*x+coef(model3)[1]}
equation2=function(x){coef(model3)[2]*x+coef(model3)[1]+coef(model3)[3]}
ggplot(data = HeartFailure ,aes(y=Cholesterol,x=MaxHR,color=ExerciseAngina))+geom_point()+
  geom_smooth(method="lm")+ggtitle( "Cholesterol vs Max HR")+
  theme(plot.title = element_text(hjust = 0.5))
stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])
ggPredict(model3,se=TRUE,interactive=TRUE)

#model 4 regression and plot with regression line
model4<-lm(Cholesterol~Age+ExerciseAngina, data = HeartFailure)
equation1=function(x){coef(model4)[2]*x+coef(model4)[1]}
equation2=function(x){coef(model4)[2]*x+coef(model4)[1]+coef(model4)[3]}
ggplot(data = HeartFailure ,aes(y=Cholesterol,x=Age,color=ExerciseAngina))+geom_point()+
  geom_smooth(method="lm")+ggtitle( "Cholesterol vs Age")+
  theme(plot.title = element_text(hjust = 0.5))
stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])
ggPredict(model4,se=TRUE,interactive=TRUE)

# created two dummy variables for sex and exercise angina
df1<-dummy(HeartFailure$Sex, sep = "_")
HeartFailure<-cbind(HeartFailure, df1)

df2<-dummy(HeartFailure$ExerciseAngina, sep = "_")
HeartFailure<-cbind(HeartFailure, df2)

#analysis after creating dummy variables
HeartFailure[c("Sex", "ExerciseAngina")]<-NULL
head(HeartFailure)
summary(HeartFailure)
head(HeartFailure)

#regression analysis and plot for dummy variables 
model5<-lm(HeartFailure$Age ~ Sex_M+ExerciseAngina_Y, data = HeartFailure)
model5

require(ggplot2)
ggplot(HeartFailure,aes(y=MaxHR,x=Sex_M + ExerciseAngina_Y))+geom_point()+geom_smooth(method="lm")


## 5. Part 2 - Regression for each subset
#Sex
mod_male <- lm(Cholesterol ~ MaxHR, data = Male)
summary(mod_male)
ggplot(Male, aes(x = MaxHR, y = Cholesterol )) + geom_point( color="#94ce7b", size=6, alpha = 0.5)+
  ggtitle("Cholesterol in Males") + xlab("Max Heart Rate") + ylab("Cholesterol")+
  stat_smooth(method = lm, se = F)+ theme(plot.title = element_text(hjust = 0.5))


#Exercise Angina
mod_EA <- lm(Cholesterol ~ MaxHR, data = ExerciseAngina_yes)
summary(mod_EA)
ggplot(ExerciseAngina_yes, aes(x = MaxHR, y = Cholesterol )) + 
  geom_point( color="#99004C", size=6, alpha = 0.5)+
  ggtitle("Cholesterol in people with Exercise Angina") + xlab("MaxHR") + ylab("Cholesterol")+
  stat_smooth(method = lm, se = F)+ theme(plot.title = element_text(hjust = 0.5))
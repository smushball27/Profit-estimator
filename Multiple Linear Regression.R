rm(list = ls()) #Clears environment

setwd("C:/College materials/Softwares/R software/Datasets") #Set corresponding dir
dataset=read.csv("50_Startups.csv",header=T)

#install.packages("caTools") #If caTools package not installed
library(caTools) 

set.seed(70) #To lock the code
split=sample.split(dataset$Profit,SplitRatio=0.8)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)


# regressor=lm(formula = Profit ~ .,data=training_set) 
# summary(regressor) #Linear model

# On building model with all the independent variables you could see that except "R.D.spend"
# are having no impact to the dependent variable. So we build our model with only "R.D.spend"

regressor=lm(formula = Profit ~ R.D.Spend,data=training_set) 
summary(regressor) #Linear model

y_test =predict(regressor,newdata = test_set)
y_train = predict(regressor,newdata=training_set)

print(y_train)
#Result
print(y_test)

#install.packages("ggplot2")  #If ggplot2 package not installed
library(ggplot2)

#Training data in graph format
ggplot() + 
  geom_point(aes(x=training_set$R.D.Spend,y=training_set$Profit),colour="red") +
  geom_line(aes(x=training_set$R.D.Spend,y=y_train),colour="blue")+
  xlab("R.D.Spend") + ylab("Profit") + 
  ggtitle("Multiple Linear regression sample - Training set")

#Testing data in graph format

ggplot() + 
  geom_point(aes(x=test_set$R.D.Spend,y=test_set$Profit),colour="red") +
  geom_line(aes(x=training_set$R.D.Spend,y=y_train),colour="purple")+
  xlab("R.D.Spend") + ylab("Profit") + 
  ggtitle("Multiple Linear regression sample - Test set")





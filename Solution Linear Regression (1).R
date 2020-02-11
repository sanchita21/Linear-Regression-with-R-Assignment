
#Boston Pricing case study

setwd("D:\\")

##Loading Data
prices<-read.csv("boston_prices.csv",header=TRUE,stringsAsFactors=FALSE)

##Checking Data Characteristics
dim(prices)
str(prices)
head(prices)
names(prices)

#summary statistics
summary(prices)

#Missing values treatment
colSums(is.na(prices)) #MEDV has a lot of missing values
summary((prices$MEDV))
prices$MEDV[is.na(prices$MEDV)]<-mean(prices$MEDV,na.rm=TRUE)

#Outlier plots
par(mfrow=c(2,7)) #This allows you to plot 14 charts on a single page; It is optional.
list<-names(prices) #Store the names of the dataset in a list format
list<-list[-4]
for(i in 1:length(list)) #Plot the boxplots of all variables and shortlist which ones need outlier treatment.
{
  boxplot(prices[,list[i]],main=list[i])
}

#Restore the par parameters to normal
dev.off()

#In this solution, We have replaced the outlier values by the median values
#You can decide to replace by max or mean values based on business objectives

#Outlier treatment
for(i in 1:length(list)) ##For loop to replace all the outlier values with the mean value ; if you want you can replace with median value as well.
{
    x<-boxplot(prices[,list[i]])
    out<-x$out
    index<-which(prices[,list[i]] %in% x$out)
    prices[index,list[i]]<-mean(prices[,list[i]])
    rm(x)
    rm(out)
}


#Exploratory analysis
library(ggplot2)

#Study the histogram of the DV and the transformed histogram
hist(prices$MEDV)
#hist(prices$log_MEDV) #Once you create the transformations;look down

#You can look at the correlation between each IDV and the DV
#An eg :
ggplot(prices,aes(x=MEDV,y=LSTAT)) +geom_point()
ggplot(prices,aes(x=MEDV,y=DIS)) +geom_point()
ggplot(prices,aes(x=MEDV,y=AGE)) +geom_point()

#Inorder to quicken the process, lets write a function :
#Below is a function that gives you the correlation values between all IDV's and the DV
#Simply taking a look at the output of this function, you can quickly shortlist 
#Which all IDV's are correlated to the DV

#Function to get the list of correlations between : DV and the IDV's
list1<-list[-13]
for(i in 1:length(list1))
{
  x<-cor(prices$MEDV,prices[list[i]])
  print(x)
}

#Significant variables are : B LSTAT AGE X.rooms.dwelling nitric.oxides.concentration INDUS

#You can also try to use data transformations

#Log transformations
#Create the log transformation for all variables
prices$log_CRIM<-log(prices$CRIM)
prices$log_ZN<-log(prices$ZN)
prices$log_NOX<-log(prices$nitric.oxides.concentration)
prices$log_RM<-log(prices$X.rooms.dwelling)
prices$log_AGE<-log(prices$AGE)
prices$log_DIS<-log(prices$DIS)
prices$log_RAD<-log(prices$RAD)
prices$log_TAX<-log(prices$TAX)
prices$log_PTRATIO<-log(prices$PTRATIO)
prices$log_B<-log(prices$B)
prices$log_LSTAT<-log(prices$LSTAT)
prices$log_MEDV<-log(prices$MEDV) #DV
prices$log_INDUS<-log(prices$INDUS)


#Refer to the profiling excel sheet to see all the correlations documented

#Function to get the list of correlations between : log_DV and log of IDV's

list_log<-names(prices)[c(15:25,27)]
for(i in 1:length(list_log))
{
  xlog<-cor(prices$log_MEDV,prices[list_log[i]])
  print(xlog)
}

#Function to get the list of correlations between : log_DV and IDV's

list_log_DV<-names(prices)[1:13]
list_log_DV<-list_log_DV[-4]
for(i in 1:length(list_log_DV))
{
  xlogdv<-cor(prices$log_MEDV,prices[list_log_DV[i]])
  print(xlogdv)
}


sampling<-sort(sample(nrow(prices), nrow(prices)*.7))

#Select training sample
train<-prices[sampling,]
test<-prices[-sampling,]

##Building SimpLe Linear Regression Model

#Metrics :
#Rsquare
#Coefficients
#P values : Significance levels of the IDV's
#Residuals distribution

#Factor variables as IDV's
#All good modelssummm
Reg<-lm(log_MEDV~CRIM+INDUS+RAD+TAX+B+
                 Charles.River.dummy.variable+
                  DIS+ZN+PTRATIO+LSTAT+AGE+X.rooms.dwelling+nitric.oxides.concentration,data=train)

summary(Reg)

#Getting the formula
formula(Reg)

#Getting the formula
formula(Reg)

#Remove insignificant variables :

Reg1<-lm(log_MEDV~
          Charles.River.dummy.variable+
          DIS+PTRATIO+LSTAT+AGE+X.rooms.dwelling+nitric.oxides.concentration,data=train)
summary(Reg1)


#Reg2 : remove insignificant values

Reg2 <- lm(log_MEDV ~CRIM+INDUS+RAD+TAX+B+
                 Charles.River.dummy.variable+
                 DIS+ZN+PTRATIO+LSTAT+X.rooms.dwelling+nitric.oxides.concentration, data=train)
summary(Reg2)

#Reg3 _ remove insignificant values
Reg3 <- lm(log_MEDV ~CRIM+RAD+
             Charles.River.dummy.variable+
             DIS+ZN+PTRATIO+LSTAT+nitric.oxides.concentration, data=train)
summary(Reg3)

#Some other combination 
Reg4<-lm(log_MEDV~INDUS  +ZN + X.rooms.dwelling + LSTAT+CRIM + Charles.River.dummy.variable,data=train)
summary(Reg4)

#The best model happens to be : Reg3

##Getting predicted values
predicted<-predict(Reg3)
plot(predicted)
length(predicted)


##Finding Residuals
residuals<-resid(Reg3)
plot(residuals)
length(residuals)

##Plotting Residuals vs Predicted Values
##Checking Heteroskedastcity
##There should be no trend between predicted values and residual values
plot(predicted,residuals,abline(0,0))

#You can notice that there seems to be an inverse pattern for some points

#So this model may not be the preferred model.

#atttching predicted values to test data
predicted<-predict(Reg3,newdata=test)
length(predicted)
test$p<-predicted

#Calculating error in the test dataset - (Actual- predicted)/predicted values
test$error<-(test$log_MEDV-test$p)/test$log_MEDV
mean(test$error)*100 #you get to know the average error in the given dataset



##Plotting actual vs predicted values
plot(test$p,col="blue",type="l")
lines(test$log_MEDV,col="red",type="l")

#checking for Correlation between variables
library(car)
vif(Reg3)

#You can drop variables if they have a vif>10 ; means high correlation between variables




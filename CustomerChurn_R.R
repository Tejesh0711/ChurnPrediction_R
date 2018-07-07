library(plyr)
install.packages("corrplot")
library(corrplot)
library(ggplot2)
library(gridExtra)
installed.packages("ggthemes")
library(ggthemes)
library(caret)
library(MASS)
library(randomForest)
install.packages("party")
library(party)
churn <- read.csv('E://Projects//CustomerChurn//Telco_Customer_Churn.csv')
str(churn)

#sapply to check number of missing values in each column
sapply(churn, function(x) sum(is.na(x)))
#removing all missing values from TotalCharges
churn <- churn[complete.cases(churn), ]

#Data Wrangling - Change "No internet service" to "No" for 6 columns
cols_recode1 <- c(10:15)
for(i in 1:ncol(churn[,cols_recode1])) {
  churn[,cols_recode1][,i] <- as.factor(mapvalues
                                        (churn[,cols_recode1][,i], from =c("No internet service"),to=c("No")))
}

# Change "No phone service" to "No" for "Multiple Lines"
churn$MultipleLines <- as.factor(mapvalues(churn$MultipleLines, 
                                           from=c("No phone service"),
                                           to=c("No")))

#find max and min of tenure
min(churn$tenure); max(churn$tenure)

#Group tenure into 5 bins
group_tenure <- function(tenure){
  if (tenure >= 0 & tenure <= 12){
    return('0-12 Month')
  }else if(tenure > 12 & tenure <= 24){
    return('12-24 Month')
  }else if (tenure > 24 & tenure <= 48){
    return('24-48 Month')
  }else if (tenure > 48 & tenure <=60){
    return('48-60 Month')
  }else if (tenure > 60){
    return('> 60 Month')
  }
}

#convert tenure_group values to the bins we created
churn$tenure_group <- sapply(churn$tenure,group_tenure)
churn$tenure_group <- as.factor(churn$tenure_group)

#Change columns in "Senior Citizen" from 0,1 to "No", "Yes"
churn$SeniorCitizen <- as.factor(mapvalues(churn$SeniorCitizen,
                                           from=c("0","1"),
                                           to=c("No", "Yes")))

churn$customerID <- NULL
churn$tenure <- NULL


numeric.var <- sapply(churn, is.numeric)
corr.matrix <- cor(churn[,numeric.var])
corrplot(corr.matrix, main="\n\nCorrelation Plot for Numerical Variables", method="number")

#The plot shows that TotalCharges and MonthlyCharges are correlated, hence we remove one of them (TotalCharges)
churn$TotalCharges <- NULL

#Split data into training and testing sets:
intrain<- createDataPartition(churn$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- churn[intrain,]
testing<- churn[-intrain,]
dim(training); dim(testing)

#Implement and train using Logistic Model
LogModel <- glm(Churn ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))

#Analysis of Varianace - statistical method to test difference between 2 or more means and likelihood ratio using chi-squared
anova(LogModel, test="Chisq")

#Predict the churn column for testing set using the LogModel
testing$Churn <- as.character(testing$Churn)
testing$Churn[testing$Churn=="No"] <- "0"
testing$Churn[testing$Churn=="Yes"] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$Churn)
print(paste('Logistic Regression Accuracy',1-misClasificError))

#Confusion Matrix
print("Confusion Matrix for Logistic Regression"); 
table(testing$Churn, fitted.results > 0.5)




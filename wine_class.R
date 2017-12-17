# Load packages and datasets
library(readxl)
library(rpart)
library(rpart.plot)
library(caret)
library(party)
library(partykit)
install.packages("pastecs")
library(pastecs)

#Load 2k DB:
exerc1 <- read_excel("~/Documents/GitHub/wine_class/Exer1.xls")
View(exerc1)
#Load 8k DB:
customersDB <- read_excel("~/Documents/GitHub/wine_class/finalDS.xlsx")

#Discriptive analysis
options(scipen=100)
options(digits = 2)
stat.desc(exerc1, basic = FALSE)
summary(exerc1)
sapply(Exer1, class)
table(exerc1$Spcork)
histogram(exerc1$Spcork)


prop.table(table(exerc1$Spcork))
print("Only 0.0725 in 2k customers bought the product")


#Check some main var
print("Kids at Home")
prop.table(table(exerc1$Spcork,exerc1$Kidhome))
print("Teens at Home?")
prop.table(table(exerc1$Spcork,exerc1$Teenhome))

#Dataset only with buyers
buyers <- exerc1[which(exerc1$Spcork == 1), ]
notbuyers <- exerc1[which(exerc1$Spcork == 0), ]
stat.desc(buyers, basic = FALSE)
stat.desc(notbuyers, basic = FALSE)

boxplotB_NB <- function(column_name){return(boxplot(c(buyers[column_name], notbuyers[column_name]), col=(c("light green", "#CD5C5C")), main="Comparision Between Buyers and Non-Buyers", xlab=("column_name")))}
boxplotB_NB("Age")
boxplotB_NB("Monetary")
boxplotB_NB("Freq")
boxplotB_NB("Income")
  
#Decision Tree with rpart
#Method = "class" if you have a classification tree
#Method = "anova" if you have a regression tree

colnames(exerc1)
dtwines <-rpart(formula = Spcork ~ Dayswus + Income + Recency + Monetary + Age + Edu + Teenhome + Kidhome + Freq, data = exerc1, method = "class")
printcp(dtwines)
as.party(dtwines) 
#Now we see how many variables don't really matter")
#Variables actually used in tree construction: Dayswus  Income   Monetary Recency


#Run the model only with the proper var + rpart.control
dtwine_reduced <- rpart(Spcork ~ 
                   Dayswus + Income + Recency + Monetary, 
                   data = exerc1, method="class", control = rpart.control(minsplit = 40, cp = 0.005))

#Info + viz on Decision Tree
as.party(dtwine_reduced)
printcp(dtwine_reduced)
rpart.plot(dtwine_reduced)


#Making Predictions
myprediction <- predict(dtwine_reduced, newdata = customersDB, type="class")
plot(myprediction)
mysolution <- data.frame(customersDB, Spcork = myprediction)
mysolution


#Confusing Matrix files (testing model in the 2k set)
dtwines2k <- exerc1
pred_test <- predict(dtwine_reduced, newdata = dtwines2k, type="class")
pred_test_ds <- data.frame(ID=dtwines2k$Custid, Spcork=dtwines2k$Spcork, Pred=pred_test) 

#Testing Da
#ConfusionMatrix for 2k
confusionMatrix2k <- confusionMatrix(pred_test_ds$Spcork, pred_test_ds$Pred)
confusionMatrix2k

#Accuracy: Overall, how often is the classifier correct?
#Misclassification Rate: Overall, how often is it wrong? 1 minus Accuracy ("Error Rate")


############################################Model 2

model2 <- rpart(Spcork ~ 
                          Dayswus + Income + Recency + Monetary, 
                        data = exerc1, method="class", control = rpart.control(minsplit = 10, cp = 0.002))
##Info + viz on Decision Tree
as.party(model2)
printcp(model2)
##PRED
dtwines_model2 <- exerc1
pred_model2 <- predict(model2, newdata = dtwines_model2, type="class")
pred_model2_ds <- data.frame(ID=dtwines_model2$Custid, Spcork=dtwines_model2$Spcork, Pred=pred_model2) 
##Test
confusionMatrix_model2 <- confusionMatrix(pred_model2_ds$Spcork, pred_model2_ds$Pred)
confusionMatrix_model2


####Train and Test Set
##Dividing the sample intro train/test set
smp_size <- floor(0.7 * nrow(exerc1))
set.seed(123)
train_ind <- sample(seq_len(nrow(exerc1)), size = smp_size)
train_wines <- exerc1[train_ind, ]
test_wines <- exerc1[-train_ind, ]
nrow(train_wines)+nrow(test_wines)
nrow(train_wines)
nrow(test_wines)

#Model3 (with train_data)

model_3 <- rpart(Spcork ~ Dayswus + Income + Recency + Monetary, 
                 data = train_wines, method="class", 
                 control = rpart.control(minsplit = 10, cp = 0.005))

#Prediction
myprediction3 <- predict(model_3, newdata = test_wines, type = "class")
plot(myprediction3)

#Solution
mysolution3 <- data.frame(test_wines, testingspcork = myprediction3)

#Confusion Matrix
confusionMatrix3 <- confusionMatrix(mysolution3$Spcork, mysolution3$testingspcork)
confusionMatrix3


#Decision Tree on Predicted Data aka.. Testing solution
#Load 8k table with out predictions

prop.table(table(mysm$Spcork))

testsolution<- rpart(formula = Spcork ~ Dayswus + Income + Recency + Monetary, data = solution_merged, 
                     method = "class",
                     control = rpart.control(minsplit = 40, cp = 0))
printcp(testsolution)
as.party(testsolution)
rpart.plot(testsolution)

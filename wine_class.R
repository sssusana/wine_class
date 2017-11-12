# Load packages and datasets
library(readxl)
library(rpart)
library(rpart.plot)
library(caret)
library(party)
library(partykit)

#Load 2k DB:
exerc1 <- read_excel("~/Documents/GitHub/wine_class/Exer1.xls")
#Load 8k DB:
customersDB <- read_excel("~/Documents/GitHub/wine_class/finalDS.xlsx")

#Discriptive analysis
summary(exerc1)
sapply(Exer1, class)
table(exerc1$Spcork)
prop.table(table(exerc1$Spcork))
print("Only 0.0725 in 2k customers bought the product")

#Check some main var
print("Kids at Home")
prop.table(table(exerc1$Spcork,exerc1$Kidhome))
print("Teens at Home?")
prop.table(table(exerc1$Spcork,exerc1$Teenhome))

#Dataset only with buyers
buyers <- exerc1[which(exerc1$Spcork == 1), ]

summary(exerc1)
summary(buyers)
comb <- rbind(cbind(buyers, table = "Buyers"), cbind(exerc1, table="All"))
s <- summary(table ~., data=comb, method= "reverse")
print(s, exclude1 = TRUE)

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


#Confusing Matrix files (testing model in the 2k)
dtwines2k <- exerc1
pred_test <- predict(dtwine_reduced, newdata = dtwines2k, type="class")
pred_test_ds <- data.frame(ID=dtwines2k$Custid, Spcork=dtwines2k$Spcork, Pred=pred_test) 

#Testing Da
#ConfusionMatrix for 2k
confusionMatrix2k <- confusionMatrix(pred_test_ds$Spcork, pred_test_ds$Pred)
confusionMatrix2k


############################################Model 2

model2 <- rpart(Spcork ~ 
                          Dayswus + Income + Recency + Monetary, 
                        data = exerc1, method="class", control = rpart.control(minsplit = 10, cp = 0.002))
#Info + viz on Decision Tree
as.party(model2)
printcp(model2)
#PRED
dtwines_model2 <- exerc1
pred_model2 <- predict(model2, newdata = dtwines_model2, type="class")
pred_model2_ds <- data.frame(ID=dtwines_model2$Custid, Spcork=dtwines_model2$Spcork, Pred=pred_model2) 
#Test
confusionMatrix_model2 <- confusionMatrix(pred_model2_ds$Spcork, pred_model2_ds$Pred)
confusionMatrix_model2


#Decision Tree on Predicted Data aka.. Testing solution
#Load 8k table with out predictions

prop.table(table(mysm$Spcork))

testsolution<- rpart(formula = Spcork ~ Dayswus + Income + Recency + Monetary, data = solution_merged, 
                     method = "class",
                     control = rpart.control(minsplit = 40, cp = 0))
printcp(testsolution)
as.party(testsolution)
rpart.plot(testsolution)

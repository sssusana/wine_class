# Load packages and files 
library(readxl)
library(rpart)
library(rpart.plot)
library(caret)
library(party)
library(partykit)
exerc1 <- read_excel("~/Documents/GitHub/wine_class/Exer1.xls")
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

#DS only with buyers
buyers <- exerc1[which(exerc1$Spcork == 1), ]

#Decision Tree with rpart
#Method = "class" if you have a classification tree
#Method = "anova" if you have a regression tree
#Variables actually used in tree construction: Dayswus  Income   Monetary Recency 
colnames(dt_wines)
dtwines <-rpart(formula = Spcork ~ Dayswus + Income + Recency + Monetary + Age + Edu + Teenhome + Kidhome + Freq, data = dt_wines, method = "class")
printcp(dtwines)
as.party(dtwines) 
print("Now we see how many variables don't really matter")

#Only with the proper var + rpart.control
dtwine_control <- rpart(Spcork ~ 
                   Dayswus + Income + Recency + Monetary, 
                   data = dt_wines, method="class", control = rpart.control(minsplit = 40, cp = 0.005))

#Info + viz on Decision Tree
as.party(dtwine_control)
printcp(dtwine_control)
rpart.plot(dtwine_control)


#Making Predictions
myprediction <- predict(dtwine_control, newdata = customersDB, type="class")
plot(myprediction)
mysolution <- data.frame(Custid=customersDB$Custid, Spcork = myprediction)
solution_merged <- merge(customersDB, mysolution, by="Custid")

#Confusing Matrix files (testing model in the 2k)
dtwines2k <-dt_wines
predkfold <- predict(dtwine_control, newdata = dtwines2k, type="class")
predkfold_sol <- data.frame(Custid=dt_wines$Custid, kfold=predkfold) 
predkfold_merged <- merge(dt_wines, predkfold_sol, by="Custid")

#ConfusionMatrix for 2k
confusionMatrix2k <- confusionMatrix(predkfold_merged$kfold, predkfold_merged$Spcork)
confusionMatrix2k

#Saving
write.table(solution_merged, file= "mysm.xls", col.names = TRUE)

dt_solution <- read_excel("~/Documents/GitHub/wine_class/mysm.xls")

#Decision Tree on Predicted Data aka.. Testing solution
prop.table(table(mysm$Spcork))

testsolution<- rpart(formula = Spcork ~ Dayswus + Income + Recency + Monetary, data = mysm, 
                     method = "class",
                     control = rpart.control(minsplit = 40, cp = 0))
printcp(testsolution)
as.party(testsolution)
rpart.plot(testsolution)

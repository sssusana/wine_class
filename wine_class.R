# Load packages and files 
library(readxl)
library(rpart)
exerc1 <- read_excel("~/Documents/GitHub/wine_class/Exer1.xls")
View(Exer1)

#Discriptive analysis
summary(exerc1)
sapply(Exer1, class)
table(exerc1$Spcork)

#Check some main var
print("Kids at Home")
prop.table(table(exerc1$Spcork,exerc1$Kidhome))
print("Teens at Home?")
prop.table(table(exerc1$Spcork,exerc1$Teenhome))

#DS only with buyers

buyers <- exerc1[which(exerc1$Spcork == 1), ]

#Decision Tree with rpart

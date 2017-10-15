# Load packages and files 
library(readxl)
exerc1 <- read_excel("~/Documents/GitHub/Wine_Classification/Exer1.xls")
View(Exer1)

#Discriptive analysis
summary(exerc1)
sapply(Exer1, class)
table(exerc1$Spcork)

#Check some main var
print("Kids Home")
prop.table(table(exerc1$Spcork,exerc1$Kidhome))

prop.table(table(exerc1$Spcork,exerc1$Teenhome))

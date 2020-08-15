library(tidyverse)
library(corrplot)
library(shiny)
library(RColorBrewer)
library(xtable)
library(shinydashboard)
library(beanplot)

genderCol <- c("#007ea7","#f27059")
smokCol <- c("#52796f","maroon")
smokText <- c("non-smoker","smoker")

csvData <- read_csv("data/datasets_insurance.csv")
corrData <- cor(csvData)
sexCount <- table(csvData$sex)
names(sexCount) <- c("F","M")
sexCount<- as.data.frame(sexCount)
colnames(sexCount) <- c("Gender","Count")

smokerCount <- table(csvData$smoker)
names(smokerCount) <- c("non-smoker","smoker")
smokerCount<- as.data.frame(smokerCount)
colnames(smokerCount) <- c("Type","Count")

smokerData <- csvData %>% filter(smoker == 1)
nonsmokerData <- csvData %>% filter(smoker == 0)
smokerMean <- format(round(mean(smokerData$charges),0), nsmall = 0)
nonsmokerMean <- format(round(mean(nonsmokerData$charges),0), nsmalla = 0) 

sexsmokerData <- csvData[,c("sex", "smoker", "charges")]
aggMean <- aggregate(charges ~ sex+smoker,sexsmokerData, mean )
diffMale <- round(diff(aggMean$charges[aggMean$sex ==1] ),0)
diffFemale <- round(diff(aggMean$charges[aggMean$sex ==0] ),0)

agesmokerData <- csvData[,c("bmi", "sex","age", "smoker", "charges")]
levels(csvData$sex) <- c("0","1","M","F")
csvData$sex[csvData$sex == 1] <- "M"
csvData$sex[csvData$sex == 0] <- "F"
levels(csvData$sex) <- c("M","F")

modelData <- csvData
csvData$intsmoker <- csvData$smoker 

 levels(csvData$smoker) <- c("0","1","Y","N")
 csvData$smoker[csvData$smoker == 1] <- "Y"
 csvData$smoker[csvData$smoker == 0] <- "N"
 levels(csvData$smoker) <- c("N","Y")

formula <- as.formula("charges ~ smoker + age + bmi")
model <- lm(formula, data = agesmokerData)
r_sq <- summary(model)$r.squared

prediction <- predict(model, newdata = agesmokerData)

residuals <- agesmokerData$charges - prediction
rmse <- sqrt(mean(residuals^2))
summ <- summary(model)



agesmokerData$prediction <- predict(model, newdata = agesmokerData)
ggplot(agesmokerData, aes(x = prediction, y = charges)) + 
  geom_point(color = "#007ea7", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")

Bob <- data.frame(age = 19,
                  bmi = 27.9,
                  smoker = 1)
# print(paste0("Health care charges for Bob: ", round(predict(model, Bob), 2)))



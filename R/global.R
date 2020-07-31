library(tidyverse)
library(corrplot)
library(shiny)
library(RColorBrewer)
library(xtable)
library(shinydashboard)
library(beanplot)

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
smokerMean <- format(round(mean(smokerData$charges),2), nsmall = 2)
nonsmokerMean <- format(round(mean(nonsmokerData$charges),2), nsmall = 2) 

sexsmokerData <- csvData[,c("sex", "smoker", "charges")]

agesmokerData <- csvData[,c("age", "smoker", "charges")]

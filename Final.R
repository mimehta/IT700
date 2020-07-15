library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(xtable)

csvData <- read_csv("C:/00snhulocal/IT-700/finalP/datasets_26475_38092_insurance2.csv")
txData <- read_csv("C:/00snhulocal/IT-700/finalP/datasets_26475_38092_insurance2.csv",
    col_types = cols(
      age = col_integer(),
      sex = col_factor(),
      bmi = col_double(),
      children = col_integer(),
      smoker = col_factor(),
      region = col_factor(),
      charges = col_double(),
      insuranceclaim = col_factor()
    )
)
# check data
head(txData)
# find the NA
colSums(is.na(csvData))
              
corrData <- cor(csvData)

corrplot(corrData)
round(corrData["charges",],digits = 2)
# shows the strong relationship with smoker. Surprise to see the weak relationship with bmi
smokerData <- csvData %>% filter(smoker == 1)
nonsmokerData <- csvData %>% filter(smoker == 0)
mean(smokerData$charges)
mean(nonsmokerData$charges)

hist(smokerData$charges, col = "orange", main="charges for smoker", xlab="Charges in $", ylab="# of cases")
hist(nonsmokerData$charges, col = "orange", main="charges for nonsmoker", xlab="Charges in $", ylab="# of cases")
abline(csvData$smoker, csvData$charges)

boxplot(nonsmokerData$charges,smokerData$charges, 
        at = c(1,2), col = c("blue","red"), 
        horizontal = T, notch = T, 
        main="charges comparision", 
        names  = c("nonsmoker", "smoker"), 
        xlab="charges")

plot(csvData[csvData$charges < 17000,c("age", "charges")], col = "blue4", main="relation of age and charge")
sexsmokerData <- csvData[,c("sex", "smoker", "charges")]
#### Not Used #####
chargesSplit <- with(sexsmokerData, split(sexsmokerData, list(sex, smoker)))
chargesList <- lapply(seq_along(chargesList), function(ls)  as.data.frame(chargesList[[ls]])[,3] )
#### Not Used #####
boxplot(charges ~ sex + smoker, sexsmokerData, horizontal = F, notch = T, 
               main="charges comparision for smoker- gender wise", at = c(1,2,3,4), col = c("blue","red"),outline=FALSE)


agesmokerData <- csvData[,c("age", "smoker", "charges")]

##  to be continue
ageChargeVect2 <- unlist(lapply(seq(min(agesmokerData$age), max(agesmokerData$age) ) , function(n) c(mean(agesmokerData[agesmokerData$age == n & agesmokerData$smoker ==0,]$charges)) ) )


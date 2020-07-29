library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(xtable)

library(ggplot2)
histogram <- function(x1, x2, binwidth = 500, xlim = c(0, 70000)) {
  df <- data.frame(
    x = c(x1, x2),
    g = c(rep("x1", length(x1)), rep("x2", length(x2)))
  )
  
  ggplot(df, aes(x, fill = g)) +
    geom_dotplot(binwidth = binwidth) +
    scale_y_continuous(NULL, breaks = NULL) +
    coord_fixed( xlim = xlim,ylim = c(0,30000))
#    coord_cartesian(xlim = xlim, ylim = c(0,4000))
}

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
summary(txData)
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
        horizontal = F, notch = T, 
        main="charges comparision", 
        names  = c("nonsmoker", "smoker"), 
        xlab="charges")
legend(1, 30000, c(   paste0("Mean:",mean(smokerData$charges)  ) ) , bg = "lightblue"  )

plot(csvData[csvData$charges < 17000,c("age", "charges")], col = "blue4", main="relation of age and charge")
sexsmokerData <- csvData[,c("sex", "smoker", "charges")]
#### Not Used #####
#chargesSplit <- with(sexsmokerData, split(sexsmokerData, list(sex, smoker)))
#chargesList <- lapply(seq_along(chargesList), function(ls)  as.data.frame(chargesList[[ls]])[,3] )
#### Not Used #####
boxplot(charges ~ sex + smoker, sexsmokerData, horizontal = F, notch = T, 
               main="charges comparision for smoker- gender wise", at = c(1,2,3,4), 
        col = c("#226699","#FF5511"),outline=FALSE,names=c("","","","") )
legend(1, 55000, c("female", "male"),  fill=c("#226699","#FF5511"))
legend(1, 30000, c("non-smoker"),bg = "lightblue"  )
legend(3.5, 5000, c("smoker"), bg = "#FFBB77")

agesmokerData <- csvData[,c("age", "smoker", "charges")]

##  to be continue
ageChargeVect2 <- unlist(lapply(seq(min(agesmokerData$age), max(agesmokerData$age) ) , function(n) c(mean(agesmokerData[agesmokerData$age == n & agesmokerData$smoker ==0,]$charges)) ) )

 histogram(smokerData$charges,nonsmokerData$charges)
 
tt<- csvData %>% count(smoker, sex, wt=charges)
mm<- csvData %>% count(smoker, sex) %>%
  full_join(tt, by = c("smoker","sex")) %>%
  mutate("avg" = n.y/n.x)
#          fill_color="#036564", line_color="#033649")
smoker18 <- agesmokerData[agesmokerData$age == 40 & agesmokerData$smoker == 1,]
nonsmoker18 <- agesmokerData[agesmokerData$age == 40 & agesmokerData$smoker == 0,]

boxplot(nonsmoker18$charges,smoker18$charges, 
        at = c(1,2), col = c("blue","red"), 
        horizontal = F, notch = F, outline = F,
        main="charges comparision", 
        names  = c("nonsmoker", "smoker"), 
        xlab="charges")
legend(0.8, 20000, paste0("mean:",round(mean(smoker18$charges) , digits = 2)),bg = "lightblue"  )
legend(1.6, 10000, paste0("mean:",round(mean(nonsmoker18$charges) , digits = 2)),bg = "lightblue"  )


plot(agesmokerData$age, agesmokerData$charges,pch=16, col=c("#226699","#FF5511")[agesmokerData$smoker + 1])
legend(17, 60000, c("non-smoker","smoker"),fill = c("#226699","#FF5511")  )

abline(lm(charges ~ age, data = filter(agesmokerData,smoker ==0)),col ="#226699"  )
abline(lm(charges ~ age, data = filter(agesmokerData,smoker ==1)) , col= "#FF5511")


#lines(filter(agesmokerData,smoker ==0)$age, filter(agesmokerData,smoker ==0)$charges)
boxplot(charges ~ age + smoker, agesmokerData, horizontal = F, notch = F, 
        main="charges comparision for smoker- gender wise", at = 1:94, 
        col = c("#226699","#FF5511"),outline=FALSE )
 
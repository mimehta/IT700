library(tidyverse)
library(corrplot)
library(RColorBrewer)
library(xtable)
library(beanplot)

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
genderCol <- c("aqua","orange")
smokCol <- c("#008080","maroon")
smokText <- c("non-smoker","smoker")

ggplot(csvData,aes(group=smoker,color=smoker, x=charges )) +
  geom_density( ) 

hist(csvData$charges[csvData$smoker==1], freq = FALSE, col = smokCol[1+1],density = 7,
     main = paste0("Distribution of charges for ",smokText[1+1]),xlab = "Charges", ylim = c(0,0.00009)  )
lines(density(csvData$charges[csvData$smoker==1]),col = smokCol[1+1], lwd=3 )
lines(density(csvData$charges[csvData$smoker==0]),col = smokCol[0+1], lwd=3 )
legend(30000, 0.00005, smokText,  fill=smokCol)
legend(17000, 0.00005, "Charges", col = "black",density = 15)

hist(csvData$charges, freq = FALSE,col = "black",density = 12, 
     main = paste0("Distribution of charges"),xlab = "Charges", ylim = c(0,0.00009))
lines(density(csvData$charges),col = "black", lwd=3 )
lines(density(csvData$charges[csvData$smoker==0]),col = smokCol[0+1], lwd=3 )
lines(density(csvData$charges[csvData$smoker==1]),col = smokCol[1+1], lwd=3 )
abline(v=mean(csvData$charges), lwd=3, lty=3)
legend(11000, 0.00007, "Average",bty = "n")
legend(30000, 0.00005, smokText, fill=smokCol)
legend(17000, 0.00005, "Charges", col = "black",density = 15)



curve(dnorm(x,mean = mean(csvData$charges[csvData$smoker==0]), sd = sd(csvData$charges[csvData$smoker==0])), add=TRUE,col="red")

mean(csvData$charges[csvData$smoker==1])
sd(csvData$charges[csvData$smoker==1])


lines(csvData$charges[csvData$smoker==1])

hist(smokerData$charges, col = "orange", main="charges for smoker", xlab="Charges in $", ylab="# of cases")
hist(nonsmokerData$charges, col = "orange", main="charges for nonsmoker", xlab="Charges in $", ylab="# of cases")
abline(csvData$smoker, csvData$charges)
library("GGally")




barplot(table(csvData$sex, csvData$age), beside = T)

boxplot(nonsmokerData$charges,smokerData$charges, 
        at = c(1,2), col = c("blue","red"), 
        horizontal = F, notch = T, 
        main="charges comparision", 
        names  = c("nonsmoker", "smoker"), 
        xlab="charges")
legend(1, 30000, c(   paste0("Mean:",mean(smokerData$charges)  ) ) , bg = "lightblue"  )
rect(0, 0, 1.45, 60000, lwd = 5, border = smokCol[1])
rect(1.55, 0, 2.5, 60000, lwd = 5, col = adjustcolor( smokCol[2],alpha = 0.5))


plot(csvData[csvData$charges < 17000,c("age", "charges")], col = "blue4", main="relation of age and charge")
sexsmokerData <- csvData[,c("sex", "smoker", "charges")]
#### Not Used #####
#chargesSplit <- with(sexsmokerData, split(sexsmokerData, list(sex, smoker)))
#chargesList <- lapply(seq_along(chargesList), function(ls)  as.data.frame(chargesList[[ls]])[,3] )
#### Not Used #####
boxplot(charges ~ sex + smoker, sexsmokerData, horizontal = F, notch = T, 
               main="charges comparision for smoker- gender wise", at = c(1,2,3,4), 
        col = genderCol,outline=FALSE,names=c("","","","") )
rect(0, 0, 2.5, 70000, col = adjustcolor(smokCol[1],alpha = 0.1)  )
rect(2.5, 0, 4.5, 70000,  col = adjustcolor( smokCol[2],alpha = 0.1))

legend(1, 45000, c("female", "male"),  fill=genderCol,bty = "n")
legend(1, 60000, c("non-smoker"),bty = "n" )
legend(3, 60000, c("smoker"), bty = "n")

agesmokerData <- csvData[,c("bmi","sex","age", "smoker", "charges")]

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

beanplot(nonsmoker18$charges,smoker18$charges, what = c(1,1,1,0),side = "both",
        at = c(1,2), col = list("blue","red"), beanlinewd=4,
        horizontal = F, notch = F, outline = F,
        main="charges comparision", 
        names  = c("nonsmoker", "smoker"), 
        xlab="charges")


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
 


plot(
  agesmokerData$age,
  agesmokerData$charges,
  pch = 16,xlab = "Age", ylab = "Charges",
  col = genderCol[agesmokerData$sex + 1]
)
legend(17, 60000, c("female", "male"),bty = "n", fill = genderCol)

abline(lm(charges ~ age, data = agesmokerData), lwd = 2, col = "black" )
abline(lm(charges ~ age, data = filter(agesmokerData, sex == 0)), lwd = 2,col = genderCol[1])
abline(lm(charges ~ age, data = filter(agesmokerData, sex == 1)) ,lwd = 2, col = genderCol[2])


plot(
  agesmokerData$bmi,
  agesmokerData$charges,
  pch = 16,
  col = smokCol[agesmokerData$smoker + 1]
)
legend(17, 60000, smokText,bty = "n", fill = smokCol)

abline(lm(charges ~ bmi, data = agesmokerData), lwd =3, col = "black" )
abline(lm(charges ~ bmi, data = filter(agesmokerData, smoker == 0)), lwd =3,col = smokCol[1])
abline(lm(charges ~ bmi, data = filter(agesmokerData, smoker == 1)) ,lwd =3, col = smokCol[2])


plot(
  agesmokerData$age,
  agesmokerData$charges,
  pch = 16,xlab = "Age", ylab = "Charges",
  col = smokCol[agesmokerData$sex + 1]
)
legend(17, 60000, c("non-smoker", "smoker"),bty = "n", fill = smokCol)

abline(lm(charges ~ age, data = agesmokerData), lwd = 2, col = "black" )
abline(lm(charges ~ age, data = filter(agesmokerData, smoker == 0)), lwd = 2,col = smokCol[1])
abline(lm(charges ~ age, data = filter(agesmokerData, smoker == 1)) ,lwd = 2, col = smokCol[2])



beanplot(
  agesmokerData[round(agesmokerData$bmi,digits = 0) == 18 &
                  agesmokerData$smoker == 0,]$charges,
  agesmokerData[round(agesmokerData$bmi,digits = 0) == 18 &
                  agesmokerData$smoker == 1,]$charges,
  at = c(1, 2),
  col = list("#52796f","maroon"),
  what = c(1, 1, 1, 0),
  side = "both",
  horizontal = F,
  notch = F, 
  outline = F,
  axes = T,
  names  = c("", ""),
  ylab = "charges",
  xlab = "nonsmoker:smoker"
)
legend(0.5,
       50000,
       c("non-smoker", "smoker"),
       fill = smokCol)

#kaggle.com/ruslankl/health-care-cost-prediction-w-linear-regression
n_train <- round(0.8 * nrow(csvData))
train_indices <- sample(1:nrow(csvData), n_train)
Data_train <- csvData[train_indices, ]
Data_test <- csvData[-train_indices, ]
formula_0 <- as.formula("charges ~ age + sex + bmi + children + smoker + region")
model_0 <- lm(formula_0, data = csvData)
summary(model_0)
r_sq_0 <- summary(model_0)$r.squared
#predict data on test set
prediction_0 <- predict(model_0, newdata = Data_test)
#calculating the residuals
residuals_0 <- Data_test$charges - prediction_0
#calculating Root Mean Squared Error
rmse_0 <- sqrt(mean(residuals_0^2))

Data_test$prediction <- predict(model_1, newdata = Data_test)
ggplot(Data_test, aes(x = prediction, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")

Data_train$prediction <- predict(model_1, newdata = Data_train)
ggplot(Data_train, aes(x = prediction, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "red") +
  ggtitle("Prediction vs. Real values")

csvData$prediction <- predict(model_1, newdata = csvData)
ggplot(csvData, aes(x = prediction, y = charges)) + 
  geom_point(color = "blue", alpha = 0.7) + 
  geom_abline(color = "maroon") +
  ggtitle("Prediction vs. Real values")

Bob <- data.frame(age = 19,
                  bmi = 27.9,
                  children = 0,
                  smoker = 1,
                  region = 2)
residualPlot(model_1, type = "rstandard")

csvData$residuals <- csvData$charges - csvData$prediction

plot(csvData$residuals, csvData$prediction)

ggplot(data = csvData, aes(x = prediction, y = residuals)) +
  geom_pointrange(aes(ymin = 0, ymax = residuals), color = "blue", alpha = 0.7) +
  geom_hline(yintercept = 0, linetype = 3, color = "maroon") +
  ggtitle("Residuals vs. Linear model prediction")

print(paste0("Health care charges for Bob: ", round(predict(model_1, Bob), 2)))
pr1 <- predict(model_1, Bob,se.fit = TRUE)


## https://www.kaggle.com/ruslankl/health-care-cost-prediction-w-linear-regression
## https://www.kaggle.com/grosvenpaul/regression-eda-and-statistics-tutorial
## https://www.kaggle.com/sinaasappel/tutorial-multiple-regression
## https://www.kaggle.com/jmullan/eda-and-linear-regression-prediction
## https://www.kaggle.com/krohitm/linear-regression-modeling-and-assumptions


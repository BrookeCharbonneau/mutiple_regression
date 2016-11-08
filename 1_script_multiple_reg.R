library(tidyverse)
library(apaTables)


# load data
my.data <- read_csv("regLectureData.csv")

glimpse(my.data)


#Correlation table
apa.cor.table(my.data)


#make sure data isn't curvilinear, so regression makes sense
psych::pairs.panels(as.data.frame(my.data))


#do the regression
my.regression <- lm(VidScore ~ iq + age, data = my.data)

#gets the b-weights for variables and age
my.regression

#"Estimate" column is b-weight
summary(my.regression)

#Get b -weights, beta-weights, sr2, r, and R2
apa.reg.table(my.regression)


#best guess of VidScore for partucular age _ IQ
x_axis_range <- data.frame(age = c(43), iq=c(130))
x_axis_range
CI_data <- predict(my.regression, newdata = x_axis_range, interval = "confidence", level=0.95)
CI_data <- as.data.frame(cbind(x_axis_range, CI_data))
CI_data
PI_data <- predict(my.regression, newdata = x_axis_range, interval = "prediction", level=0.95)
PI_data <- as.data.frame(cbind(x_axis_range, PI_data))
PI_data
# For individuals who are 43 years old with an IQ of 130, the best guess of the mean video game score is
# 128, with a CI of 126-141. 
# For individuals who are 43 years old with an IQ of 130, the best guess of the mean video game score is
# 128, but 95% of video game scores for future samples would range from 110 to 147.


#hierarchical regression example
head(attitude)
reg1 <- lm(rating ~ complaints + privileges, data=attitude)
reg2 <- lm(rating ~ complaints + privileges + learning, data=attitude)
#F value and significance for delta r squared - ie. change in r between 2 regressiosn
#if delta r is significant, no point in incuding the new varialbe
anova(reg1, reg2)
#OR use tables - if delta is not starred, it's not sig, so don't include it
apa.reg.table(reg1, reg2)


# loading libraries
library("readxl")
library("tidyverse")
library("lattice")
library("rstatix")
library("ggpubr")
library("lme4")
library("fitdistrplus")
library("lmerTest")
library("car")

# loading xls files
my_data <- read_excel("ImageJ_R.xlsx", sheet = 1)

# combining relevant sheets of excel file
for (i in 2:36) {
  temp <- read_excel("ImageJ_R.xlsx", sheet = i)
  my_data <- rbind(my_data, temp)
  print(i)
}

# removing unnecessary columns
my_data <- my_data[c(1,2,4,17)]

# turn dates into number of days from start (07/02/2020)
startdate <- as.Date("2020-02-07","%Y-%m-%d")
my_data$Date_days <- as.numeric(difftime(my_data$Date,startdate ,units="days"), units="days")

# change headers of column survival
my_data = my_data %>% 
  rename(
    Survival = `Survival (%)`
  )

# removing rows containing NAs (so the rows that don't contain the average values for survival)
my_data <- na.omit(my_data)

# write.csv(my_data,"test.csv", row.names = FALSE)

## check repeated measures ANOVA requirements

# visualising data with box plots

boxplot(my_data$`Survival` ~ my_data$Date,
        data=my_data, 
        main="Survival", 
        xlab="Date", 
        ylab="Survival")

# normality tests
ggqqplot(my_data, x = "Survival")

hist(my_data$Survival)

shapiro_test(my_data$Survival)

# Testing for homogeneity of variances
leveneTest(my_data$Survival, my_data$Treatment)
fligner.test(my_data$Survival, my_data$Treatment)
# Variances are NOT homogeneous

# Testing for sphericity
# Mauchly's Test for Sphericity!

# Testing which distribution fits the data best

test <- fitdist(my_data$Survival, "norm")
plot(test)
test$aic

test <- fitdist(my_data$Survival, "lnorm")
plot(test)
test$aic

test <- fitdist(my_data$Survival, "pois")
plot(test)
test$aic

test <- fitdist(my_data$Survival, "exp")
plot(test)
test$aic

test <- fitdist(my_data$Survival, "gamma")
plot(test)
test$aic

test <- fitdist(my_data$Survival, "nbinom")
plot(test)
test$aic

test <- fitdist(my_data$Survival, "geom")
plot(test)
test$aic

test <- fitdist((my_data$Survival/100), "beta", method = "mme")
plot(test)
test$aic

test <- fitdist(my_data$Survival, "unif")
plot(test)
test$aic

test <- fitdist(my_data$Survival, "logis")
plot(test)
test$aic

# A beta distribution fits best, so this will be used in the model

# Generalized Linear Mixed Model with Repeated Measures

Model <- glmer(Survival~Treatment+Date_days + (1|Structure), family=gaussian, data=my_data)
print(summary(Model),correlation=FALSE)
plot(Model)
qqnorm(resid(Model))


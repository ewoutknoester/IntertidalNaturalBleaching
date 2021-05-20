# loading libraries
library("readxl")
library("tidyverse")
library("lattice")
library("rstatix")
library("ggpubr")
library("lme4")
library("fitdistrplus")
library("lmerTest")

# loading xls files
my_data <- read_excel("ImageJ_R.xlsx", sheet = 2)

# combining relevant sheets of excel file
for (i in 3:19) {
  temp <- read_excel("ImageJ_R.xlsx", sheet = i)
  my_data <- rbind(my_data, temp)
  print(i)
}

# removing unnecessary columns
my_data <- my_data[c(1,3,5,14)]

startdate <- as.Date("2020-02-07","%Y-%m-%d")
my_data$Date2 <- as.numeric(difftime(my_data$Date,startdate ,units="days"), units="days")

my_data = my_data %>% 
  rename(
    Bleaching = `Adjusted brightness (0 - 255)`
  )

# removing rows containing NAs (so the rows that don't contain the average values for brightness/Bleaching)
my_data <- na.omit(my_data)

# write.csv(my_data,"test.csv", row.names = FALSE)

## check repeated measures ANOVA requirements

# visualising data with box plots

boxplot(my_data$`Bleaching` ~ my_data$Date,
        data=my_data, 
        main="Bleaching", 
        xlab="Date", 
        ylab="Bleaching")

# normality tests
ggqqplot(my_data, x = "Bleaching")

hist(my_data$Bleaching)

shapiro_test(my_data$Bleaching)

# not normal -> sqrt transformation
my_data <- transform(my_data, `SQRT Bleaching`= sqrt(Bleaching))

ggqqplot(my_data, x = "SQRT.Bleaching")

hist(my_data$SQRT.Bleaching)

shapiro_test(my_data$SQRT.Bleaching)

# still not normal -> log10 transformation
my_data <- transform(my_data, LOG10.Bleaching= log(Bleaching))

ggqqplot(my_data, x = "LOG10.Bleaching")

hist(my_data$LOG10.Bleaching)

shapiro_test(my_data$LOG10.Bleaching)

# still not normal -> 1/x transformation
my_data <- transform(my_data, INVERSE.Bleaching= 1/Bleaching)

ggqqplot(my_data, x = "INVERSE.Bleaching")

hist(my_data$INVERSE.Bleaching)

shapiro_test(my_data$INVERSE.Bleaching)

# Testing which distribution fits the data best

test <- fitdist(my_data$Bleaching, "norm")
#plot(test)
test$aic

test <- fitdist(my_data$Bleaching, "lnorm")
plot(test)
test$aic

test <- fitdist(my_data$Bleaching, "pois")
#plot(test)
test$aic

test <- fitdist(my_data$Bleaching, "exp")
#plot(test)
test$aic

test <- fitdist(my_data$Bleaching, "gamma")
#plot(test)
test$aic

test <- fitdist(my_data$Bleaching, "nbinom")
#plot(test)
test$aic

test <- fitdist(my_data$Bleaching, "geom")
#plot(test)
test$aic

test <- fitdist(my_data$Bleaching, "beta")
#plot(test)
test$aic

test <- fitdist(my_data$Bleaching, "unif")
#plot(test)
test$aic

test <- fitdist(my_data$Bleaching, "logis")
#plot(test)
test$aic

# A lognormal distribution fits best, so this will be used in the model

# Generalized Linear Mixed Model with Repeated Measures

#Model <- glmer(Bleaching~Origin+Date2 + (1|Structure), family=gaussian, data=my_data)
#print(summary(Model),correlation=FALSE)

Model2 <- glmer(log(Bleaching)~Origin+Date2 + (1|Structure), family=gaussian, data=my_data)
print(summary(Model2),correlation=FALSE)

#Outcome is: Origin has a t-value of 0.410 and Date has a t-value of 0.759
#T-values have to be >= 1.960 (because of high degrees of freedom and 2-tailed test)
#Therefore, null hypothesis is NOT rejected

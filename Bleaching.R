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
my_data <- my_data[c(1,2,4,13)]

# turn dates into number of days from start (07/02/2020)
startdate <- as.Date("2020-02-07","%Y-%m-%d")
my_data$Date_days <- as.numeric(difftime(my_data$Date,startdate ,units="days"), units="days")

# change headers of column bleaching
my_data = my_data %>% 
  rename(
    Bleaching = `Adjusted brightness (0 - 255)`
  )

# removing rows containing NAs (so the rows that don't contain the average values for bleaching)
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
#my_data <- transform(my_data, `SQRT Bleaching`= sqrt(Bleaching))
#ggqqplot(my_data, x = "SQRT.Bleaching")
#hist(my_data$SQRT.Bleaching)
#shapiro_test(my_data$SQRT.Bleaching)

# not normal -> log10 transformation: Works best!
my_data <- transform(my_data, LOG10.Bleaching= log(Bleaching))
ggqqplot(my_data, x = "LOG10.Bleaching")
hist(my_data$LOG10.Bleaching)
shapiro_test(my_data$LOG10.Bleaching)

# not normal -> 1/x transformation
# my_data <- transform(my_data, INVERSE.Bleaching= 1/Bleaching)
# ggqqplot(my_data, x = "INVERSE.Bleaching")
# hist(my_data$INVERSE.Bleaching)
# shapiro_test(my_data$INVERSE.Bleaching)

# Testing for homogeneity of variances
leveneTest(my_data$Bleaching, my_data$Treatment)
fligner.test(my_data$Bleaching, my_data$Treatment)
# Variances are NOT homogeneous

# Testing for sphericity
# Mauchly's Test for Sphericity!


# Testing which distribution fits the data best

test <- fitdist(my_data$Bleaching, "norm")
#plot(test)
test$aic

test <- fitdist(my_data$Bleaching, "lnorm")
plot(test)
test$aic
#This one works best!

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

Model <- glmer(LOG10.Bleaching~Treatment+Date_days + (1|Structure), data=my_data, family = gaussian(link = "log"))
print(summary(Model),correlation=FALSE)
plot(Model)
qqnorm(resid(Model))


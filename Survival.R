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

# removing 07/02 and 11/05 because of inaccurate measurements
my_data<-my_data[!(my_data$Date_days=="0" | my_data$Date_days=="94"),]

# change headers of column survival
my_data = my_data %>% 
  rename(
    Survival = `Survival (%)`
  )

# removing rows containing NAs (so the rows that don't contain the average values for survival)
my_data <- na.omit(my_data)

# Make treatment and days factors
my_data$Treatment <- as.factor(my_data$Treatment)
class(my_data$Treatment)
my_data$Date_days <- as.factor(my_data$Date_days)
class(my_data$Date_days)

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

# Generalized Linear Mixed Model with Repeated Measures
Model <- glmer(Survival~Treatment+Date_days + (1|Structure), family=gaussian, data=my_data)
print(summary(Model),correlation=FALSE)
plot(Model)
qqnorm(resid(Model))


# Setup
rm(list=ls()) # Clear workspace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set directory at current (make sure file is saved locally)

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

# combining relevant sheets of excel file (each structure had it's own sheet)
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

# removing 07/02 and 11/05 because of inaccurate measurements
my_data<-my_data[!(my_data$Date_days=="0" | my_data$Date_days=="94"),]

# change headers of column bleaching
my_data = my_data %>% 
  rename(
    Bleaching = `Adjusted brightness (0 - 255)`
  )

# removing rows containing NAs (so the rows that don't contain the average values for bleaching)
my_data <- na.omit(my_data)

# Make treatment and days factors
my_data$Treatment <- as.factor(my_data$Treatment)
class(my_data$Treatment)
my_data$Date_days <- as.factor(my_data$Date_days)
class(my_data$Date_days)

# visualizing data with box plots
boxplot(my_data$`Bleaching` ~ my_data$Date,
        data=my_data, 
        main="Bleaching", 
        xlab="Date", 
        ylab="Bleaching")

# normality tests
ggqqplot(my_data, x = "Bleaching")
hist(my_data$Bleaching)
shapiro_test(my_data$Bleaching)

# not normal -> log10 transformation helps
my_data <- transform(my_data, LOG10.Bleaching= log(Bleaching))
ggqqplot(my_data, x = "LOG10.Bleaching")
hist(my_data$LOG10.Bleaching)
shapiro_test(my_data$LOG10.Bleaching)

# Testing for homogeneity of variances
leveneTest(my_data$Bleaching, my_data$Treatment)
fligner.test(my_data$Bleaching, my_data$Treatment)
# Variances are NOT homogeneous

# Testing for sphericity
# Mauchly's Test for Sphericity!

# Generalized Linear Mixed Model with Repeated Measures
Model <- lmer(LOG10.Bleaching~Treatment*Date_days + (1|Structure), data=my_data)
print(summary(Model),correlation=FALSE)
plot(Model)
qqnorm(resid(Model))






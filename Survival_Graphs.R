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
my_data <- my_data[c(1,2,4,21)]

# turn dates into number of days from start (07/02/2020)
startdate <- as.Date("2020-02-07","%Y-%m-%d")
my_data$Date_days <- as.numeric(difftime(my_data$Date,startdate ,units="days"), units="days")

# removing 07/02 and 11/05 because of inaccurate measurements
my_data<-my_data[!(my_data$Date_days=="0" | my_data$Date_days=="94"),]

# change headers of columns AVG survival
my_data = my_data %>% 
  rename(
    AVG_Survival = `AVG Survival`
  )

# removing rows containing NAs (so the rows that don't contain the average values for survival)
my_data <- na.omit(my_data)

# Make treatment and days factors
my_data$Treatment <- as.factor(my_data$Treatment)
class(my_data$Treatment)
my_data$Date_days <- as.factor(my_data$Date_days)
class(my_data$Date_days)

# creating average values for survival per treatment and date
my_data <- as.tbl(my_data) %>% 
  bind_rows(my_data) %>% 
  group_by(Treatment, Date_days) %>% 
  summarise_all(c("mean"))

################### FROM THIS POINT ONLY RUN SEPERATE ITEMS ###################

# To create scatter plots with line: replace type = "l" with type=c("p","smooth")

# Create graph for difference between all treatments
xyplot(my_data$AVG_Survival ~ my_data$Date, 
       data = my_data, 
       groups=my_data$Treatment,
       main="Survival",
       col=c("magenta", "green3", "red", "blue"),
       lwd=2,
       lty = c("solid", "longdash", "solid", "longdash"),
       key=list(
         corner=c(1,0.95),
         text = list(c("Intertidal to intertidal", "Intertidal to subtidal", "Subtidal to intertidal", "Subtidal to subtidal")),
         lines = list(lwd=2,
                      lty = c("solid", "longdash", "solid", "longdash"),
                      col=c("magenta", "green3", "red", "blue"))),
       type = c("l","g"), 
       xlab="Date", 
       ylab="Survival (%)")

# Create graph for difference between IntToInt and SubToSub
my_data<-my_data[!(my_data$Treatment=="IntToSub" | my_data$Treatment=="SubToInt"),]
xyplot(my_data$AVG_Survival ~ my_data$Date, 
       data = my_data, 
       groups=my_data$Treatment,
       main="Survival in intertidal and subtidal zone",
       col=c("green3","dodgerblue3"),
       lwd=2,
       key=list(
         corner=c(1,0.95),
         text = list(c("Intertidal to intertidal", "Subtidal to subtidal")),
         lines = list(lwd=2, col=c("green3","dodgerblue3"))),
       type = c("l","g"), 
       xlab="Date", 
       ylab="Survival (%)")


# Create graph for difference between IntToInt and SubToInt
my_data<-my_data[!(my_data$Treatment=="SubToSub" | my_data$Treatment=="IntToSub"),]
xyplot(my_data$AVG_Survival ~ my_data$Date, 
       data = my_data, 
       groups=my_data$Treatment,
       main="Survival in intertidal zone",
       col=c("green3","dodgerblue3"),
       lwd=2,
       key=list(
         corner=c(1,0.95),
         text = list(c("Intertidal to intertidal", "Subtidal to intertidal")),
         lines = list(lwd=2, col=c("green3","dodgerblue3"))),
       type = c("l","g"), 
       xlab="Date", 
       ylab="Survival (%)")


# Create graph for difference between SubToSub and IntToSub
my_data<-my_data[!(my_data$Treatment=="IntToInt" | my_data$Treatment=="SubToInt"),]
xyplot(my_data$AVG_Survival ~ my_data$Date, 
       data = my_data, 
       groups=my_data$Treatment,
       main="Survival in subtidal zone",
       col=c("green3","dodgerblue3"),
       lwd=2,
       key=list(
         corner=c(1,0.95),
         text = list(c("Intertidal to subtidal", "Subtidal to subtidal")),
         lines = list(lwd=2, col=c("green3","dodgerblue3"))),
       type = c("l","g"), 
       xlab="Date", 
       ylab="Survival (%)")





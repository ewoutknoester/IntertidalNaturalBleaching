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
library("latticeExtra")
library("dplyr")

# loading xls files
my_data <- read_excel("Temperature_R.xlsx", sheet = 2)

# creating line graph
xyplot(my_data$MAX + my_data$AVG + my_data$MIN ~ my_data$Date, 
       data = my_data,
       main="Sea Surface Temperature (SST) of intertidal zone",
       col=c("red", "gold2", "dodgerblue"),
       lwd=2,
       key=list(
         corner=c(1,0.95),
         text = list(c("Maximum", "Average", "Minimum")),
         lines = list(lwd=2, col=c("red", "gold2", "dodgerblue"))),
       type = c("l","g"), 
       xlab="Date", 
       ylab="Sea Surface Temperature (SST) (ºC)",
       ylim=c(25, 36))


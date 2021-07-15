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
my_data <- read_excel("Temperature_R.xlsx", sheet = 1)

# Drop rows after 31 July
my_data <- my_data[-c(183:225), ]

# creating line graph
Temp_graph <- xyplot(my_data$`Int AVG` + my_data$`Sub AVG` + my_data$NOAA + my_data$`AVG MMM`~ my_data$Date, 
                     data = my_data,
                     par.settings = list(superpose.line = list(col=c("black", "red"))),
                     main="Sea Surface Temperature (SST) & Degree Heating Week (DHW)",
                     col=c("turquoise", "dodgerblue3", "green3", "gold2", "red"),
                     key=list(
                       corner=c(1,0.95),
                       text = list(c("Intertidal (AVG)", "Subtidal (AVG)", "NOAA values", "AVG MMM", "DHW")),
                       lines = list(lty = c(rep("solid", 3), rep("twodash", 1), rep("longdash", 1)),
                                    lwd = c(rep(2, 3), rep(3, 1)),
                                    col=c("turquoise", "dodgerblue3", "green3", "gold2", "red"))),
                     type = c("l","g"),
                     lty=c(rep("solid", 3), rep("twodash", 1)),
                     lwd=c(rep(2, 3), rep(3, 1)),
                     xlab="Date", 
                     ylab="Sea Surface Temperature (SST) (ºC)")

Temp_graph

# creating DHW graph
DHW_graph <- xyplot(my_data$DHW ~ my_data$Date, 
                    data = my_data,
                    superpose=T,
                    col="red",
                    lwd=2,
                    type = c("l"),
                    lty = 5,
                    xlab="Date", 
                    ylab="Degree Heating Week (DHW)")

DHW_graph

# creating combined graph
doubleYScale(Temp_graph, DHW_graph, add.ylab2 = TRUE)


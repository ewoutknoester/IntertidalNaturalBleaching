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

# combining relevant sheets of excel file
for (i in 2:36) {
  temp <- read_excel("ImageJ_R.xlsx", sheet = i)
  my_data <- rbind(my_data, temp)
  print(i)
}

# removing unnecessary columns, focussing on average only
my_data <- my_data[c(1,2,4,20,21)]

# turn dates into number of days from start (07/02/2020)
startdate <- as.Date("2020-02-07","%Y-%m-%d")
my_data$Date_days <- as.numeric(difftime(my_data$Date,startdate ,units="days"), units="days")

# change headers of columns AVG bleaching and AVG survival
my_data = my_data %>%   rename(AVG_Bleaching = `AVG Brightness`)
my_data = my_data %>%   rename(AVG_Survival = `AVG Survival`)

# removing rows containing NAs (so the rows that don't contain the average values for bleaching and survival)
my_data <- na.omit(my_data)

### WHAT HAS BEEN DONE WITH MISSING DATA?
# write.csv(my_data,"test.csv", row.names = FALSE)

# creating average values for brightness and survival per treatment and date
my_data <- as.tbl(my_data) %>% 
  bind_rows(my_data) %>% 
  group_by(Treatment, Date_days) %>% 
  summarise_all(c("mean"))

# bleaching plot
xyplot(my_data$AVG_Bleaching ~ my_data$Date, data = my_data, groups=my_data$Treatment, 
       auto.key = TRUE, 
       par.settings = list(superpose.symbol = list(col = c("blue","green", "red", "purple"), pch = 19),
                           superpose.line = list(col = c("blue","green", "red", "purple"), lwd = 2)),
       type = "l", xlab="Date", ylab="Bleaching (average brightness value (0-255))")

# survival plot
xyplot(my_data$AVG_Survival ~ my_data$Date, data = my_data, groups=my_data$Treatment, 
       auto.key = TRUE, 
       par.settings = list(superpose.symbol = list(col = c("blue","green", "red", "purple"), pch = 19),
                           superpose.line = list(col = c("blue","green", "red", "purple"), lwd = 2)),
       type = "l", xlab="Date", ylab="Survival (average % survival)")


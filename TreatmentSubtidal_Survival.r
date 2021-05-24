# loading libraries
library("readxl")
library("tidyverse")
library("lattice")
library("rstatix")
library("ggpubr")

# loading xls files
my_data <- read_excel("ImageJ_R.xlsx", sheet = 2)

# combining relevant sheets of excel file
for (i in 3:19) {
  temp <- read_excel("ImageJ_R.xlsx", sheet = i)
  my_data <- rbind(my_data, temp)
  print(i)
}

# removing unnecessary columns
my_data <- my_data[c(3,5,21,22)]

# removing rows containing NAs (so the rows that don't contain the average values for Survival/survival)
my_data <- na.omit(my_data)

# write.csv(my_data,"test.csv", row.names = FALSE)

## check repeated measures ANOVA requirements

# visualising data with box plots to see if there are outliers

boxplot(my_data$`AVG Survival` ~ my_data$Date,
        data=my_data, 
        main="Survival", 
        xlab="Date", 
        ylab="Survival (average Survival value)")
# outliers identified seem "normal"

# normality tests
ggqqplot(my_data, x = "AVG Survival")

hist(my_data$`AVG Survival`)

shapiro_test(my_data$`AVG Survival`)
# data is obviously not normal


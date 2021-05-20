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

# removing rows containing NAs (so the rows that don't contain the average values for brightness/survival)
my_data <- na.omit(my_data)

# write.csv(my_data,"test.csv", row.names = FALSE)

## check repeated measures ANOVA requirements

# visualising data with box plots to see if there are outliers

boxplot(my_data$`AVG Brightness` ~ my_data$Date,
        data=my_data, 
        main="Brightness", 
        xlab="Date", 
        ylab="Bleaching (average brightness value)")
# outliers identified seem "normal"

# normality tests
ggqqplot(my_data, x = "AVG Brightness")

hist(my_data$`AVG Brightness`)

shapiro_test(my_data$`AVG Brightness`)

# not normal -> sqrt transformation
my_data <- transform(my_data, `SQRT AVG Brightness`= sqrt(`AVG Brightness`))

ggqqplot(my_data, x = "SQRT.AVG.Brightness")

hist(my_data$SQRT.AVG.Brightness)

shapiro_test(my_data$SQRT.AVG.Brightness)

# still not normal -> log10 transformation
my_data <- transform(my_data, LOG10.AVG.Brightness= log10(AVG.Brightness))

ggqqplot(my_data, x = "LOG10.AVG.Brightness")

hist(my_data$LOG10.AVG.Brightness)

shapiro_test(my_data$LOG10.AVG.Brightness)

# still not normal -> 1/x transformation
my_data <- transform(my_data, INVERSE.AVG.Brightness= 1/AVG.Brightness)

ggqqplot(my_data, x = "INVERSE.AVG.Brightness")

hist(my_data$INVERSE.AVG.Brightness)

shapiro_test(my_data$INVERSE.AVG.Brightness)





## Exploratory graphs:

# creating average values for brightness and survival per origin and date
my_data <- as.tbl(my_data) %>% 
  bind_rows(my_data) %>% 
  group_by(Origin, Date) %>% 
  summarise_all(c("mean"))

# bleaching plot
xyplot(my_data$`AVG.Brightness` ~ my_data$Date, 
                        data = my_data, 
                        groups=my_data$Origin, 
                        auto.key = TRUE, 
                        par.settings = list(superpose.symbol = list(col = c("blue","green"),
                                                   pch = 19),
                           superpose.line = list(col = c("blue","green"),
                                                 lwd = 2)),
                        type = "l", 
                        xlab="Date", 
                        ylab="Bleaching (average brightness value)")

# survival plot
xyplot(my_data$`AVG.Survival` ~ my_data$Date, 
       data = my_data, 
       groups=my_data$Origin, 
       auto.key = TRUE, 
       par.settings = list(superpose.symbol = list(col = c("blue","green"),
                                                   pch = 19),
                           superpose.line = list(col = c("blue","green"),
                                                 lwd = 2)),
       type = "l", 
       xlab="Date", 
       ylab="Survival (average % survival)")


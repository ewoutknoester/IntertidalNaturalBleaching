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

# removing 07/02 and 11/05 because of inaccurate measurements
my_data<-my_data[!(my_data$Date_days=="0" | my_data$Date_days=="94"),]

# change headers of column bleaching
my_data = my_data %>% 
  rename(
    Bleaching = `Adjusted brightness (0 - 255)`
  )

# removing rows containing NAs (so the rows that don't contain the average values for bleaching)
my_data <- na.omit(my_data)

# creating an average value per structure
my_data <- as.tbl(my_data) %>% 
  bind_rows(my_data) %>% 
  group_by(Structure, Treatment) %>% 
  summarise_all(c("mean"))

# removing columns Structure number, Date and Date_days
my_data <- my_data[c(2,4)]

# Make treatment a factor
my_data$Treatment <- as.factor(my_data$Treatment)
class(my_data$Treatment)

# visualizing data with boxplot
boxplot(my_data$Bleaching ~ my_data$Treatment,
        data=my_data, 
        main="Bleaching", 
        xlab="Treatment", 
        ylab="Bleaching")

# normality tests
ggqqplot(my_data, x = "Bleaching")
hist(my_data$Bleaching)
shapiro_test(my_data$Bleaching)
# if >0.05 then it is normally distributed (so our data is NOT)

# not normal -> sqrt transformation helps a tiny bit
#my_data <- transform(my_data, `SQRT Bleaching`= sqrt(Bleaching))
#ggqqplot(my_data, x = "SQRT.Bleaching")
#hist(my_data$SQRT.Bleaching)
#shapiro_test(my_data$SQRT.Bleaching)

# not normal -> log10 transformation helps a little more but not enough
#my_data <- transform(my_data, LOG10.Bleaching= log(Bleaching))
#ggqqplot(my_data, x = "LOG10.Bleaching")
#hist(my_data$LOG10.Bleaching)
#shapiro_test(my_data$LOG10.Bleaching)

# Testing for homogeneity of variances (not necessary, it's not normal anyway)
#leveneTest(my_data$Bleaching, my_data$Treatment)
# if >0.05 then the variances are homogeneous (so in our case they are NOT)

# Testing for sphericity (not necessary, it's not normal anyway)
# Mauchly's Test for Sphericity!

# Get summary statistics
group_by(my_data, Treatment) %>%
  summarise(
    count = n(),
    mean = mean(Bleaching, na.rm = TRUE),
    sd = sd(Bleaching, na.rm = TRUE),
    median = median(Bleaching, na.rm = TRUE),
    IQR = IQR(Bleaching, na.rm = TRUE)
  )

# Kruskal-Wallis Test
kruskal.test(Bleaching ~ Treatment, data = my_data)
# p-value is <0.05, meaning the medians of all groups are NOT equal
# -> There ARE significant differences between groups

# Post-hoc test: Wilcoxon rank sum test
pairwise.wilcox.test(my_data$Bleaching, my_data$Treatment,
                     p.adjust.method = "bonferroni")




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
my_data <- my_data[c(1,2,22)]

# change headers of column Survival
my_data = my_data %>% 
  rename(
    Survival = 'Diff Survival'
  )

# removing rows containing NAs (so the rows that don't contain the average values for Survival)
my_data <- na.omit(my_data)

# removing column Structure number
my_data <- my_data[c(2,3)]

# Make treatment and days factors
my_data$Treatment <- as.factor(my_data$Treatment)
class(my_data$Treatment)
my_data$Date_days <- as.factor(my_data$Date_days)
class(my_data$Date_days)

# visualizing data with boxplot
boxplot(my_data$Survival ~ my_data$Treatment,
        data=my_data, 
        main="Survival difference", 
        xlab="Treatment", 
        ylab="Survival difference (end % - begin %)")

# normality tests
ggqqplot(my_data, x = "Survival")
hist(my_data$Survival)
shapiro_test(my_data$Survival)
# if >0.05 then it is normally distributed (so our data is NOT)

# sqrt transformation not possible with both positive and negative values (?)
#my_data <- transform(my_data, `SQRT Survival`= sqrt(Survival))
#ggqqplot(my_data, x = "SQRT.Survival")
#hist(my_data$SQRT.Survival)
#shapiro_test(my_data$SQRT.Survival)

# Testing for homogeneity of variances (not necessary, it's not normal anyway)
#leveneTest(my_data$Survival, my_data$Treatment)
# if >0.05 then the variances are homogeneous (so in our case they are NOT)

# Testing for sphericity (not necessary, it's not normal anyway)
# Mauchly's Test for Sphericity!

# Make treatment a factor
my_data$Treatment <- as.factor(my_data$Treatment)
class(my_data$Treatment)

# Get summary statistics
group_by(my_data, Treatment) %>%
  summarise(
    count = n(),
    mean = mean(Survival, na.rm = TRUE),
    sd = sd(Survival, na.rm = TRUE),
    median = median(Survival, na.rm = TRUE),
    IQR = IQR(Survival, na.rm = TRUE)
  )

# Kruskal-Wallis Test
kruskal.test(Survival ~ Treatment, data = my_data)
# p-value is <0.05, meaning the medians of all groups are NOT equal
# -> There ARE significant differences between groups

# Post-hoc test: Wilcoxon rank sum test
pairwise.wilcox.test(my_data$Survival, my_data$Treatment,
                     p.adjust.method = "bonferroni")




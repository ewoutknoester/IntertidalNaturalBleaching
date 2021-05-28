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

# change headers of column bleaching
my_data = my_data %>% 
  rename(
    Bleaching = `Adjusted brightness (0 - 255)`
  )

# removing rows containing NAs (so the rows that don't contain the average values for bleaching)
my_data <- na.omit(my_data)

my_data <- as.tbl(my_data) %>% 
  bind_rows(my_data) %>% 
  group_by(Structure, Treatment, Date_days) %>% 
  summarise_all(c("mean"))

# remove data point that is likely wrong
my_data <- my_data[-c(27),] 

# write.csv(my_data,"test.csv", row.names = FALSE)


# scatter plot
#reg1 <- lm(write~read,data=hsb2) 
#summary(reg1)
#with(hsb2,plot(read, write))
#abline(reg1)


## check repeated measures ANOVA requirements

# visualising data with box plots

boxplot(my_data$`Bleaching` ~ my_data$Date,
        data=my_data, 
        main="Bleaching", 
        xlab="Date", 
        ylab="Bleaching")

# normality tests
ggqqplot(my_data, x = "Bleaching")
hist(my_data$Bleaching)
shapiro_test(my_data$Bleaching)

# not normal -> sqrt transformation
#my_data <- transform(my_data, `SQRT Bleaching`= sqrt(Bleaching))
#ggqqplot(my_data, x = "SQRT.Bleaching")
#hist(my_data$SQRT.Bleaching)
#shapiro_test(my_data$SQRT.Bleaching)

# not normal -> log10 transformation: Works best!
my_data <- transform(my_data, LOG10.Bleaching= log(Bleaching))
ggqqplot(my_data, x = "LOG10.Bleaching")
hist(my_data$LOG10.Bleaching)
shapiro_test(my_data$LOG10.Bleaching)

# not normal -> 1/x transformation
# my_data <- transform(my_data, INVERSE.Bleaching= 1/Bleaching)
# ggqqplot(my_data, x = "INVERSE.Bleaching")
# hist(my_data$INVERSE.Bleaching)
# shapiro_test(my_data$INVERSE.Bleaching)

# Testing for homogeneity of variances
leveneTest(my_data$Bleaching, my_data$Treatment)
fligner.test(my_data$Bleaching, my_data$Treatment)
# Variances are NOT homogeneous

# Testing for sphericity
# Mauchly's Test for Sphericity!


## Testing which distribution fits the data best

#test <- fitdist(my_data$Survival, "norm")
#plot(test)
#test$aic

#test <- fitdist(my_data$Survival, "lnorm")
#plot(test)
#test$aic

#test <- fitdist(my_data$Survival, "pois")
#plot(test)
#test$aic

#test <- fitdist(my_data$Survival, "exp")
#plot(test)
#test$aic

#test <- fitdist(my_data$Survival, "gamma")
#plot(test)
#test$aic

#test <- fitdist(my_data$Survival, "nbinom")
#plot(test)
#test$aic

#test <- fitdist(my_data$Survival, "geom")
#plot(test)
#test$aic

#test <- fitdist((my_data$Survival/100), "beta", method = "mme")
#plot(test)
#test$aic

#test <- fitdist(my_data$Survival, "unif")
#plot(test)
#test$aic

#test <- fitdist(my_data$Survival, "logis")
#plot(test)
#test$aic

# A lognormal distribution fits best, so this will be used in the model

# Generalized Linear Mixed Model with Repeated Measures

Model <- lm(LOG10.Bleaching~Treatment+Date_days, data=my_data)
print(summary(Model),correlation=FALSE)
plot(Model)
qqnorm(resid(Model))

cooksd <- cooks.distance(Model)
plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
abline(h = 1, col="red")  # add cutoff line
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>1, names(cooksd),""), col="red")  # add labels


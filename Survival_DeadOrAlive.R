library(rstudioapi)
library("readxl")
library("tidyverse")
library("lattice")
library("rstatix")
library("ggpubr")
library("lme4")
library("fitdistrplus")
library("lmerTest")
library("car")
library(nlme) # mixed models allowing for heterogeneity
library(multcomp) # add significance levels to plot
library(emmeans) # Pairwise comparisons
library(stringr) # remove spaces
library("grid")
library("dplyr")
library("FSA")

# MAKE SURE FILE IS SAVED BEFORE RUNNING THIS CODE:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set directory at current (make sure file is saved)
# If you get error: save all files, reopen the project and re-run the code

# load first xls sheet
survival.raw <- read_excel("ImageJ_R.xlsx", sheet = 1)

# combining next sheets of excel file (each structure had its own sheet)
for (i in 2:36) {
  temp <- read_excel("ImageJ_R.xlsx", sheet = i)
  survival.raw <- rbind(survival.raw, temp)
}

# check missing data: much more frags missing in the subtidal
survival.raw %>%
  group_by(Treatment) %>%
  summarize(Sum_Missing = sum(`Cause of death` == "Missing", na.rm=TRUE))

# SET SURVIVAL TO 0 WHEN MISSING:
survival.raw <- within(survival.raw, `Survival (%)`[`Cause of death` == 'Missing'] <- 0)

# removing unnecessary columns
my_data.S <- survival.raw[c(1,2,4,17)]

# turn dates into number of days from start (07/02/2020)
startdate <- as.Date("2020-02-07","%Y-%m-%d")
my_data.S$Date_days <- as.numeric(difftime(my_data.S$Date, startdate, units="days"), units="days")

# change headers of column Survival
my_data.S = my_data.S %>% rename(Survival = `Survival (%)`)

# Make treatment a factor
my_data.S$Treatment <- as.factor(my_data.S$Treatment)

# removing 07/02 and 11/05 because of inaccurate measurements
my_data.S1 <- my_data.S[!(my_data.S$Date_days == "0" | my_data.S$Date_days == "94"),]

# get insight into NAs
my_data.S1 %>%
  group_by(Treatment) %>%
  summarize(Sum_NA = sum(is.na(Survival)))

# removing rows containing NAs
my_data.S2 <- na.omit(my_data.S1)

# setting survival to either 0 (dead) or 1(alive)
my_data.S2$Survival <- as.integer(my_data.S2$Survival>0)

# sum number of alive fragments per structure per date
my_data.Savg <- as.tbl(my_data.S2) %>% 
  group_by(Structure, Treatment, Date, Date_days) %>% 
  summarise_all(c("mean"))

# omit all data that is not from 09/07 (Date_days = 153)
my_data.Savg2<-my_data.Savg[(my_data.Savg$Date_days==153),]

# normality tests
ggqqplot(my_data.Savg2, x = "Survival")
hist(my_data.Savg2$Survival)
shapiro_test(my_data.Savg2$Survival)

# not normal -> kruskal-wallis test
kruskal.test(Survival ~ Treatment, data = my_data.Savg2)

# Post-hoc test: Wilcoxon rank sum test
pairwise.wilcox.test(my_data.Savg2$Survival, my_data.Savg2$Treatment,
                     p.adjust.method = "bonferroni")

# Function to facilitate averaging a dataset
data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE),
      n  = length(x[[col]]),
      se = sd(x[[col]], na.rm=TRUE)/sqrt(length(x)))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

# get means and SEs
my_data_plot <- data_summary(my_data.Savg2, varname = "Survival", groupnames = c("Treatment"))

# set colours for the Treatments
colours = c("red", "cyan", "orange", "dodgerblue2")

# create bar graph with for each zone the average number of alive fragments (plus error bars) at 09/07 (Date_days = 153)
letterA1 <- grobTree(textGrob("a", x=0.136, y=0.93, hjust=0, gp=gpar(col="black", fontsize=14)))
letterA2 <- grobTree(textGrob("a", x=0.613, y=0.98, hjust=0, gp=gpar(col="black", fontsize=14)))
letterAB <- grobTree(textGrob("ab", x=0.844, y=0.84, hjust=0, gp=gpar(col="black", fontsize=14)))
letterB <- grobTree(textGrob("b", x=0.375, y=0.614, hjust=0, gp=gpar(col="black", fontsize=14)))

ggplot(my_data_plot, aes(Treatment, Survival))+
  geom_col(stat = "identity", position = position_dodge(),
           fill = colours)+ labs(y = "Fraction of fragments alive per nursery")+
  geom_errorbar(aes(ymin=Survival-(se), ymax=Survival+(se)), width=.2, position=position_dodge(.9))+
  theme_light()+
  annotation_custom(letterA1)+
  annotation_custom(letterA2)+
  annotation_custom(letterB)+
  annotation_custom(letterAB)+
  ggtitle("Survival at 09/07")+
  theme(plot.title = element_text(hjust = 0.5))

ggsave("Survival_DeadOrAlive.png", width = 23, height = 10, units = "cm")





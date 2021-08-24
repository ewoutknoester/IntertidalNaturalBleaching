
# Setup

rm(list=ls()) # Clear workspace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #Set directory at current (make sure file is saved)

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
library("lubridate") # Floor dates

# loading xls files

# load data NOAA (taken from: https://coralreefwatch.noaa.gov/product/vs/data/kenya.txt)
NOAA <- read.table(file = "kenya_2021-08-20.txt", header = TRUE, skip = 21, sep = "")
NOAA$YYYY <- as.Date(paste(NOAA$YYYY, NOAA$MM, NOAA$DD, sep = "-"), format = "%Y-%m-%d") # Merge 1 date column

# Organize NOAA data
NOAA.1 <- NOAA[c(1,4,5,9)]
names(NOAA.1) <- c("Date", "SST.min", "SST.max", "DHW")
NOAA.1 <- as.data.frame(append(NOAA.1, list(SST.avg = (NOAA.1$SST.min + NOAA.1$SST.max)/2), after = 3))

# Select subset with relevant dates
NOAA.2 <- NOAA.1[NOAA.1$Date >= "2020-01-01" & NOAA.1$Date <= "2020-09-12", ]

# Load data HOBO logger
HOBO <- read_excel("HOBO (light & temp)_2021-08.xlsx", sheet = 1)

# Organize HOBO data
HOBO$Experiment <- as.factor(HOBO$Experiment)
HOBO$Treatment <- as.factor(HOBO$Treatment)
HOBO.1 <- HOBO[c(2,3,6,7,8)]
names(HOBO.1) <- c("Experiment", "Treatment", "Date", "Temp", "Light")
HOBO.1 <- HOBO.1[HOBO.1$Experiment == "Intertidal",]
HOBO.1$Date <- floor_date(HOBO.1$Date, unit = "day")

# Average per day
HOBO.2 <- HOBO.1 %>%
  group_by(Treatment, Date) %>%
  dplyr::summarize(HOBO.avg = mean(Temp), HOBO.min = min(Temp), HOBO.max = max(Temp), Light = mean(Light), Experiment = Experiment[1])

HOBO.2 <- HOBO.2[-c(6,7)]

# Long to wide
HOBO.avg <- HOBO.2[-c(4,5)]
HOBO.avg <- spread(HOBO.avg, key = Treatment, value = HOBO.avg)
names(HOBO.avg) <- c("Date", "INT.avg", "SUB.avg")
HOBO.avg$Date <- as.Date(HOBO.avg$Date, format = "%Y-%m-%d")

# Join NOAA and HOBO datasets
Temp <- plyr::join(NOAA.2, HOBO.avg, by = "Date")

# New dataframes for Min and Max
HOBO.int <- HOBO.2[HOBO.2$Treatment == "Intertidal",]
HOBO.sub <- HOBO.2[HOBO.2$Treatment == "Subtidal",]

# Plot
# Select dates for plotting

Temp.plot <- Temp[Temp$Date >= "2020-02-01" & Temp$Date <= "2020-09-12", ]
Temp.plot$MMM <- 28.7719 + 1 # Set average monthly maximum mean

# creating line graph
Temp_graph <- xyplot(data = Temp.plot, SST.max + INT.avg + SUB.avg + MMM ~ Date, 
                     par.settings = list(superpose.line = list(col=c("black", "red"))),
                     col=c("green3", "turquoise", "dodgerblue3", "yellow", "red"),
                     key=list(corner = c(1, 0.95),
                     text = list(c("Intertidal (avg)", "Subtidal (avg)", "NOAA (max)", "MMM + 1", "DHW")),
                     lines = list(lty = c("solid", "solid", "solid", "twodash", "twodash"), lwd = 3,
                                  col=c("turquoise", "dodgerblue3", "green3", "yellow", "red"))),
                     type = c("l","g"), # Set type of graph: line graph
                     lty = c("solid", "solid", "solid", "twodash", "twodash"),
                     lwd = c(2, 2.5, 2.5),
                     xlab = "Date", 
                     ylab = expression("Temperature " ( degree~C))
                     )

Temp_graph

# creating DHW graph
DHW_graph <- xyplot(DHW ~ Date, 
                    data = Temp.plot,
                    superpose=T,
                    col="red",
                    lwd=2,
                    type = c("l"),
                    lty = 5,
                    xlab="Date", 
                    ylab="Degree Heating Week (DHW)")

DHW_graph

# creating combined graph
png("Temp and DHW.png", width = 23, height = 12, units = "cm", res = 200)
doubleYScale(Temp_graph, DHW_graph, add.ylab2 = TRUE)
dev.off()

# creating line graph intertidal
xyplot(HOBO.max + HOBO.avg + HOBO.min ~ Date, data = HOBO.int,
       col=c("red", "gold2", "dodgerblue"), lwd=c(2.5,3,2.5), key=list(corner=c(1,0.95),
       text = list(c("Maximum", "Average", "Minimum")),
      lines = list(lwd=c(4,5,4), col=c("red", "gold2", "dodgerblue"))),
       type = c("l","g"), 
       xlab="Date", ylab = expression("Temperature " ( degree~C)),
       ylim=c(23, 36))

# creating line graph subtidal
xyplot(HOBO.max + HOBO.avg + HOBO.min ~ Date, data = HOBO.sub,
       col=c("red", "gold2", "dodgerblue"), lwd=c(2.5,3,2.5), key=list(corner=c(1,0.95),
       text = list(c("Maximum", "Average", "Minimum")),
       lines = list(lwd=c(4,5,4), col=c("red", "gold2", "dodgerblue"))),
       type = c("l","g"), 
       xlab="Date", ylab = expression("Temperature " ( degree~C)),
       ylim=c(23, 36))

# creating line graph subtidal
trellis.par.set(strip.background = list(col = "#DADADA"))
trellis.par.set(axis.text = list(cex = 1, fontface = "bold", col="#000000"))
trellis.par.set(par.xlab.text = list(cex = 1, fontface = "plain", col="#000000"))

xyplot(HOBO.max + HOBO.avg + HOBO.min ~ Date |Treatment, data = HOBO.2,
              col=c("red", "gold2", "dodgerblue"), lwd=c(2.5,3,2.5), key=list(corner=c(1,0.95),
              text = list(c("Maximum", "Average", "Minimum")),
              lines = list(lwd=c(4,5,4), col=c("red", "gold2", "dodgerblue"))),
              type = c("l","g"), 
              xlab="Date", ylab = list(expression("Temperature " ( degree~C)), cex = 1, fontface = "bold"),
              ylim=c(23, 36), layout=c(1,2),  scales=list(y=list(relation="free")))


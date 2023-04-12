library(tidyverse)
library(ggplot2)
library(readxl)
library(pacman)
library(drc)

library(nlme)
library(aomisc)

projectT<-read_csv("Downloads/cpu-clean.csv")
view(projectT)

projectT = projectT[,-7]
projectT = projectT[,-9]
projectT = projectT[,-3]
projectT = projectT[,-4]
view(projectT)

#Clean N/A
any(is.na(projectT))                    #return true if exist N/A
sum(is.na(projectT))                    #Total N/A in dataset
colSums(is.na(projectT))                #Total N/A in each column
projectT<-na.omit(projectT)             #Delete all N/A     
any(is.na(projectT))                    #return true if exist N/A
view(projectT)

par(mfrow=c(1,2))
hist(projectT$`bfreq`)
proj.lm <- lm(formula = projectT$`bfreq` ~ projectT$`litho`,data = projectT)
plot(x =  projectT$`litho`,
     y =  projectT$`bfreq`,
     xlab = "litho",
     ylab = "bfreq",
     main = "Adding a regression line with abline()"
)
abline(proj.lm,
       col = "red", lwd = 2)


glm.lm <- lm(formula = projectT$`bfreq` ~ projectT$`litho` + projectT$`ncore`,data = projectT)
summary(glm.lm)
anova(proj.lm,glm.lm)



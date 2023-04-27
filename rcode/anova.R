pacman::p_load(rio,     # for dealing with basic import export
               ggplot2, # for dealing with plot formats
               zoo,     # for dealing with year quarter formats
               ggpubr,  # customize ggplot 
               rstatix,
               tidyverse,
               nortest,
               FSA,
               car)
library(readr)
# Import data
#setwd("/Users/admin/Desktop/_probability_PROJECT/btlprob/rcode")
data <- import("./cpu-clean.csv") # rio::import

#--------------tdp ~ litho -----------#
#--------------one way anova -----------#


# Import data
data <- import("./cpu-clean.csv") # rio::import

# Only data from 2004 as it is less noisy
#data <- data[data$ldate > 2004,]

# remove data with few count group ldate and remove NA
data <- data[data$litho !=28,] #too few
data <- data[data$litho !=180,] # too small bfreq
data <- data[data$litho != 250,] # too small bfreq
data <- data[!is.na(data$tdp), ]
data$litho <- as.factor(data$litho)

data <- data[complete.cases(data[, c("litho")]), ]


# table has enough observation no need to remove anything
table(data$litho)

# plot for the cleaned data
ggplot(data, aes(x = litho,y = tdp,color = litho))+
  geom_point()

ggplot(data, aes(x = litho,y = tdp,color = litho))+
  geom_boxplot()

table(data$litho,data$gncore)

# create anova model of type III as our data is unbalanced
model<- aov(tdp ~ litho ,data = data)
# test assumption
plot(model, 1)

#take out the residual to check for the normality
resid <- residuals(model)

#histogram of residual
hist(resid,breaks = 30)

# qqplot of residual
qqPlot(resid)

# sharpiro test for the residual
shapiro.test(resid)
#levent test for Equality of variances
leveneTest(tdp ~ as.factor(litho),data = data)

summary(model)

kruskal.test(tdp ~ litho, data = data)

#post hoc test
Tukey<-TukeyHSD(model)
Tukey
plot(Tukey,las = 2)
dunnTest(tdp ~ litho, data = data,method = "bonferroni")
#--------------tdp ~ market -----------#
#--------------one way anova -----------#

# Import data
data <- import("./cpu-clean.csv") # rio::import

# Only data from 2004 as it is less noisy
#data <- data[data$ldate > 2004,]
# remove data with few count group ldate and remove NA
data <- data[data$tdp <150,]
data <- data[!is.na(data$tdp), ]
hist(data$tdp)
# table has enough observation no need to remove anything

# plot for the cleaned data
ggplot(data, aes(x = market,y = tdp,color = market))+
  geom_point()

ggplot(data, aes(x = market,y = tdp,color = market))+
  geom_boxplot()

#levent test for Equality of variances

leveneTest(tdp ~ as.factor(market),data = data)

# create anova model 
model<- aov(tdp ~ market ,data = data)

plot(model, 1)

#take out the residual to check for the normality
resid <- residuals(model)

#histogram of residual
hist(resid,breaks = 30)

# qqplot of residual
qqPlot(resid)

# sharpiro test for the residual
shapiro.test(resid)

summary(model)

kruskal.test(tdp ~ market, data = data)
Tukey<-TukeyHSD(model)
plot(Tukey,las = 2)
dunnTest(tdp ~ market, data = data,method = "bonferroni")








pacman::p_load(rio,     # for dealing with basic import export
               ggplot2, # for dealing with plot formats
               zoo,     # for dealing with year quarter formats
               ggpubr,  # customize ggplot 
               tseries,
               rstatix,
               tidyverse,
               nortest,
               car)
library(readr)
# Import data
data <- import("./cpu-clean.csv") # rio::import

#----------test for litho----------#
# Import data
data <- import("./cpu-clean.csv") # rio::import

# Only data from 2004 as it is less noisy
data <- data[data$ldate > 1999,]

# remove data with few count group ldate and remove NA
data <- data[data$litho !=28,]
#data <- data[data$litho !=130,]
data$litho <- as.factor(data$litho)
data$gldate <- cut(data$ldate,
                   breaks = c(0,2004,2008,2012,2016,Inf))
#labels = c("low","medium","high"))
data <- data[complete.cases(data[, c("status")]), ]
data <- data[complete.cases(data[, c("litho")]), ]

# create the count table to check for factors with few observation
table(data$status,data$litho)
count_table<-table(data$status,data$litho)
count_table <- as.data.frame.matrix(count_table > 20)

# loop over the data and add all the row that has factor below the observation limit
out <- rbind(data[0,])
for (x in 1:nrow(data)) {
  # extract the "litho" and "gldate" values for the current row
  comp <- data[x, c("litho", "status")]
  # check if the corresponding value in "wow" is TRUE
  if (count_table[comp$status,comp$litho] == FALSE) {
    # TODO if condition is true remove the rows from the data
    out <- rbind(out,data[x,])
  }
}

# remove the row that has below observation limit
data <- anti_join(x = data, y = out)
#data <- data[!(data$litho == "90" & data$status == "End of Interactive Support"),]
data <- data[!(data$litho == "32" & data$status == "End of Interactive Support"),]
# plot for the cleaned data
ggplot(data, aes(x = status,y = bfreq,color = litho))+
  geom_point()

ggplot(data, aes(x = status,y = bfreq,color = litho))+
  geom_boxplot()

#levent test for Equality of variances
as.character()
leveneTest(bfreq ~ as.factor(litho) * status ,data = data)

# create anova model of type III as our data is unbalanced
model<- aov(bfreq ~ litho * status ,data = data)
#Anova(model, type = "III")
plot(model, 1)
#take out the residual to check for the normality
resid <- residuals(model)

#histogram of residual
ggplot(data,aes(x=resid))+
  geom_histogram()
# qqplot of residual
qqPlot(resid)

# sharpiro test for the residual
shapiro.test(resid)

summary(model)



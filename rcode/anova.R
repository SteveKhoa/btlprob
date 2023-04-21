pacman::p_load(rio,     # for dealing with basic import export
               ggplot2, # for dealing with plot formats
               zoo,     # for dealing with year quarter formats
               ggpubr,  # customize ggplot 
               rstatix,
               tidyverse,
               nortest,
               car)
library(readr)
# Import data
data <- import("./cpu-clean.csv") # rio::import

#----------test for litho and gldate ----------(not really good but u can check)#
# Import data
data <- import("./cpu-clean.csv") # rio::import


# Only data from 2004 as it is less noisy
data <- data[data$ldate > 2000,]

# remove data with few count group ldate and remove NA
data <- data[data$litho !=28,]
data <- data[data$litho !=130,]
data$litho <- as.factor(data$litho)
data$gldate <- cut(data$ldate,
                   breaks = c(2005,2007.5,2,2010,2012,5,2015.5,Inf))
                    #labels = c("low","medium","high"))
data <- data[complete.cases(data[, c("gldate")]), ]
data <- data[complete.cases(data[, c("litho")]), ]

# create the count table to check for factors with few observation
table(data$gldate,data$litho)
count_table<-table(data$gldate,data$litho)
count_table <- as.data.frame.matrix(count_table > 44)

# loop over the data and add all the row that has factor below the observation limit
out <- rbind(data[0,])
for (x in 1:nrow(data)) {
  # extract the "litho" and "gldate" values for the current row
  comp <- data[x, c("litho", "gldate")]
  # check if the corresponding value in "wow" is TRUE
  if (count_table[comp$gldate,comp$litho] == FALSE) {
    # TODO if condition is true remove the rows from the data
    out <- rbind(out,data[x,])
  }
}

# remove the row that has below observation limit
data <- anti_join(x = data, y = out)
#data <- data[!(data$litho == "90" & data$status == "End of Interactive Support"),]
data <- data[!(data$litho == "32" & data$gldate == "End of Interactive Support"),]
# plot for the cleaned data
ggplot(data, aes(x = ldate,y = bfreq,color = litho))+
  geom_point()

ggplot(data, aes(x = gldate,y = bfreq,color = litho))+
  geom_boxplot()

#levent test for Equality of variances
as.character()
leveneTest(bfreq ~ as.factor(litho) * gldate ,data = data)

# create anova model of type III as our data is unbalanced
model<- aov(bfreq ~ litho * gldate ,data = data)
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

#-----------litho and ncore(the only decent stuff)-------------#



# Import data
data <- import("./cpu-clean.csv") # rio::import

# Only data from 2004 as it is less noisy might not even need to use this as 
# as ldate has a lot of NA
# data <- data[data$ldate > 2004,]

# remove data with few count group ldate and remove NA
data <- data[data$litho !=28,]
#data <- data[data$litho !=180,]
data$litho <- as.factor(data$litho)
data$gncore <- cut(data$ncore,
                   breaks = c(0,1,2,4,Inf))
#labels = c("low","medium","high"))
data <- data[complete.cases(data[, c("gncore")]), ]
data <- data[complete.cases(data[, c("litho")]), ]

# create the count table to check for factors with few observation
table(data$gncore,data$litho)
count_table<-table(data$gncore,data$litho)
count_table <- as.data.frame.matrix(count_table > 30)

# loop over the data and add all the row that has factor below the observation limit
out <- rbind(data[0,])
for (x in 1:nrow(data)) {
  # extract the "litho" and "status" values for the current row
  comp <- data[x, c("litho", "gncore")]
  # check if the corresponding value in "wow" is TRUE
  if (count_table[comp$gncore,comp$litho] == FALSE) {
    #if condition is true remove the rows from the data
    out <- rbind(out,data[x,])
  }
}

# remove the row that has below observation limit
data <- anti_join(x = data, y = out)

# plot for the cleaned data
ggplot(data, aes(x = ldate,y = bfreq,color = litho))+
  geom_point()

ggplot(data, aes(x = gncore,y = bfreq,color = litho))+
  geom_boxplot()

table(data$litho,data$gncore)
#levent test for Equality of variances
leveneTest(bfreq ~ as.factor(litho) *  gncore ,data = data)

# create anova model of type III as our data is unbalanced
model<- aov(bfreq ~ as.factor(litho) *  gncore ,data = data)
#Anova(model, type = "III")
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

#--------------if kruskal only work for one way anova-----------#
#--------------then we will do only litho and call it-----------#



# Import data
data <- import("./cpu-clean.csv") # rio::import

# Only data from 2004 as it is less noisy
#data <- data[data$ldate > 2004,]

# remove data with few count group ldate and remove NA
data <- data[data$litho !=28,] #too few
data <- data[data$litho !=180,] # too small bfreq
data <- data[data$litho != 250,] # too small bfreq
data$litho <- as.factor(data$litho)

data <- data[complete.cases(data[, c("litho")]), ]


# create the count table to check for factors with few observation
table(data$litho)
count_table<-table(data$litho)
count_table <- as.data.frame.matrix(count_table > 30)

# loop over the data and add all the row that has factor below the observation limit
out <- rbind(data[0,])
for (x in 1:nrow(data)) {
  # extract the "litho" and "status" values for the current row
  comp <- data[x, c("litho")]
  # check if the corresponding value in "wow" is TRUE
  if (count_table[comp$litho] == FALSE) {
    #if condition is true remove the rows from the data
    out <- rbind(out,data[x,])
  }
}

# remove the row that has below observation limit
data <- anti_join(x = data, y = out)

# plot for the cleaned data
ggplot(data, aes(x = ldate,y = bfreq,color = litho))+
  geom_point()

ggplot(data, aes(x = litho,y = bfreq,color = litho))+
  geom_boxplot()

table(data$litho,data$gncore)
#levent test for Equality of variances
leveneTest(bfreq ~ as.factor(litho),data = data)

# create anova model of type III as our data is unbalanced
model<- aov(bfreq ~ as.factor(litho) ,data = data)

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



# out lier if you want
outlier <- data %>% 
  group_by(litho) %>%
  identify_outliers(bfreq)



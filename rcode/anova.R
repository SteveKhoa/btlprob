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
setwd("/Users/admin/Desktop/_probability_PROJECT/btlprob/rcode")
data <- import("./cpu-clean.csv") # rio::import



#--------------bfreq ~ litho -----------#
#--------------one way anova -----------#


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


# table has enough observation no need to remove anything
table(data$litho)

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

kruskal.test(bfreq ~ litho, data = data)

#--------------bfreq ~ ncore -----------#
#--------------one way anova -----------#


# Import data
data <- import("./cpu-clean.csv") # rio::import

# Only data from 2004 as it is less noisy
#data <- data[data$ldate > 2004,]

# remove data with few count group ldate and remove NA
data$ncore <- cut(data$ncore,
                  breaks = c(0,1,2,4,Inf),
                  labels = c("1","2","4","6"))

data$ncore <- as.factor(data$ncore)
data <- data[complete.cases(data[, c("ncore")]), ]


# table has enough observation no need to remove anything
table(data$ncore)

# plot for the cleaned data
ggplot(data, aes(x = ncore,y = bfreq,color = ncore))+
  geom_point()

ggplot(data, aes(x = ncore,y = bfreq,color = ncore))+
  geom_boxplot()

#levent test for Equality of variances
leveneTest(bfreq ~ as.factor(ncore),data = data)

# create anova model of type III as our data is unbalanced
model<- aov(bfreq ~ as.factor(ncore) ,data = data)

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

kruskal.test(bfreq ~ ncore, data = data)

#--------------bfreq ~ litho + ncore-----------#
#--------------two way anova-------------------#



# Import data
data <- import("./cpu-clean.csv") # rio::import

# Only data from 2004 as it is less noisy
#data <- data[data$ldate > 2004,]

# remove data with few count group ldate and remove NA
data <- data[data$litho !=28,] #too few
data <- data[data$litho !=180,] # too small bfreq
data <- data[data$litho != 250,] # too small bfreq
data$ncore <- cut(data$ncore,
                   breaks = c(0,1,2,4,Inf),
                   labels = c("1","2","4","6"))
data$litho <- as.factor(data$litho)
data$ncore <- as.factor(data$ncore)
data <- data[complete.cases(data[, c("litho","ncore")]), ]


# create the count table to check for factors with few observation
table(data$litho,data$ncore)
count_table<-table(data$litho,data$ncore)
count_table <- as.data.frame.matrix(count_table > 30)

# loop over the data and add all the row that has factor below the observation limit
out <- rbind(data[0,])
for (x in 1:nrow(data)) {
  # extract the "litho" and "status" values for the current row
  comp <- data[x, c("litho","ncore")]
  # check if the corresponding value in "wow" is TRUE
  if (count_table[comp$litho,comp$ncore] == FALSE) {
    #if condition is true remove the rows from the data
    out <- rbind(out,data[x,])
  }
}

# remove the row that has below observation limit
data <- anti_join(x = data, y = out)

# plot for the cleaned data
ggplot(data, aes(x = ncore,y = bfreq,color = litho))+
  geom_point() +
  facet_wrap(~litho)

ggplot(data, aes(x = ncore,y = bfreq,color = litho))+
  geom_boxplot()

table(data$litho,data$ncore)
#levent test for Equality of variances
leveneTest(bfreq ~ as.factor(litho) * ncore,data = data)

# create anova model of type III as our data is unbalanced
model<- aov(bfreq ~ as.factor(litho) * ncore,data = data)
Anova(model, type = "II")
df <- as.data.frame(data[,c("litho","ncore")])

# measures the linear relationship between two variables.
# high linear relationship -> can not do type III anova
# you can ask Luo for more info cause he got the cor tech

x <- sapply(df, is.factor)
#convert all factor columns to numeric
df[ , x] <- as.data.frame(apply(df[ , x], 2, as.numeric))
cor(df)
plot(model, 1)

# interaction plot
ggline(data, x = "litho", y = "bfreq", color = "ncore",
       add = c("mean_se"))
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
















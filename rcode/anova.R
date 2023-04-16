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
# set the dependent and the independenet variable

#------------test for ldate and market-----------#

data$gldate <- cut(data$ldate,
                    breaks = c(2005,2010,2014,Inf))
                    #labels = c("low","medium","high"))

data <- data[complete.cases(data[, c("market","gldate")]), ]

ggplot(data, aes(x = ldate, y = bfreq,color = market))+
  geom_point()+
  facet_wrap(~market+gldate)

ggplot(data,aes(x = bfreq))+
  geom_histogram(aes(fill = market)) +
  facet_wrap(~market)

ggplot(data,aes(x = market,y = bfreq)) +
  geom_boxplot()
leveneTest(bfreq ~ as.factor(market),data = data)
data <- data[data$ldate > 2005,]
# test for variance

# split the money data

# detect oulier
outlier <- data %>%
  group_by(market,gldate) %>% 
  identify_outliers(!!sym(x))

#filter out the extreme outlier

outlier <- outlier %>% filter(!is.extreme)

# remove the extreme outlier from the data
# data<- data %>%
#   anti_join(outlier , by = c(independent, x))

data_clean <- data[complete.cases(data[, c("status","gldate")]), ]
#build model
ggplot(data_clean, aes(x = gldate, y = bfreq,color = market))+
  geom_boxplot()
model <- aov(bfreq ~ market * gldate, data=data_clean)
table(data$gldate,data$market)
summary(model)
leveneTest(bfreq ~ market * gldate,data = data_clean)


#----------test for litho----------#
# Import data
data <- import("./cpu-clean.csv") # rio::import
#only data from 2005 allow
ggplot(data, aes(x = litho,y = bfreq))+
  geom_point()
data <- data[data$ldate > 2003,]
data <- data[data$litho !=28,]
#data <- data[data$litho !=90,]
data <- data[data$litho !=130,]
#data <- data[data$litho !=65,]
#data <- data[data$litho !=14,]
#data <- data[data$litho !=22,]
data$litho <- as.factor(data$litho)
data$gldate <- cut(data$ldate,
                   breaks = c(0,2009,2013,Inf))
#labels = c("low","medium","high"))
data <- data[complete.cases(data[, c("gldate")]), ]
data <- data[complete.cases(data[, c("litho")]), ]

table(data$gldate,data$litho)
ggplot(data, aes(x = ldate,y = bfreq,color = litho))+
  geom_point()

ggplot(data, aes(x = litho,y = bfreq,color = gldate))+
  geom_boxplot()

leveneTest(bfreq ~ as.factor(litho) * as.factor(gldate) ,data = data)
oneway.test(bfreq ~ as.factor(litho) ,data = data,var.equal = FALSE)



# #----------------test for normal-------------#
# 
# x = "bfreq"
# independent = "litho"
# 
# # see the table
# table(data$market)
# # build and summary the first anova model
# model <- aov(data[[x]]  ~ data[[independent]] , data = data)
# summary(model)
# 
# #take out the resid
# resid <- unname(residuals(model))
# 
# # find the outlier
# 
# outlier <- data %>%
#   group_by(!!sym(independent)) %>%
#   identify_outliers(!!sym(x))
# 
# #filter out the extreme outlier
# 
# outlier <- outlier %>% filter(!is.extreme)
# 
# # remove the extreme outlier from the data
# data<- data %>%
#   anti_join(outlier , by = c(independent, x))
# 
# # build the model again
# 
# model <- aov(data[[x]]  ~  data[[independent]], data = data)
# summary(model)
# resid <- unname(residuals(model))
# 
# #qq norm
# qqnorm(resid)
# qqline(resid)
# #shapiro
# shapiro.test(resid)
# hist(resid)
# 
# data %>%
#   group_by(!!sym(independent)) %>%
#   shapiro_test(!!sym(x))
# 
# 
# ggplot(data,aes(x = data[[x]] )) +
#   geom_histogram(bins = 30) +
#   facet_wrap(vars(!!sym(independent)), scales = "free")


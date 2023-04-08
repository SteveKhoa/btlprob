pacman::p_load(rio,     # for dealing with basic import export
               ggplot2, # for dealing with plot formats
               zoo,     # for dealing with year quarter formats
               ggpubr)  # customize ggplot 
library(readr)
# Import data
setwd("***** YOUR WORKING DIRECTORY *******") # Set working directory
data <- import("./cpu-before.csv") # rio::import

# Rename labels
names(data) <- c("market", "status", "ldate", "litho", 
                 "rprice", "ncore", "nthread", "bfreq", "tdp", 
                 "memband", "temp") # Rename columns, easier to deal with


# write.csv(data, file = "my_data.csv", row.names = FALSE)

# View 
# levels(factor(data[,"market"])) # See how many MARKET SEGMENTS are there.
# levels(factor(data[,"status"])) # See how many TYPES OF STATUS are there.

# --------------------- Global data preprocessing ---------------------------
#   Data preprocessing needed before all step


data[,"ldate"] <- (
  as.yearqtr(data[,"ldate"], format = "Q%q'%y")
)

data[,"litho"] <- as.numeric(
  gsub(" nm", "", data[,"litho"]) # CAST "%d nm" -> "%d"
)

# Preprocess prices - stage 1 : Just take the max value
data[,"rprice"] <- gsub("(^\\$(\\d)+.(\\d)+ - )", "", data[,"rprice"])
#preporcess prices - stage 2 : change all "N/A" to N/A
data$rprice <- ifelse(data$rprice == "N/A", NA, data$rprice)
#write.csv(data, "cpu shorten.csv")
# Preprocess prices - stage 3 : Cut out dollar sign '$ '
data$rprice <- as.numeric(gsub('\\$|,', '', data$rprice))
# Preprocess base frequency - stage 1: cut out 'GHz' and 'MHz'
data[,"bfreq"] <- as.numeric(
  gsub("( GHz)|( MHz)", "", data[,"bfreq"])
)

# Preprocess base frequency - stage 2: (> 10) is probably 'MHz', TRANSFORM value*0.001
data<- data[!is.na(data$bfreq), ] # Truncate NAs
data$bfreq[data$bfreq > 10] <- data$bfreq[data$bfreq > 10]*0.001

data[,"tdp"] <- as.numeric(
  gsub(" W", "", data[,"tdp"])
)

data[,"memband"] <- as.numeric(
  gsub(" GB/s", "", data[,"memband"])
)

# Preprocess temperature column
# any way i did my own shit 
data[,"temp"] <- (gsub("[^0-9]+", ",", data[,"temp"])) # change every word to , B1 or C1 or -20 will be come ,1, ,1,20, but highest temp is at least 60 so it fine
# Loop through each element in the "temp" column
# Loop through each element in the "temp" column
for (i in seq_along(data[["temp"]])) {# loop through each col and split the string, find the max val and then return it
  # Split the element by commas and convert to numeric
  temp_values <- strsplit(data[i, "temp"], ",")
  temp_values <- unlist(lapply(temp_values, as.numeric))
  
  # Find the maximum value and save to the "temp" column
  max_value <- max(temp_values, na.rm = TRUE)
  if (is.infinite(max_value)) {
    max_value <- NA
  }
  data[i, "temp"] <- max_value
}
# --------------------- Local data preprocessing ---------------------
#   Data preprocessing method at a specific step
# Only take Launch date > 2005 - Q1 (as well as NAs)
data <- data[data$ldate > as.yearqtr("Q1'05", format="Q%q'%y"), ]
data <- data[!is.na(data$ldate), ]
#huh
?ggplot
p1 <- ggplot(data,aes(y = ..density..,x=tdp)) +
  geom_histogram(color = "black", fill = "blue") +
  geom_density(color = "red")
ggplot(data,mapping = aes(x = bfreq,y = tdp)) +
  geom_violin(aes(fill = as.factor(litho)))
p2 <- ggplot(data,aes(x = bfreq,y = tdp,color = litho)) +
  geom_point() +
  geom_smooth()
p3 <- ggplot(data,aes(x = ncore,y = tdp,color = litho)) +
  geom_point() +
  geom_smooth()
p4 <- ggplot(data,aes(x = nthread,y = tdp,color = litho)) +
  geom_point() +
  geom_smooth()
p5 <- ggplot(data,aes(x = rprice,y = tdp,color = litho)) +
  geom_point() +
  geom_smooth()
p6 <- ggplot(data,aes(x = memband,y = tdp,color = litho)) +
  geom_point() +
  geom_smooth()
ggplot(data,aes(x = litho,y = tdp,color = litho)) +
  geom_point() +
  geom_smooth()
ggarrange(p1, p2, p3, p4, p5, p6, common.legend = TRUE,nrow = 2, ncol = 3,legend = "right")
# Create a sample data



pacman::p_load(rio,     # for dealing with basic import export
               ggplot2, # for dealing with plot formats
               zoo)     # for dealing with year quarter formats
# Import data
setwd("***** YOUR WORKING DIRECTORY *******") # Set working directory
data <- import("./cpu-short.csv") # rio::import

# Rename labels
names(data) <- c("market", "status", "ldate", "litho", 
                 "rprice", "ncore", "nthread", "bfreq", "tdp", 
                 "memband", "temp") # Rename columns, easier to deal with
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

# Preprocess prices - stage 2 : Cut out dollar sign '$ '
data[,"rprice"] <- as.numeric(
  gsub("(^\\$)", "", data[,"rprice"])
)

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


# --------------------- Local data preprocessing ---------------------
#   Data preprocessing method at a specific step
# Only take Launch date > 2005 - Q1 (as well as NAs)
data <- data[data$ldate > as.yearqtr("Q1'05", format="Q%q'%y"), ]
data <- data[!is.na(data$ldate), ]
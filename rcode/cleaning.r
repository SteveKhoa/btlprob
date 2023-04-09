##  DATA CLEANING
##
##  Input: cpu-short.csv
##  Output: cpu-clean.csv
##
##  Dataset flow
##      cpu-raw.csv ---importing.r---> cpu-short.csv ---cleaning.r---> cpu-clean.csv
##
##  Description:
##      The dataset produced by this code is cleaned and all unecessary strings
##      such as 'nm', 'GHz', 'MHz' are cut out. Only numeric values are left.
##
##  Caveats:
##      The NAs values are intentially NOT removed yet. It must be further 
##      processed for you own interest.
##
##  NK
##  ---

##  LOAD NECESSARY PACKAEGS
pacman::p_load(
  rio,     # for imports & exports
  ggplot2, # for plots
  zoo      # for year-quarter formats
  )
##  IMPORT THE DATA
setwd("/Users/admin/Desktop/_probability_PROJECT/btlprob/rcode")   # set working directory
data <- import("cpu-short.csv")        # rio::import

##  RENAME LABELS
names(data) <- c("market", "status", "ldate", "litho", 
                 "rprice", "ncore", "nthread", "bfreq", "tdp", 
                 "memband", "temp"
                ) # Rename columns into shorter names

##
##  CLEANING PROCESS
##
##  Since the original dataset contains non-numeric values, which is not suitable 
##  for analyzing. The cleaning process attempts to normalize the data, into usable
##  typeclasses.
##  ---

##  DATE
#     note     "Q%q'%y"        : Our raw date format.
#                                %q means quarter
#                                %y means year
#                                Just like C-format!
#
#     Transform raw date format into zoo's standard date format.
#     Zoo's standard date format is easier to deal with, such as
#     
data[,"ldate"] <- (
  as.yearqtr(data[,"ldate"], format = "Q%q'%y")
)

##  LITHO
#
#     Remove "nm" from all lithography values, so we used
#     gsub to substitute all patterns of " nm" with "".
#    
data[,"litho"] <- as.numeric(
  gsub(" nm", "", data[,"litho"])
)

##  RPRICE
#     note      /regex/       : PLEASE EXPLAIN THE REGEX VIETUNG
#
#     TO-DO
#     TO-DO
#     TO-DO
#
data[,"rprice"] <- gsub(
  "(^\\$(\\d)+.(\\d)+ - )", 
  "", 
  data[,"rprice"]
  )
#preporcess prices - stage 2 : change all "N/A" to N/A
data$rprice <- ifelse(data$rprice == "N/A", NA, data$rprice)
# Preprocess prices - stage 3 : Cut out dollar sign '$ '
data$rprice <- as.numeric(gsub('\\$|,', '', data$rprice))

##  BFREQ
#     note     /regex/       : Regex to match "GHz" or "MHz"
#
#     First, remove all occurences of "GHz" and "MHz".
#     Second, all values greater than 10 must be MHz, we multiply them by 0.001
#
data[,"bfreq"] <- as.numeric(
  gsub("( GHz)|( MHz)", "", data[,"bfreq"])
)
data<- data[!is.na(data$bfreq), ]   # Truncate NAs because subscripting with NA
                                    # is not allowed.
data$bfreq[data$bfreq > 10] <- data$bfreq[data$bfreq > 10]*0.001

##  TDP
#
#     Remove all occurences of "W"
#
data[,"tdp"] <- as.numeric(
  gsub(" W", "", data[,"tdp"])
)

##  MEMBAND
#
#     Remove all occurences of "GB/s"
#
data[,"memband"] <- as.numeric(
  gsub(" GB/s", "", data[,"memband"])
)

##  TEMP
#     note    /regex/         : Match all decimal numbers and their sign
#                               perl=TRUE to enable extended Regex feature
#     
#     First, we match ony the decimal numbers, and separate them by comma,
#     (all characters unrelated to a specific decimal number are forced to become
#     comma)
#     Second, we use 'strsplit' to separate the numbers, with the delimeter is ","
#     by doing that, we have a list, with each entry of that list contains multiple values
#     Third, for each entry 'for (i in seq_along(...))' of that list, we keep only the maximum 
#     numeric value among the elements of the same entry. Entries with no valid numerics are
#     treated as NAs.
data[,"temp"] <- (gsub("[^0-9.\\-]+", ",", data[,"temp"]))  # 
for (i in seq_along(data[["temp"]])) {                    # For each elements in the same entry
  temp_values <- strsplit(data[i, "temp"], ",")           # Split into a list of words
  temp_values <- unlist(lapply(temp_values, as.numeric))  # Transform them into equivalent numerics
  max_value <- max(temp_values, na.rm = TRUE)             # Find max
  if (is.infinite(max_value)) {                           # Is it an invalid numeric?
    max_value <- NA
  }
  data[i, "temp"] <- max_value                            # Store the maximum value
}

export(data, "cpu-clean.csv")
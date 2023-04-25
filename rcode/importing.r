pacman::p_load(rio,     # for dealing with basic import export
               ggplot2, # for dealing with plot formats
               zoo,     # for dealing with year quarter formats
               ggpubr)  # customize ggplot 
library(readr)
# Import data

data <- import("./cpu-raw.csv") # rio::import

#Data attribute selection
data <- data[, c("Vertical_Segment","Status","Launch_Date",
                 "Lithography","Recommended_Customer_Price",
                 "nb_of_Cores","nb_of_Threads","Processor_Base_Frequency",
                 "TDP","Max_Memory_Bandwidth","T")] 

# Rename labels
names(data) <- c("market", "status", "ldate", "litho", 
                 "rprice", "ncore", "nthread", "bfreq", "tdp", 
                 "memband", "temp") # Rename columns, easier to deal with



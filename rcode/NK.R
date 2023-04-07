#
#   ! NOT IMPORTANT TO LOOK AT !
#
#   Personal Note

q()
pacman::p_load(rio, ggplot2, zoo)

# ---------- Data import ----------
data <- import("cpu-short.csv") # rio::import

# ---------- Preprocessing options ----------
data <- head(data, n = 5000) # get first 5000 lines

data <- na.omit(data) # omit N/A rows (DEPRECATED)
row.names(data) <- seq(nrow(data)) # reassign index number

data[,"Lithography"] <- as.numeric(gsub(" nm", "", data[,"Lithography"])) # cut "nm" out of the sequence, and cast it to numeric type
data[,"Launch_Date"] <- (as.yearqtr(data[,"Launch_Date"], format = "Q%q'%y") - as.yearqtr("Q1'00", format = "Q%q'%y"))*4 # zoo::yearqtr Process Quarter-Year into the number of quarters away from Q1'00 (2000 Q1)

# ---------- Display the data  ----------
View(data)
plot(Lithography ~ Launch_Date, data=data) # The lithography of cpus is decreasing

# Temporary area


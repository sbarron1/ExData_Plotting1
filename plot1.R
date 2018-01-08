library(tidyverse)
library(lubridate)

#reading and cleaning

data_raw <- read_csv2("household_power_consumption//household_power_consumption.txt")

data <- data_raw %>% filter(grepl("^1/2/2007$|^2/2/2007$", Date))


data$Date <- dmy(data$Date)

data$datetime <- paste(data$Date, data$Time)

data$datetime <- strptime(data$datetime, format = "%Y-%m-%d %H:%M:%S")

#check for na's 
sapply(data, function(x) sum(is.na(x)))
#check for "?"
sapply(data[3:9], function(x) sum(x == "?", na.rm =  T))

data[3:9] <- sapply(data[3:9], function(x) ifelse(x == "?", NA, x))

data[3:9] <- sapply(data[3:9], as.numeric)

#plotting
hist(data$Global_active_power, col = "red",
main = "Global Active Power",  xlab = "Global Active Power (kilowatts)")

dev.copy(png, "plot1.png")

dev.off()

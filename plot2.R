library(tidyverse)
library(lubridate)
library(scales)
#reading and cleaning

data_raw <- read_csv2("household_power_consumption//household_power_consumption.txt")

data <- data_raw %>% filter(grepl("1/2/2007|2/2/2007", Date))

data$Date <- dmy(data$Date)

data$datetime <- paste(data$Date, data$Time)

data$datetime <- strptime(data$datetime, format = "%Y-%m-%d %H:%M:%S")

sapply(data, function(x) sum(is.na(x)))

sapply(data[3:9], function(x) sum(x == "?", na.rm =  T))

data[3:9] <- sapply(data[3:9], function(x) ifelse(x == "?", NA, x))

data[3:9] <- sapply(data[3:9], as.numeric)


#plotting
ggplot(data, aes(datetime, Global_active_power)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
  labs(x = "", y = "Global Active Power (kilowatts)") +
  theme_classic()

#save plot
ggsave("plot2.png")

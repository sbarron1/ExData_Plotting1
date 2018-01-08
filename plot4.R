library(tidyverse)
library(lubridate)
library(scales)
library(cowplot) # function (plot_grid) from the package used 
                # to arrange ggplot objects into a grid 

#reading, cleaning and transforms

data_raw <- read_csv2("household_power_consumption//household_power_consumption.txt")

data <- data_raw %>% filter(grepl("1/2/2007|2/2/2007", Date))

data$Date <- dmy(data$Date)

data$datetime <- paste(data$Date, data$Time)

data$datetime <- strptime(data$datetime, format = "%Y-%m-%d %H:%M:%S")

sapply(data, function(x) sum(is.na(x)))

sapply(data[3:9], function(x) sum(x == "?", na.rm =  T))

data[3:9] <- sapply(data[3:9], function(x) ifelse(x == "?", NA, x))

data[3:9] <- sapply(data[3:9], as.numeric)
data$datetime <- as.POSIXct(data$datetime)

data <- data %>% gather(Sub_metering_1, Sub_metering_2, Sub_metering_3,
                        key = "metering_type", value = "metering_reading")

data$metering_type <- as.factor(data$metering_type)

#plots

plot_a <- ggplot(data, aes(datetime, Global_active_power)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
  labs(x = "", y = "Global Active Power (kilowatts)") +
  theme_classic()

plot_a

plot_b <- ggplot(data, aes(datetime, Voltage)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
  theme_classic()

plot_b

plot_c <- ggplot(data, aes(datetime, metering_reading, colour = metering_type)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
  labs(x = "", y = "Energy sub metering", colour = "") +
  theme_classic() +
  theme(legend.position = c(1,1), legend.justification = c(1,1))
plot_c

plot_d <- ggplot(data, aes(datetime, Global_reactive_power)) +
  geom_line() +
  scale_x_datetime(date_breaks = "1 day", date_labels = "%a") +
  theme_classic()

plot_grid(plot_a, plot_b, plot_c, plot_d) -> plot4

save_plot("plot4.png", plot4,
          ncol = 2,
          nrow = 2, 
          base_aspect_ratio = 1.3)

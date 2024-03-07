library(ggplot2)
library(tidyr)
library(dplyr)
library(zoo)
library(animint2)

# Read data
data <- read.csv('./Programming_Language_Statistics.csv')

# Reshape data
data_long <- data %>%
  gather(key = "programming_language", value = "popularity", -Date)

# Extract year from Date column
data_long$Year <- as.numeric(format(as.yearmon(data_long$Date, "%B %Y"), "%Y"))

# Define ggplot object
scatter <- ggplot(data_long, aes(x = programming_language, y = popularity, color = Year)) +
  geom_point(showSelected = "Year") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Programming Language", y = "Popularity", title = "Popularity of Programming Languages Over Time")
scatter 

# Define animint object with specified duration
viz.duration <- animint(scatter, duration = list(Year=2000),out.dir = "./output")
viz.duration.time <- viz.duration
viz.duration.time$time <- list(variable="Year", ms=3000)
viz.duration.time
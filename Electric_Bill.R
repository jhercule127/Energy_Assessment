library(ggplot2)
library(scales)
library(lubridate)
data = read.csv(file = "C:/Users/alcir/Downloads/electric bill.csv")
data = data[-c(3, 4)]
colnames(data)[1] = c("Date")
colnames(data)[2] = c("Total kwh")
data$Mnbtwu = data$`Total kwh` * .00341
data$Date = as.Date(data$Date, format("%m/%d/%Y"))
data = data[!duplicated(data$Date),]

ggplot(data, aes(data$Date, data$Mnbtwu)) + geom_point(color ="blue",alpha=0.5) + scale_x_date(labels = year(data$Date)) +
  theme_minimal() + scale_x_date() + xlab("Date") + ylab("MMBTU") + ggtitle("Total MMBTU in the last 2 years") + theme(plot.title = element_text(hjust = 0.5))


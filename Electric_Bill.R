library(ggplot2)
library(scales)
library(lubridate)


# Electric bill data csv file - file = your path 
data = read.csv(file = "C:/Users/alcir/Downloads/electric bill.csv")
# Get rid of Nan coloumns
data = data[-c(3, 4)]
# Change coloumn names
colnames(data)[1] = c("Date")
colnames(data)[2] = c("Total kwh")
# Calculate MMBTU
data$Mnbtwu = data$`Total kwh` * .00341
data$Date = as.Date(data$Date, format("%m/%d/%Y"))
data = data[!duplicated(data$Date),]
# Plot graph
ggplot(data, aes(data$Date, data$Mnbtwu)) + geom_line(alpha=0.5)  +
  theme_grey() + scale_x_date() + xlab("Date") + ylab("MMBTU") + ggtitle("Total MMBTU in the last 2 years") + theme(plot.title = element_text(hjust = 0.5))


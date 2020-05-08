library(ggplot2)
library(scales)
library(lubridate)

# Setting working directory - set to your directory for data
setwd('C:/Users/alcir/OneDrive/Documents/R/Energy_Assessment/Data')

# Electric bill data csv file - file = your path 
data = read.csv(file = 'electric bill.csv')
# Get rid of Nan coloumns
data = data[-c(3, 4)]
# Change coloumn names
colnames(data)[1] = c("Date")
colnames(data)[2] = c("Total kwh")
# Calculate MMBTU
data$Mmbtu = data$`Total kwh` * .00341
data$Date = as.Date(data$Date, format("%m/%d/%Y"))
data = data[!duplicated(data$Date),]
# Plot graph
ggplot(data, aes(data$Date, data$Mmbtu)) + geom_line(alpha=0.5)  +
  theme_grey() + scale_x_date() + xlab("Date") + ylab("MMBTU") + ggtitle("Total MMBTU in the last 2 years") + theme(plot.title = element_text(hjust = 0.5))




# Chilled Water Analysis in 2018
mymonths <- c("Jan","Feb","Mar",
              "Apr","May","Jun",
              "Jul","Aug","Sep",
              "Oct","Nov","Dec")

CWater2 = read.csv(file = "Chilledwater2018.csv")
CWater2$Cost <- CWater2$mmBTUs.by.month * 0.06
summary(CWater2$Cost)
cat("Total costs of chilled water in 2018:",sum(CWater2$Cost))

g3= ggplot(CWater2,aes(CWater2$Month,CWater2$mmBTUs.by.month)) + geom_line(alpha=0.5) + scale_x_continuous(breaks=1:12, labels=mymonths)
g3 + xlab("Month") + ylab("MMBTU")  + ggtitle("MMBTUs in 2018") + theme_minimal()

g4 = ggplot(CWater2,aes(CWater2$Month,CWater2$Cost)) + geom_bar(stat="identity",fill="dodgerblue3") + scale_x_continuous(breaks=1:12, labels=mymonths)
g4 + xlab("Month") + ylab("Cost(s)") + theme_minimal() + scale_y_continuous(label=dollar_format()) + ggtitle("Costs of Chilled Water in 2018")

cat("Total costs of chilled water in 2018:",sum(CWater2$Cost))



# Chilled Water Analysis in 2019
CWater = read.csv(file = "Chilledwater2019.csv")

CWater$Cost <- CWater$tonnage.in.mmbtus * 0.062
summary(CWater$Cost)
g= ggplot(CWater,aes(CWater$month,CWater$tonnage.in.mmbtus)) + geom_line(alpha=0.5) + scale_x_continuous(breaks=1:12, labels=mymonths)
g + xlab("Month") + ylab("MMBTU") + ggtitle("MMBTUs in 2019")  + theme_minimal()

g2 = ggplot(CWater,aes(CWater$month,CWater$Cost)) + geom_bar(stat="identity",fill="springgreen4") + scale_x_continuous(breaks=1:12, labels=mymonths)
g2 + xlab("Month") + ylab("Cost(s)") + ggtitle("Costs of Chilled Water in 2019") + theme_minimal() + scale_y_continuous(label=dollar_format())

cat("Total costs of chilled water in 2019:",sum(CWater$Cost))


# Electricity Analysis 2018 and 2019
electricity = read.csv(file = "electricity by months 2018+2019.csv")
electricity = electricity[-c(1)]
electric_2018 = electricity[c(1:12),]
electric_2019 = electricity[c(13:24),]
electric_2018['Month'] = c(1:12)
electric_2019['Month'] = c(1:12)

# $0.06 per kWh 
electric_2018$Cost = electric_2018$NSC.Total.kwh * .06
electric_2019$Cost = electric_2019$NSC.Total.kwh * .062


g5= ggplot(electric_2018,aes(electric_2018$Month,electric_2018$MMBTU)) + geom_line(alpha=0.5) + scale_x_continuous(breaks=1:12, labels=mymonths)
g5 + xlab("Month") + ylab("MMBTU") + theme_grey() + ggtitle("Electricity MMBTUs in 2018")

g6= ggplot(electric_2019,aes(electric_2019$Month,electric_2019$MMBTU)) + geom_line(alpha=0.5) + scale_x_continuous(breaks=1:12, labels=mymonths)
g6 + xlab("Month") + ylab("MMBTU") + theme_grey() + ggtitle("Electricity MMBTUs in 2019")


electric_costs18 = ggplot(electric_2018,aes(electric_2018$Month,electric_2018$Cost)) + geom_bar(stat="identity",fill="dodgerblue3") + scale_x_continuous(breaks=1:12, labels=mymonths)
electric_costs19 = ggplot(electric_2019,aes(electric_2019$Month,electric_2019$Cost)) + geom_bar(stat="identity",fill="springgreen4") + scale_x_continuous(breaks=1:12, labels=mymonths)
electric_costs18 + xlab("Month") + ylab("Cost") + theme_minimal() + scale_y_continuous(label=dollar_format()) + ggtitle("Costs of Electricity in 2018")
electric_costs19 + xlab("Month") + ylab("Cost") + theme_minimal() + scale_y_continuous(label=dollar_format()) + ggtitle("Costs of Electricity in 2019")

# Summary of Costs of Electricity
summary(electric_2018$Cost)
cat("Total costs of electricity in 2018:",sum(electric_2018$Cost))

summary(electric_2019$Cost)
cat("Total costs of electricity in 2019:",sum(electric_2019$Cost))





library(dplyr)
library(lubridate)

setwd("//chnas06/MKT-Data/Retention/RET Analytics/User/Doug B/Continued Learning/John Hopkins Data Science Courses/Exploratory Data Analysis/Week 1/")
data = read.table("household_power_consumption.txt",sep = ";", header = T, na.strings = "?")
data$Date = as.Date(data$Date, format = "%d/%m/%Y")
data$Time = format(strptime(data$Time, format = "%H:%M:%S"), "%H:%M:%S")
data$Global_active_power = as.numeric(as.character(data$Global_active_power))

data = data[complete.cases(data),]

data_plot = data %>% 
  filter(Date == "2007-02-01" | Date == "2007-02-02")

dateTime = paste(data_plot$Date, data_plot$Time)
data_plot$dateTime = as.POSIXct(data_plot$dateTime)

data_plot = cbind(data_plot,dateTime)

png(file = "plot1.png",width = 480,height = 480)
hist(data_plot$Global_active_power, col = "red", main = "Global Active Power", xlab = "Global Active Power (kilowatts)")
dev.off()

png(file = "plot2.png",width = 480,height = 480)
plot(data_plot$Global_active_power~data_plot$dateTime, type="l", ylab="Global Active Power (kilowatts)", xlab="")
dev.off()

png(file = "plot3.png",width = 480,height = 480)
with(data_plot, {
  plot(Sub_metering_1~dateTime, type="l",
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
})
legend("topright", col=c("black", "red", "blue"), lwd=c(1,1,1), 
       c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
dev.off()


png(file = "plot4.png",width = 480,height = 480)
par(mfrow=c(2,2), mar=c(4,4,2,1), oma=c(0,0,2,0))
with(t, {
  plot(Global_active_power~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  plot(Voltage~dateTime, type="l", 
       ylab="Voltage (volt)", xlab="")
  plot(Sub_metering_1~dateTime, type="l", 
       ylab="Global Active Power (kilowatts)", xlab="")
  lines(Sub_metering_2~dateTime,col='Red')
  lines(Sub_metering_3~dateTime,col='Blue')
  legend("topright", col=c("black", "red", "blue"), lty=1, lwd=2, bty="n",
         legend=c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))
  plot(Global_reactive_power~dateTime, type="l", 
       ylab="Global Rective Power (kilowatts)",xlab="")
})
dev.off()
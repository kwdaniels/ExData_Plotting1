#Function to covert string numbers to numerics and change "?" to NA
cleanNumber <- function(x) {
      if (x == "?") {
            y <- NA
      } else {
            y <- as.numeric(x)
      }
      return (y);
}

#Read the entire file as is (no data conversions) to dataframe "data"
data <- read.delim("household_power_consumption.txt", header=TRUE, sep=";", as.is=TRUE)

#Copy just the Feb 1 & 2 2007 data into dataframe "data2"
data2 <- data[data$Date %in% c("1/2/2007", "2/2/2007"),]

#Use lapply to call function "cleanNumber" to convert all character numbers to numerics and change "?" to NA
data2 <- cbind(data2[1:2], lapply(data2[3:9], cleanNumber))

#Create new column DateTime that combines Date and Time fields into a single datetime field
#(Note: may be able to use "usetz = FALSE" to force to POSIXct instead of using as.POSIXct)
data2$DateTime <- with(data2[,1:2],as.POSIXct(strptime(paste(Date,Time), format="%d/%m/%Y %H:%M:%S")))

#Convert character Date to date field
data2$Date <- as.Date(data2$Date, "%d/%m/%Y")

library(datasets)

png(filename="plot4.png", width=480, height=480)

#Set up 2 x 2 matrix in which to place the 4 charts
par(mfrow=c(2,2))

#Chart #1
plot(Global_active_power ~ DateTime, data2, type = "l", ylab="Global Active Power", xlab="")

#Chart #2
plot(Voltage ~ DateTime, data2, type = "l", xlab="datetime")

#Chart #3
plot(Sub_metering_1 ~ DateTime, data2, type = "l", ylab="Energy sub metering", xlab="")
points(Sub_metering_2 ~ DateTime, data2, type = "l", col="Red")
points(Sub_metering_3 ~ DateTime, data2, type = "l", col="Blue")
legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), lty=c(1,1), bty="n", col=c("black", "red", "blue"), cex=0.75)

#Chart #4
plot(Global_reactive_power ~ DateTime, data2, type = "l", ylab="Global_rective_power", xlab="datetime")

#Turn off the device
dev.off()
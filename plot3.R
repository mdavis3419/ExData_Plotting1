#This program is designed to create a triple line graph of the Energy Sub Metering
#in kilowatts using the UC Irvine Machine Learning Repository
#"Individual household electric power consumption Data Set" for 2007-02-01 
#and 2007-02-02

library(data.table)

main <- function() {
    inputFile = ".//household_power_consumption.txt"
    outputFile = "plot3.png"
    data <- importData(inputFile)
    Sub_metering_1 <- as.numeric(data$Sub_metering_1[(data$Date == "1/2/2007" 
                                                              | data$Date == "2/2/2007")
                                                             & data$Sub_metering_1 
                                                             != "?"])
    Sub_metering_2 <- as.numeric(as.character(data$Sub_metering_2[(data$Date == "1/2/2007" 
                                                      | data$Date == "2/2/2007")
                                                     & data$Sub_metering_2
                                                     != "?"]))
    Sub_metering_3 <- as.numeric(data$Sub_metering_3[(data$Date == "1/2/2007" 
                                                      | data$Date == "2/2/2007")
                                                     & data$Sub_metering_3
                                                     != "?"])
    Dates <- data$Date[(data$Date == "1/2/2007" 
                        | data$Date == "2/2/2007")
                       & data$Global_active_power 
                       != "?"]
    Times <- data$Time[(data$Date == "1/2/2007" 
                        | data$Date == "2/2/2007")
                       & data$Global_active_power 
                       != "?"]
    DateTimes <- strptime(paste(Dates, " ",Times), "%d/%m/%Y %H:%M:%S")
    buildLineGraph(Sub_metering_1, Sub_metering_2, Sub_metering_3,
                                DateTimes, outputFile)
}

importData <- function(inputFile) {
    data <- data.table(read.table(inputFile, header = TRUE, sep = ";"))
    data
}

buildLineGraph <- function(Sub_metering_1, Sub_metering_2, Sub_metering_3,
                           DateTimes, outputFile) {
    png(outputFile, width = 480, height = 480, bg = "transparent")
    plot(y = Sub_metering_1, x = DateTimes, type = "l", 
         ylab = "Energy sub metering", xlab = "")
    lines(x = DateTimes, y = Sub_metering_2, col = "red")
    lines(x = DateTimes, y = Sub_metering_3, col = "blue")
    legend("topright", pch = "-", col = c("black", "red", "blue"), legend = c("Sub Metering 1",
                                                                   "Sub Metering 2",
                                                                   "Sub Metering 3"))
    dev.off()
}

main()
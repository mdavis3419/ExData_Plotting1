#This program is designed to create a line graph of the Global Active Power
#in kilowatts using the UC Irvine Machine Learning Repository
#"Individual household electric power consumption Data Set" for 2007-02-01 
#and 2007-02-02

library(data.table)

main <- function() {
    inputFile = ".//household_power_consumption.txt"
    outputFile = "plot4.png"
    data <- importData(inputFile)
    globalActivePower <- as.numeric(data$Global_active_power[(data$Date == "1/2/2007" 
                                                              | data$Date == "2/2/2007")
                                                             & data$Global_active_power != "?"
                                                             & data$Sub_metering_1 != "?" 
                                                             & data$Sub_metering_2 != "?" 
                                                             & data$Sub_metering_2 != "?"])
    Dates <- data$Date[(data$Date == "1/2/2007" 
                        | data$Date == "2/2/2007")
                       & data$Global_active_power != "?"
                       & data$Sub_metering_1 != "?" 
                       & data$Sub_metering_2 != "?" 
                       & data$Sub_metering_2 != "?"]
    Times <- data$Time[(data$Date == "1/2/2007" 
                        | data$Date == "2/2/2007")
                       & data$Global_active_power != "?"
                       & data$Sub_metering_1 != "?" 
                       & data$Sub_metering_2 != "?" 
                       & data$Sub_metering_2 != "?"]
    DateTimes <- strptime(paste(Dates, " ",Times), "%d/%m/%Y %H:%M:%S")
    Sub_metering_1 <- as.numeric(data$Sub_metering_1[(data$Date == "1/2/2007" 
                                                      | data$Date == "2/2/2007")
                                                     & data$Global_active_power != "?"
                                                     & data$Sub_metering_1 != "?" 
                                                     & data$Sub_metering_2 != "?" 
                                                     & data$Sub_metering_2 != "?"])
    Sub_metering_2 <- as.numeric(as.character(data$Sub_metering_2[(data$Date == "1/2/2007" 
                                                                   | data$Date == "2/2/2007")
                                                                  & data$Global_active_power != "?"
                                                                  & data$Sub_metering_1 != "?" 
                                                                  & data$Sub_metering_2 != "?" 
                                                                  & data$Sub_metering_2 != "?"]))
    Sub_metering_3 <- as.numeric(data$Sub_metering_3[(data$Date == "1/2/2007" 
                                                      | data$Date == "2/2/2007")
                                                     & data$Global_active_power != "?"
                                                     & data$Sub_metering_1 != "?" 
                                                     & data$Sub_metering_2 != "?" 
                                                     & data$Sub_metering_2 != "?"])
    Voltage <- as.numeric(data$Voltage[(data$Date == "1/2/2007" 
                                               | data$Date == "2/2/2007")
                                              & data$Global_active_power != "?"
                                              & data$Sub_metering_1 != "?" 
                                              & data$Sub_metering_2 != "?" 
                                              & data$Sub_metering_2 != "?"])
    Global_Reactive_Power <- as.numeric(data$Global_reactive_power[(data$Date == "1/2/2007" 
                                                                    | data$Date == "2/2/2007")
                                                                   & data$Global_active_power != "?"
                                                                   & data$Sub_metering_1 != "?" 
                                                                   & data$Sub_metering_2 != "?"
                                                                   & data$Sub_metering_2 != "?"])
    buildGraphs(globalActivePower, DateTimes, Sub_metering_1, Sub_metering_2,
                Sub_metering_3, Voltage, Global_Reactive_Power, outputFile)
}

importData <- function(inputFile) {
    data <- data.table(read.table(inputFile, header = TRUE, sep = ";"))
    data
}

buildGraphs <- function(globalActivePower, DateTimes, Sub_metering_1, Sub_metering_2,
                        Sub_metering_3, Voltage, Global_Reactive_Power, outputFile) {
    png(outputFile, width = 480, height = 480, bg = "transparent")
    par(mfrow = c(2,2), mar = c(4, 4, .5, .5))
    plot(y = globalActivePower, x = DateTimes, type = "l", 
         ylab = "Global Active Power", xlab = "")
    plot(y = Voltage, x = DateTimes, type = "l", 
         ylab = "Voltage", xlab = "datetime")
    plot(y = Sub_metering_1, x = DateTimes, type = "l", 
         ylab = "Energy sub metering", xlab = "")
    lines(x = DateTimes, y = Sub_metering_2, col = "red")
    lines(x = DateTimes, y = Sub_metering_3, col = "blue")
    legend("topright", pch = "-", col = c("black", "red", "blue"),
           legend = c("Sub Metering 1", "Sub Metering 2", "Sub Metering 3"),
           bty = "n")
    plot(y = Global_Reactive_Power, x = DateTimes, type = "l", 
         ylab = "Global Reactive Power", xlab = "datetime")
    dev.off()
}

main()
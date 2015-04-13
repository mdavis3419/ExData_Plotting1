#This program is designed to create a line graph of the Global Active Power
#in kilowatts using the UC Irvine Machine Learning Repository
#"Individual household electric power consumption Data Set" for 2007-02-01 
#and 2007-02-02

library(data.table)

main <- function() {
    inputFile = ".//household_power_consumption.txt"
    outputFile = "plot2.png"
    data <- importData(inputFile)
    globalActivePower <- as.numeric(data$Global_active_power[(data$Date == "1/2/2007" 
                                                              | data$Date == "2/2/2007")
                                                             & data$Global_active_power 
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
    buildLineGraph(globalActivePower, DateTimes, outputFile)
}

importData <- function(inputFile) {
    data <- data.table(read.table(inputFile, header = TRUE, sep = ";"))
    data
}

buildLineGraph <- function(globalActivePower, DateTimes, outputFile) {
    png(outputFile, width = 480, height = 480, bg = "transparent")
    plot(y = globalActivePower, x = DateTimes, type = "l", 
         ylab = "Global Active Power (kilowatts)", xlab = "")
    dev.off()
}

main()
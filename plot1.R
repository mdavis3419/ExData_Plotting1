#This program is designed to create a histogram of the Global Active Power
#in kilowatts using the UC Irvine Machine Learning Repository
#"Individual household electric power consumption Data Set" for 2007-02-01 
#and 2007-02-02

library(data.table)

main <- function() {
    inputFile = ".//household_power_consumption.txt"
    outputFile = "plot1.png"
    data <- importData(inputFile)
    globalActivePower <- as.numeric(data$Global_active_power[(data$Date == "1/2/2007" 
                                                 | data$Date == "2/2/2007")
                                                & data$Global_active_power 
                                                != "?"])
    buildPNG(globalActivePower, outputFile)
}

importData <- function(inputFile) {
    data <- data.table(read.table(inputFile, header = TRUE, sep = ";"))
    data
}

buildPNG <- function(data, outputFile) {
    png(outputFile, width = 480, height = 480, bg = "transparent")
    hist(data2, col = "red", main = "Global Active Power",
                 xlab = "Global Active Power (kilowatts)")
    dev.off()
}

main()
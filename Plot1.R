makegraphs1 <- function(datapath){
  data <- read.table(file = paste(datapath,"household_power_consumption.txt", sep = ""), header = TRUE, sep = ";", colClasses = "character")
  datasubset <- data[data$Date == "1/2/2007"|data$Date == "2/2/2007",]
  datasubset$DateTime <- strptime(paste(datasubset$Date,datasubset$Time), "%d/%m/%Y %H:%M:%S")
  datasubset <- datasubset[,3:ncol(datasubset)]
  colnum <- ncol(datasubset) - 1
  for (i in 1:colnum){
    datasubset[,i] <- as.numeric(datasubset[,i])}
  plot1 <- hist(datasubset$Global_active_power, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", ylab = "Frequency", col = "red")
  dev.copy(png,paste(datapath, "plot1.png"))
  dev.off()
}
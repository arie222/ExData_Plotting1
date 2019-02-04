makegraphs2 <- function(datapath){
  data <- read.table(file = paste(datapath,"household_power_consumption.txt", sep = ""), header = TRUE, sep = ";", colClasses = "character")
  datasubset <- data[data$Date == "1/2/2007"|data$Date == "2/2/2007",]
  datasubset$DateTime <- strptime(paste(datasubset$Date,datasubset$Time), "%d/%m/%Y %H:%M:%S")
  datasubset <- datasubset[,3:ncol(datasubset)]
  colnum <- ncol(datasubset) - 1
  for (i in 1:colnum){
    datasubset[,i] <- as.numeric(datasubset[,i])}
  plot2 <- plot(datasubset$DateTime, datasubset$Global_active_power, type = "n", xlab = "", ylab = "Global Active Power (kilowatts)")
  plot2 <- lines(datasubset$DateTime, datasubset$Global_active_power, type = "l")
  dev.copy(png,paste(datapath, "plot2.png"))
  dev.off()
}
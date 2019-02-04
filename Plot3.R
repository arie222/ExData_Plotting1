makegraphs3 <- function(datapath){
  data <- read.table(file = paste(datapath,"household_power_consumption.txt", sep = ""), header = TRUE, sep = ";", colClasses = "character")
  datasubset <- data[data$Date == "1/2/2007"|data$Date == "2/2/2007",]
  datasubset$DateTime <- strptime(paste(datasubset$Date,datasubset$Time), "%d/%m/%Y %H:%M:%S")
  datasubset <- datasubset[,3:ncol(datasubset)]
  colnum <- ncol(datasubset) - 1
  for (i in 1:colnum){
    datasubset[,i] <- as.numeric(datasubset[,i])}
  plot3 <- with(datasubset, {
    plot(datasubset$DateTime, datasubset[,5], type = "l", col = "black", xlab = "", ylab ="Energy sub metering")
    lines(datasubset$DateTime, datasubset[,6], type = "l", col = "red")
    lines(datasubset$DateTime, datasubset[,7], type = "l", col = "blue")
    legend("topright", lty = 1, col = c("black", "red", "blue"), legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")) 
    
  })
  dev.copy(png,paste(datapath, "plot3.png"))
  dev.off()
}
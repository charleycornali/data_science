plot3 <- function(file) {
    
    #   Checks to see if the file has been unziped and is in the working directory, 
    #   if it doesnt exist the file is downloaded and unzipped    
    if(!file.exists(file)){
        if(!file.exists("exdata-data-household_power_consumption.zip")) {
            temp <- tempfile()
            download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip",temp)
            file <- unzip(temp)
            unlink(temp)
        }
    }
    
    #   Read the dataset 
    power_data <- read.table(file, header = TRUE, 
                             sep = ";", stringsAsFactors = FALSE, 
                             na.strings="?")
    
    #   Format the dates in the Date column
    power_data$Date <- as.Date(power_data$Date, format = "%d/%m/%Y")
    #   Subset the data so the data is only that from the relevant dates 
    df <- power_data[(power_data$Date == "2007-02-01") | (power_data$Date == "2007-02-02"), ]
    #   Remove old dataset variable
    rm(power_data)
    
    #   Change the values in the folowing columns to numeric values
    df$Global_active_power <- as.numeric(as.character(df$Global_active_power))
    df$Global_reactive_power <- as.numeric(as.character(df$Global_reactive_power))
    df$Voltage <- as.numeric(as.character(df$Voltage))
    df$Sub_metering_1 <- as.numeric(as.character(df$Sub_metering_1))
    df$Sub_metering_2 <- as.numeric(as.character(df$Sub_metering_2))
    df$Sub_metering_3 <- as.numeric(as.character(df$Sub_metering_3))
    #   Adding a timestamp column to the data.frame
    df <- transform(df, timestamp = as.POSIXct(paste(Date, Time)))
    
    #   Setting the parameters for the plots
    par(mfrow = c(2, 2))
    
    #   Top left plot
    plot(df$timestamp, df$Global_active_power, 
         type = "l", xlab= "", ylab = "Global Active Power")
    #   Top right plot
    plot(df$timestamp, df$Voltage, type = "l", 
         xlab = "datetime", ylab = "Voltage")
    
    #   Bottom left plot
    plot(df$timestamp, df$Sub_metering_1, 
         type = "l", xlab = "", ylab = "Energy sub metering")
    lines(df$timestamp, df$Sub_metering_2, col = "red")
    lines(df$timestamp, df$Sub_metering_3, col = "blue")
    
    #   bty argument removes the box around the legend
    #   cex argument shrinks the legend text and adds spacing labels
    legend("topright", col = c("black","red","blue"), 
           c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"), 
            lty = c(1, 1), bty = "n", cex = 0.5) 
    
    #   Bottom right plot
    plot(df$timestamp, df$Global_reactive_power, 
         type = "l", xlab = "datetime", ylab = "Global_reactive_power")
    
    #   Copying plot2 to PNG file
    dev.copy(png, file = "plot4.png", width = 480, height = 480)
    dev.off()
    
    #   Print statement is executed to show that the file has been saved
    print(paste("plot4.png has been saved on file location: ", getwd(), sep = ""))
    
}
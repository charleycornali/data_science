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
    
    #   Adding a timestamp column to the data.frame
    df <- transform(df, timestamp = as.POSIXct(paste(Date, Time)))
    
    #   Changed the values in the following columns to numeric values
    df$Sub_metering_1 <- as.numeric(as.character(df$Sub_metering_1))
    df$Sub_metering_2 <- as.numeric(as.character(df$Sub_metering_2))
    df$Sub_metering_3 <- as.numeric(as.character(df$Sub_metering_3))
    
    #   Create intital plot
    plot(df$timestamp, df$Sub_metering_1, type = "l", 
         xlab = "", ylab = "Energy sub metering")
    
    lines(df$timestamp, df$Sub_metering_2, col = "red")
    lines(df$timestamp, df$Sub_metering_3, col = "blue")
    legend("topright", col = c("black", "red", "blue"), 
           c("Sub_metering_1","Sub_metering_2", "Sub_metering_3"),
           lty = c(1, 1), lwd = c(1, 1))
    
    #   Copying plot2 to PNG file
    dev.copy(png, file = "plot3.png", width = 480, height = 480)
    dev.off()
    
    #   Print statement is executed to show that the file has been saved
    print(paste("plot3.png has been saved on file location: ", getwd(), sep = ""))
    
}
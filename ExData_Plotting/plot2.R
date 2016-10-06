plot2 <- function(file) {

    #   Checks to see if the file has been unziped and is in the working directory, 
    #   if it doesnt exist the file is downloaded and unzipped    
    if(!file.exists(file)){
        if(!file.exists("exdata-data-household_power_consumption.zip")) {
            temp <- tempfile()
            download.file("http://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip", temp)
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
    
    #   Change the values in the Global_active_powe column to numeric values
    df$Global_active_power <- as.numeric(as.character(df$Global_active_power))
    #   Add the timestamp column to create this second plot
    df <- transform(df, timestamp=as.POSIXct(paste(Date, Time)))
    #   Making the second plot
    plot(df$timestamp, df$Global_active_power, type= "l", 
         xlab = "", ylab = "Global Active Power (kilowatts)")
    #   Copying plot2 to PNG file
    dev.copy(png, file = "plot2.png", width = 480, height = 480)
    dev.off()
    
    #   Print statement is executed to show that the file has been saved
    print(paste("plot1.png has been saved on file location: ", getwd(), sep = ""))
}


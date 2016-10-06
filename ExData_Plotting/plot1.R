plot1 <- function(file) {

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
    
    #   Create histogram (plot1)
    hist(df$Global_active_power, main = "Global Active Power", 
         xlab = "Global Active Power (kilowatts)", ylab = "Frequency", col = "Red")
    
    #   Copying plot1 to PNG file
    dev.copy(png, file = "plot1.png", height = 480, width = 480)
    dev.off()
    
    #   Print statement is executed to show that the file has been saved
    print(paste("plot1.png has been saved on file location: ", getwd(), sep = ""))

}
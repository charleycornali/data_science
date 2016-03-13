best <- function(state, outcome){

    #   Formats the outcome name to match the column name
    format_outcome <- function(x) {
        
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), 
              substring(s, 2), 
              sep="", 
              collapse=".")
    }
    
    #   Change values from character to numeric and sort df colums
    sort_data_and_format_values <- function(state, outcome){
        
        dummy_mat <- as.matrix(df)
        #   Set the rows with 'Not Available' values to NA
        dummy_mat[dummy_mat == "Not Available"] <- NA
        df <- as.data.frame(dummy_mat)
        
        #   Filter out hospitals that are not equivalent to arg state
        df <- df[df$State == state, ]
        
        #   Strip NA values from rows
        df <- na.omit(df[c("Hospital.Name", outcome)])
        
        #   Change values from character data type to numeric for the outcome column
        df[, 2] <- as.numeric(as.character(df[, 2])) 
        
        #   Order the values by outcome then Hospital.Name
        df <- df[order(df[outcome], df["Hospital.Name"]), ]
    } 
    
    #   Read outcome data file
    df <- read.csv("outcome-of-care-measures.csv", 
                   colClasses = "character")
    
    #   Format the outcome name
    outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", 
                     sapply(outcome, format_outcome), 
                     sep="")
        
    #   Check the validity of the state
    if(state %in% df$State) {
        
        #   Check validity of the outcome
        if(outcome %in% names(df)) {
            
            df <- sort_data_and_format_values(state, outcome)
            
            #   Returns the Hospital.Name with the lowest outcome mortality rate
            as.character(df$Hospital.Name[1])
        }
        
        else {
            
            stop("invalid outcome")
        }
    }
        
    else {
        
        stop("invalid state")
    }
}

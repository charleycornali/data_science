rankall <- function(outcome, num = "best") {   
    
    #   Formats the outcome name to match the column name
    format_outcome <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=".")
    }

    set_na_values <- function(df) {
        
        dummy_mat <- as.matrix(df)
        #   Set the rows with 'Not Available' values to NA
        dummy_mat[dummy_mat == "Not Available"] <- NA
        df <- as.data.frame(dummy_mat)
    }
    
    sort_data_and_format_values <- function(df, state, outcome) {

        #   Filter out hospitals that are not equivalent to arg state
        df <- df[df$State == state, ]
        
        #   Strip NA values from rows
        df <- na.omit(df[c("Hospital.Name", outcome)])
        
        #   Change values from character data type to numeric for the outcome column
        df[, 2] <- as.numeric(as.character(df[, 2]))
        
        #   Order the values by outcome then Hospital.Name
        df <- df[order(df[outcome], df["Hospital.Name"]), ]
    } 
    
    get_ranked_hospital <-function(num){
        
        #   Gets the numeric number of the num arg
        if(num == "best") {
            
            num <- 1
        }
        
        else if(num == "worst"){
            
            num <- length(df[, 2])
        }
        
        #   Returns the Hospital.Name with the lowest 30-day mortality rate 
        as.character(df$Hospital.Name[num])
    }

    #    Read outcome data and change the 'Not Available' values to NA
    data_frame <- set_na_values(read.csv("outcome-of-care-measures.csv", colClasses = "character"))
    
    #   Format the outcome name
    outcome <- paste("Hospital.30.Day.Death..Mortality..Rates.from.", 
                     sapply(outcome, format_outcome), 
                     sep="")
    
    #   Character vector of the unique states in the data frame
    states_list <- sort(unique(as.character(data_frame$State)))
        
    #   Empty character vector, waiting to be populated with hospital data
    hospital <- character()
            
    #   For each state, find the hospital of the given rank for each state
    for(state in states_list) {
       
         #   Check the validity of the state
        if(state %in% data_frame$State){
            
            #   Check validity of the outcome
            if(outcome %in% names(data_frame)) {
                
                #   Sort and format data
                df <- sort_data_and_format_values(data_frame, state, outcome)
                
                #   adds the Hospital.Name with the lowest 30-day mortality rate 
                #   to the hospital character vector
                hospital <- append(hospital, get_ranked_hospital(num))
            }
        
            else {
                    
                stop("invalid outcome")
            }
        }
        else {
                
            stop("invalid state")
        }
    }
    
    #   Creates a new data frame with the desired results
    result_df <- data.frame(hospital = hospital, state = states_list) 
    #   Sets the row names to the state abbreviation
    rownames(result_df) <- result_df$State
    #   Return a data frame with the hospital names and the (abbreviated) state name
    result_df
}
